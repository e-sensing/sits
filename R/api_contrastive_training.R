#' @title Split samples into training and validation triplets for contrastive
#'   learning
#'
#' @description
#' Builds triplets \eqn{(anchor, positive, negative)} from labeled time-series
#' samples and splits them into training and validation sets for
#' contrastive / metric-learning objectives (e.g., triplet loss).
#'
#' Triplets can be generated either by:
#' \itemize{
#'   \item \code{"random"}: random triplets from available classes (optionally
#'   skipping singleton classes).
#'   \item \code{"semi-hard"}: mini-batch based selection where negatives are
#'   chosen using distances in an embedding space (semi-hard negative mining).
#' }
#'
#' @param samples Time series with the training samples.
#' @param sampling_method Triplet sampling strategy. One of \code{"random"} or
#'   \code{"semi-hard"}.
#' @param validation_split Proportion of triplets assigned to the validation
#'   set. Must be between \code{0} and \code{1}. The split is performed after
#'   triplet generation.
#' @param skip_singletons Logical. If \code{TRUE} and
#'   \code{sampling_method == "random"}, classes with fewer than 2 samples
#'   are ignored for anchor/positive selection.
#' @param classes_per_batch Integer or \code{NULL}. When
#'   \code{sampling_method == "semi-hard"}, number of distinct classes to
#'   include in each mini-batch used for mining.
#' @param samples_per_class Integer or \code{NULL}. When
#'   \code{sampling_method == "semi-hard"}, number of instances per class to
#'   include in each mini-batch used for mining.
#' @param num_triplets Integer. Total number of triplets to generate.
#' @param target_batch_size Integer. Target mini-batch size used by the
#'   semi-hard sampler. The effective batch size depends on
#'   \code{classes_per_batch} and \code{samples_per_class}.
#' @param embed_fn A function that takes a sample time-series object and
#'   returns a fixed-length numeric feature vector. Used only for
#'   \code{"semi-hard"} sampling.
#' @param dist_fn A function \code{dist_fn(a, b)} returning the distance
#'   between two embedding vectors \code{a} and \code{b}. Used only for
#'   \code{"semi-hard"} sampling.
#' @param seed Optional integer. If provided and
#'   \code{sampling_method == "random"}, it is forwarded to the random
#'   sampler to make triplet generation reproducible. The train/validation
#'   split uses base \code{sample()}.
#'
#' @return A named list with two elements:
#' \itemize{
#'   \item \code{train}: Triplets assigned to the training set.
#'   \item \code{validation}: Triplets assigned to the validation set.
#' }
#' The returned objects have the same structure as the triplets produced by
#' the underlying sampler.
#'
#' @details
#' The split is performed by sampling triplet row indices without replacement,
#' with \code{floor(n_triplets * validation_split)} triplets going to
#' validation.
#'
#' @references
#' Schroff, F., Kalenichenko, D., & Philbin, J. (2015).
#' _FaceNet: A Unified Embedding for Face Recognition and Clustering_.
#' In _Proceedings of the IEEE Conference on Computer Vision and Pattern_
#' _Recognition_ (CVPR), 815–823.
#' doi:10.1109/CVPR.2015.7298682
#'
#' @keywords internal
#' @noRd
.contrastive_training_data_split <- function(samples,
                                             sampling_method = "random",
                                             validation_split = 0.2,
                                             skip_singletons = TRUE,
                                             classes_per_batch = NULL,
                                             samples_per_class = NULL,
                                             num_triplets = 100L,
                                             target_batch_size = 64L,
                                             # TODO: make this function internal
                                             embed_fn = function(ts) { # a function to extract a fixed-length feature vector from each time_series
                                                 # grab tibble's numeric columns
                                                 mat <- as.matrix(ts[, sapply(ts, is.numeric)])
                                                 # flatten row‐wise
                                                 as.vector(t(mat))
                                             },
                                             dist_fn = function(a, b) sqrt(sum((a - b)^2)),
                                             seed = NULL) {
    if (sampling_method == "semi-hard") {
        triplets <- .contrastive_training_semi_hard_triplets(
            samples = samples,
            classes_per_batch = classes_per_batch,
            samples_per_class = samples_per_class,
            num_triplets = num_triplets,
            target_batch_size = target_batch_size,
            embed_fn = embed_fn,
            dist_fn = dist_fn
        )
    } else {
        triplets <- .contrastive_training_random_triplets(
            samples = samples,
            num_triplets = num_triplets,
            skip_singletons = skip_singletons,
            seed = seed
        )
    }

    # Train/test split
    all_idx <- seq_len(nrow(triplets))
    n_val <- floor(length(all_idx) * validation_split)
    val_idx <- sample(all_idx, size = n_val)
    train_idx <- setdiff(all_idx, val_idx)

    train_data <- triplets[train_idx, ]
    val_data <- triplets[val_idx, ]
}
#' @title Semi-hard triplet sampling method
#'
#' @description
#' Generates \eqn{(anchor, positive, negative)} triplets using a mini-batch
#' strategy inspired by FaceNet-style sampling. Each iteration samples
#' \eqn{P} classes and \eqn{K} instances per class (\eqn{P \times K} batch),
#' embeds each instance with \code{embed_fn}, computes a full pairwise
#' distance matrix using \code{dist_fn}, and then selects triplets per anchor
#' by mining:
#' \itemize{
#'   \item the closest negative (hard negative) within the batch; and
#'   \item a positive chosen as the hardest positive that is still closer than
#'   the selected negative (semi-hard positive). If no such positive exists,
#'   falls back to the easiest positive available.
#' }
#'
#' This function returns a tibble of triplets suitable for
#' contrastive / metric-learning objectives such as triplet loss.
#'
#' @param samples Time series with the training samples.
#' @param classes_per_batch Integer. Number of distinct classes \eqn{P} to
#'   sample per mini-batch. If \code{NULL}, defaults to
#'   \code{min(n_classes, 8)}.
#' @param samples_per_class Integer. Number of instances \eqn{K} to sample
#'   per class within a mini-batch. If \code{NULL}, defaults to
#'   \code{max(2, floor(target_batch_size / classes_per_batch))}.
#' @param num_triplets Integer. Total number of triplets to generate.
#' @param target_batch_size Integer. Target batch size used to derive
#'   defaults when \code{classes_per_batch} or \code{samples_per_class} are
#'   \code{NULL}. The effective batch size is
#'   \code{classes_per_batch * samples_per_class}.
#' @param embed_fn A function that takes one time-series object and returns a
#'   fixed-length numeric vector (embedding). The returned vectors must all
#'   have identical length.
#' @param dist_fn A function \code{dist_fn(a, b)} returning a non-negative
#'   scalar distance between two embedding vectors \code{a} and \code{b}.
#'
#' @return A \code{tibble} with \code{num_triplets} rows and the columns:
#' \itemize{
#'   \item \code{anchor}, \code{positive}, \code{negative}: list-columns
#'   containing the corresponding time-series objects.
#'   \item \code{anchor_idx}, \code{positive_idx}, \code{negative_idx}:
#'   integer indices pointing to the rows of \code{samples} used to build
#'   each triplet.
#' }
#'
#' @details
#' Triplets are mined only within the sampled mini-batch. Sampling is repeated
#' until \code{num_triplets} triplets are collected. If a chosen class has
#' fewer than \code{samples_per_class} instances, sampling for that class is
#' done with replacement to fill the batch.
#'
#' The negative is selected as the closest (hardest) negative in the batch
#' for a given anchor. The positive selection follows a semi-hard heuristic
#' relative to that negative: choose the farthest positive that is still
#' strictly closer than the selected negative. If none exists, choose the
#' closest positive.
#'
#' @author Alexandre Assuncao \email{alexcarssuncao@@gmail.com}
#' @keywords internal
#' @noRd
.contrastive_training_semi_hard_triplets <- function(samples,
                                                     classes_per_batch,
                                                     samples_per_class,
                                                     num_triplets,
                                                     target_batch_size,
                                                     embed_fn,
                                                     dist_fn) {
    all_labels <- samples$label
    classes <- unique(all_labels)
    n_classes <- length(classes)
    triplets <- vector("list", length = 0)

    # 1) pick defaults if user didn't supply them
    if (is.null(classes_per_batch)) {
        classes_per_batch <- min(n_classes, 8)
    }
    if (is.null(samples_per_class)) {
        # scale target_batch_size using the number of classes
        # with at least two per class
        samples_per_class <-
            max(2, floor(target_batch_size / classes_per_batch))
    }

    while (length(triplets) < num_triplets) {
        # 1) Sample P classes and then K examples per class
        chosen_classes <- sample(classes, classes_per_batch)
        batch_idx <- unlist(
            purrr::map(chosen_classes, function(cls) {
                # find all dataset indices having label == cls
                idxs <- which(all_labels == cls)
                if (length(idxs) >= samples_per_class) {
                    # if there are at least K examples, pick K *distinct* ones
                    sample(idxs, samples_per_class)
                } else {
                    # if not enough, sample with replacement
                    sample(idxs, samples_per_class, replace = TRUE)
                }
            })
        )

        # 2) Build feature matrix and labels for batch
        feats <- purrr::map(batch_idx, ~ embed_fn(samples$time_series[[.x]]))
        feats <- do.call(rbind, feats)
        labs <- all_labels[batch_idx]

        # 3) Pairwise distance matrix
        n <- nrow(feats)
        D <- matrix(0, n, n)

        for (i in seq_len(n)) {
            # compute distances from feats[i, ] to every row in feats
            D[i, ] <- vapply(
                seq_len(n),
                function(j) dist_fn(feats[i, ], feats[j, ]),
                numeric(1)
            )
        }

        order_j <- sample(seq_along(batch_idx)) # <-- shuffle anchor order

        # 4) For *each* anchor in this batch, pick hardest pos/neg
        for (j in order_j) {
            same_cls <- which(labs == labs[j] & seq_along(labs) != j)
            other_cls <- which(labs != labs[j])
            if (length(same_cls) == 0 || length(other_cls) == 0) next

            # Select hardest negative
            neg_j <- other_cls[which.min(D[j, other_cls])]
            d_neg <- D[j, neg_j]

            # distances to all same‐class examples
            d_pos_all <- D[j, same_cls]
            # keep only those strictly closer than the negative
            semi_idx <- same_cls[d_pos_all < d_neg]
            if (length(semi_idx) > 0) {
                # among those, pick the *hardest* semi‐hard positive
                pos_j <- semi_idx[which.max(D[j, semi_idx])]
            } else {
                # fallback: no semi‐hards, pick the easiest positive
                pos_j <- same_cls[which.min(d_pos_all)]
            }

            triplets[[length(triplets) + 1]] <- tibble::tibble(
                anchor       = list(samples$time_series[[batch_idx[j]]]),
                positive     = list(samples$time_series[[batch_idx[pos_j]]]),
                negative     = list(samples$time_series[[batch_idx[neg_j]]]),
                anchor_idx   = batch_idx[j],
                positive_idx = batch_idx[pos_j],
                negative_idx = batch_idx[neg_j]
            )
            if (length(triplets) >= num_triplets) break
        }
    }
    dplyr::bind_rows(triplets[1:num_triplets])
}
#' @title Random triplet sampling method
#'
#' @description
#' Generates \eqn{(anchor, positive, negative)} triplets by random sampling
#' from a labeled dataset. For each triplet:
#' \itemize{
#'   \item the anchor is sampled from an anchor pool (optionally excluding
#'   singleton classes);
#'   \item the positive is sampled from the same class as the anchor
#'   (excluding the anchor itself when possible); and
#'   \item the negative is sampled from a different class than the anchor.
#' }
#'
#' The function attempts to create \code{num_triplets} triplets, stopping
#' early if the dataset is degenerate (e.g., insufficient class structure) or
#' if repeated sampling fails after a bounded number of tries.
#'
#' @param samples Time series with the training samples.
#' @param num_triplets Integer. Total number of triplets to generate. If
#'   \code{NULL}, defaults to \code{nrow(samples)}.
#' @param skip_singletons Logical. If \code{TRUE}, anchors are sampled only
#'   from classes with at least two instances (so a distinct positive exists).
#'   If \code{FALSE}, singleton classes may be used for anchors and the
#'   function may allow a self-positive when no other same-class instance is
#'   available.
#' @param seed Optional integer seed for reproducible sampling. If provided,
#'   it is applied via \code{set.seed()}.
#'
#' @return A \code{tibble} with one row per generated triplet (up to
#' \code{num_triplets}) and the columns:
#' \itemize{
#'   \item \code{anchor}, \code{positive}, \code{negative}: list-columns
#'   containing the corresponding time-series objects.
#'   \item \code{anchor_idx}, \code{positive_idx}, \code{negative_idx}:
#'   integer indices pointing to the rows of \code{samples} used to build
#'   each triplet.
#' }
#'
#' @details
#' Triplets are generated by repeated random draws. To prevent infinite
#' loops in degenerate settings, the function limits attempts to
#' \code{max(10 * num_triplets, 10000)} draws. If no valid triplet is
#' produced, an error is raised.
#'
#' The function requires at least two distinct classes in \code{samples$label}.
#' When \code{skip_singletons = TRUE}, it also requires at least one class with
#' two or more instances.
#'
#' @author Alexandre Assuncao \email{alexcarssuncao@@gmail.com}
#' @keywords internal
#' @noRd
.contrastive_training_random_triplets <- function(samples,
                                                  num_triplets,
                                                  skip_singletons,
                                                  seed) {
    if (!is.null(seed)) set.seed(seed)

    n <- nrow(samples)
    if (is.null(num_triplets)) num_triplets <- n

    # Prepare class→indices map
    labels <- samples$label
    if (is.factor(labels)) labels <- as.character(labels)
    idx_by_class <- split(seq_len(n), labels)
    classes <- names(idx_by_class)

    # Sanity checks
    if (length(classes) < 2L) {
        stop(.conf("messages", ".contrastive_train_2class"))
    }

    # Anchor pool (optionally exclude singleton classes)
    class_sizes <- vapply(idx_by_class, length, integer(1))
    if (skip_singletons) {
        eligible_classes <- names(class_sizes[class_sizes >= 2L])
        anchor_pool <- unlist(idx_by_class[eligible_classes], use.names = FALSE)
        if (length(anchor_pool) == 0L) {
            stop(.conf("messages", ".contrastive_train_skip_singletons"))
        }
    } else {
        anchor_pool <- seq_len(n)
    }

    # Helpers
    sample_positive <- function(anchor_idx, pool) {
        if (length(pool) == 1L) {
            if (skip_singletons) {
                return(NA_integer_)
            }
            return(pool) # allow self-positive if desired
        }
        choices <- setdiff(pool, anchor_idx)
        if (length(choices) == 0L) {
            if (skip_singletons) {
                return(NA_integer_)
            }
            return(sample(pool, 1L, replace = TRUE))
        }
        sample(choices, 1L)
    }

    sample_negative <- function(anchor_label) {
        other <- setdiff(classes, anchor_label)
        if (length(other) == 0L) {
            return(NA_integer_)
        }
        neg_cls <- sample(other, 1L)
        sample(idx_by_class[[neg_cls]], 1L)
    }

    # Generate triplets
    triplets <- vector("list", num_triplets)
    wrote <- 0L
    max_tries <- max(10L * num_triplets, 10000L) # prevent degenerate loops
    tries <- 0L

    while (wrote < num_triplets && tries < max_tries) {
        tries <- tries + 1L
        a <- sample(anchor_pool, 1L)
        a_lbl <- labels[a]

        pos <- sample_positive(a, idx_by_class[[a_lbl]])
        if (is.na(pos)) next

        neg <- sample_negative(a_lbl)
        if (is.na(neg)) next

        wrote <- wrote + 1L
        triplets[[wrote]] <- tibble::tibble(
            anchor       = list(samples$time_series[[a]]),
            positive     = list(samples$time_series[[pos]]),
            negative     = list(samples$time_series[[neg]]),
            anchor_idx   = a,
            positive_idx = pos,
            negative_idx = neg
        )
    }

    if (wrote == 0L) {
        stop("messages", ".contrastive_train_no_triplets")
    }

    out <- dplyr::bind_rows(triplets[seq_len(wrote)])
    if (nrow(out) > num_triplets) out <- out[seq_len(num_triplets), ]
    out
}
#' @title Create torch/luz-ready triplet dataset
#'
#' @description
#' Creates a \code{torch::dataset()} that yields triplets for
#' contrastive / metric learning (e.g., triplet loss) in a format
#' convenient for \pkg{torch} and \pkg{luz}.
#'
#' Each observation is a list with:
#' \itemize{
#'   \item \code{x}: a tensor obtained by concatenating anchor, positive,
#'   and negative matrices along the feature/channel dimension.
#'   \item \code{y}: a scalar tensor containing the margin used by the loss.
#' }
#'
#' Optional per-band min/max scaling is supported via \code{q02} and
#' \code{q98}. When provided, each matrix is normalized as:
#' \deqn{scaled = (mat - q02) / max(q98 - q02, eps)}
#' and optionally clipped to \eqn{[0, 1]}.
#'
#' @param triplets A list of triplet objects (typically rows converted from a
#'   tibble) where each element contains \code{anchor}, \code{positive}, and
#'   \code{negative}. Each of these must be coercible to a numeric matrix
#'   with the same number of columns (bands/features). Matrices are expected
#'   to have shape \code{[n_times, n_bands]} (or any \code{[* , n_bands]}),
#'   and all three components must be compatible for concatenation.
#' @param margin Numeric scalar. Margin value to be returned as \code{y} for
#'   each sample (commonly used by triplet-margin losses).
#' @param q02 Optional numeric vector of per-band lower reference values
#'   (e.g., 2nd percentile). If \code{NULL}, no normalization is applied.
#' @param q98 Optional numeric vector of per-band upper reference values
#'   (e.g., 98th percentile). Must be the same length as \code{q02} when
#'   normalization is used.
#' @param clip Logical. If \code{TRUE}, clip normalized values to \eqn{[0, 1]}.
#' @param eps Numeric scalar. Small constant to avoid division by zero when
#'   \code{q98 - q02} is close to zero.
#'
#' @return A \code{torch::dataset} object named \code{"TripletDataset"}
#' implementing:
#' \itemize{
#'   \item \code{.length()}: number of triplets.
#'   \item \code{.getitem(i)}: a list with \code{x} and \code{y}.
#' }
#'
#' @details
#' \code{x} is built by converting each of \code{anchor}, \code{positive}, and
#' \code{negative} to matrices and then concatenating them with
#' \code{torch::torch_cat(..., dim = 2)}. This assumes the last dimension
#' corresponds to bands/features; after concatenation, the resulting feature
#' dimension is \code{3 * n_bands}.
#'
#' If either \code{q02} or \code{q98} is provided, the initializer checks
#' that both are consistent with the number of bands inferred from the first
#' triplet's anchor.
#'
#' @author Alexandre Assuncao \email{alexcarssuncao@@gmail.com}
#' @keywords internal
#' @noRd
.triplet_dataset <- torch::dataset(
    name = "TripletDataset",
    initialize = function(triplets,
                          margin = 1.0,
                          q02 = NULL,
                          q98 = NULL,
                          clip = TRUE,
                          eps = 1e-6) {
        self$triplets <- triplets
        self$margin <- margin
        self$q02 <- q02
        self$q98 <- q98
        self$clip <- clip
        self$eps <- eps
        self$model <- NULL

        # If stats are provided, sanity-check lengths against band count
        if (!is.null(q02) || !is.null(q98)) {
            one <- triplets[[1]]$anchor
            one_mat <- as.matrix(one)
            n_bands <- ncol(one_mat) # expects shape [n_times, n_bands] or [*, n_bands]
            stopifnot(length(q02) == n_bands, length(q98) == n_bands)
        }
    },
    .length = function() length(self$triplets),
    .getitem = function(index) {
        triplet <- self$triplets[[index]]

        normalize_mat <- function(mat) {
            # mat: [n_times, n_bands] (or any * x n_bands)
            if (is.null(self$q02)) {
                return(mat)
            }
            centered <- sweep(mat, 2, self$q02, `-`)
            denom <- pmax(self$q98 - self$q02, self$eps)
            scaled <- sweep(centered, 2, denom, `/`)
            if (self$clip) scaled <- pmin(pmax(scaled, 0), 1)
            scaled
        }

        anchor_mat <- normalize_mat(as.matrix(triplet$anchor))
        pos_mat <- normalize_mat(as.matrix(triplet$positive))
        neg_mat <- normalize_mat(as.matrix(triplet$negative))

        list(
            # Anchor, Positive and Negative concatenated
            x = torch::torch_cat(list(anchor_mat, pos_mat, neg_mat), dim = 2),
            # Margin
            y = torch::torch_tensor(self$margin)
        )
    }
)
