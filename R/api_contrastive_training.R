#' @title split sits samples into a set of training and validation
#'        triplets.
#'
#' @author Alexandre Assuncao \email{alexcarssuncao@@gmail.com}
#' @references
#' Schroff, F., Kalenichenko, D., & Philbin, J. (2015).
#' _FaceNet: A Unified Embedding for Face Recognition and Clustering_.
#' In _Proceedings of the IEEE Conference on Computer Vision and Pattern Recognition_ (CVPR), 815–823.
#' doi:10.1109/CVPR.2015.7298682
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
                                             embed_fn = function(ts) {
                                                 mat <- as.matrix(ts[, sapply(ts, is.numeric)])
                                                 as.vector(t(mat))
                                             },
                                             dist_fn = function(a, b) sqrt(sum((a - b)^2)),
                                             seed = NULL) {

    if (sampling_method == "hard") {
        triplets <- .contrastive_training_hard_triplets(
            samples, classes_per_batch, samples_per_class,
            num_triplets, target_batch_size, embed_fn, dist_fn
        )
    } else if (sampling_method == "semi-hard") {
        triplets <- .contrastive_training_semi_hard_triplets(
            samples, classes_per_batch, samples_per_class,
            num_triplets, target_batch_size, embed_fn, dist_fn
        )
    } else {
        triplets <- .contrastive_training_random_triplets(
            samples, num_triplets, skip_singletons, seed
        )
    }

    n <- nrow(triplets)
    all_idx <- seq_len(n)
    n_val <- floor(n * validation_split)

    val_idx <- if (n_val > 0L) sample(all_idx, size = n_val) else integer(0)
    is_val <- logical(n); is_val[val_idx] <- TRUE

    train_data <- triplets[!is_val, , drop = FALSE]
    val_data   <- triplets[ is_val, , drop = FALSE]

    list(train = train_data, val = val_data)
}
#' @title hard triplet sampling method
#'
#' @author Alexandre Assuncao \email{alexcarssuncao@@gmail.com}
#' @keywords internal
#' @noRd
.contrastive_training_hard_triplets <- function(samples, classes_per_batch, samples_per_class,
                                                num_triplets, target_batch_size, embed_fn, dist_fn) {

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
        samples_per_class <- max(2, floor(target_batch_size / classes_per_batch))
    }

    while (length(triplets) < num_triplets) {
        # 1) Sample P classes and then K examples per class
        chosen_classes <- sample(classes, classes_per_batch)
        batch_idx <- unlist(
            purrr::map(chosen_classes, function(cls) {
                idxs <- which(all_labels == cls)
                if (length(idxs) >= samples_per_class) {
                    sample(idxs, samples_per_class)
                } else {
                    sample(idxs, samples_per_class, replace = TRUE)
                }
            }))

        # 2) Build feature matrix and labels for batch
        feats <- purrr::map(batch_idx, ~ embed_fn(samples$time_series[[.x]]))
        feats <- do.call(rbind, feats)
        labs  <- all_labels[batch_idx]

        # 3) Pairwise distance matrix
        n <- nrow(feats)
        D <- matrix(0, n, n)
        for (i in seq_len(n)) {
            D[i, ] <- vapply(seq_len(n),
                             function(j) dist_fn(feats[i, ], feats[j, ]),
                             numeric(1))
        }

        order_j  <- sample(seq_along(batch_idx))  # shuffle anchor order

        # 4) For each anchor, pick batch-hard positive and batch-hard negative
        for (j in order_j) {
            same_cls  <- which(labs == labs[j] & seq_along(labs) != j)
            other_cls <- which(labs != labs[j])
            if (length(same_cls) == 0 || length(other_cls) == 0) next

            # Hard negative: closest example from a different class
            neg_j <- other_cls[ which.min(D[j, other_cls]) ]

            # Hard positive: farthest example from the same class
            pos_j <- same_cls[ which.max(D[j, same_cls]) ]

            triplets[[length(triplets) + 1]] <- tibble::tibble(
                anchor       = list(samples$time_series[[ batch_idx[j] ]]),
                positive     = list(samples$time_series[[ batch_idx[pos_j] ]]),
                negative     = list(samples$time_series[[ batch_idx[neg_j] ]]),
                anchor_idx   = batch_idx[j],
                positive_idx = batch_idx[pos_j],
                negative_idx = batch_idx[neg_j]
            )

            if (length(triplets) >= num_triplets) break
        }
    }

    dplyr::bind_rows(triplets[1:num_triplets])
}
#' @title semi-hard triplet sampling method
#'
#' @author Alexandre Assuncao \email{alexcarssuncao@@gmail.com}
#' @keywords internal
#' @noRd
.contrastive_training_semi_hard_triplets <- function(samples, classes_per_batch, samples_per_class,
                                                     num_triplets, target_batch_size, embed_fn, dist_fn) {

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
        samples_per_class <- max(2, floor(target_batch_size / classes_per_batch))
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
            }))

        # 2) Build feature matrix and labels for batch
        feats <- purrr::map(batch_idx, ~ embed_fn(samples$time_series[[.x]]))
        feats <- do.call(rbind, feats)
        labs  <- all_labels[batch_idx]

        # 3) Pairwise distance matrix
        n <- nrow(feats)
        D <- matrix(0, n, n)

        for (i in seq_len(n)) {
            # compute distances from feats[i, ] to every row in feats
            D[i, ] <- vapply(seq_len(n),
                             function(j) dist_fn(feats[i, ], feats[j, ]),
                             numeric(1))
        }

        order_j  <- sample(seq_along(batch_idx))  # <-- shuffle anchor order

        # 4) For *each* anchor in this batch, pick hardest pos/neg
        for (j in order_j) {
            same_cls   <- which(labs == labs[j]  & seq_along(labs) != j)
            other_cls  <- which(labs != labs[j])
            if (length(same_cls) == 0 || length(other_cls) == 0) next

            # Select hardest negative
            neg_j <- other_cls[ which.min(D[j, other_cls]) ]
            d_neg <- D[j, neg_j]

            # distances to all same‐class examples
            d_pos_all <- D[j, same_cls]
            # keep only those strictly closer than the negative
            semi_idx <- same_cls[ d_pos_all < d_neg ]
            if (length(semi_idx) > 0) {
                # among those, pick the *hardest* semi‐hard positive
                pos_j <- semi_idx[ which.max(D[j, semi_idx]) ]
            } else {
                # fallback: no semi‐hards, pick the easiest positive
                pos_j <- same_cls[ which.min(d_pos_all) ]
            }

            triplets[[length(triplets) + 1]] <- tibble::tibble(
                anchor       = list(samples$time_series[[ batch_idx[j] ]]),
                positive     = list(samples$time_series[[ batch_idx[pos_j] ]]),
                negative     = list(samples$time_series[[ batch_idx[neg_j] ]]),
                anchor_idx   = batch_idx[j],
                positive_idx = batch_idx[pos_j],
                negative_idx = batch_idx[neg_j]
            )
            if (length(triplets) >= num_triplets) break
        }
    }
    dplyr::bind_rows(triplets[1:num_triplets])
}
#' @title random triplet sampling method
#'
#' @author Alexandre Assuncao \email{alexcarssuncao@@gmail.com}
#' @keywords internal
#' @noRd
.contrastive_training_random_triplets <- function(samples, num_triplets, skip_singletons, seed) {
    if (!is.null(seed)) set.seed(seed)

    n <- nrow(samples)
    if (is.null(num_triplets)) num_triplets <- n

    # Prepare class→indices map
    labels <- samples$label
    if (is.factor(labels)) labels <- as.character(labels)
    idx_by_class <- split(seq_len(n), labels)
    classes <- names(idx_by_class)

    # Sanity checks
    if (length(classes) < 2L)
        .conf("messages", "sits_contrastive_triplets_insufficient_classes")

    # Anchor pool
    class_sizes <- vapply(idx_by_class, length, integer(1))
    if (skip_singletons) {
        eligible_classes <- names(class_sizes[class_sizes >= 2L])
        anchor_pool <- unlist(idx_by_class[eligible_classes], use.names = FALSE)
        if (length(anchor_pool) == 0L)
            .conf("messages", "sits_contrastive_triplets_sampling_fail")

    } else {
        anchor_pool <- seq_len(n)
    }

    # Helpers
    sample_positive <- function(anchor_idx, pool) {
        if (length(pool) == 1L) {
            if (skip_singletons) return(NA_integer_)
            return(pool) # allow self-positive if desired
        }
        choices <- setdiff(pool, anchor_idx)
        if (length(choices) == 0L) {
            if (skip_singletons) return(NA_integer_)
            return(sample(pool, 1L, replace = TRUE))
        }
        sample(choices, 1L)
    }

    sample_negative <- function(anchor_label) {
        other <- setdiff(classes, anchor_label)
        if (length(other) == 0L) return(NA_integer_)
        neg_cls <- sample(other, 1L)
        sample(idx_by_class[[neg_cls]], 1L)
    }

    # Generate triplets
    triplets <- vector("list", num_triplets)
    wrote <- 0L
    max_tries <- max(10L * num_triplets, 10000L)  # prevent degenerate loops
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

    if (wrote == 0L)
        .conf("messages", "sits_contrastive_triplets_sampling_fail")

    out <- dplyr::bind_rows(triplets[seq_len(wrote)])
    if (nrow(out) > num_triplets) out <- out[seq_len(num_triplets), ]
    out
}
#' @title create torch/luz -ready triplet dataset
#'
#' @author Alexandre Assuncao \email{alexcarssuncao@@gmail.com}
#' @keywords internal
#' @noRd
.triplet_dataset <- torch::dataset(
    name = "TripletDataset",
    initialize = function(triplets,
                          n_times,
                          margin = 1.0,
                          q02    = NULL,
                          q98    = NULL,
                          clip   = TRUE,
                          eps    = 1e-6
    ) {
        self$triplets <- triplets
        self$margin   <- margin
        self$q02      <- q02
        self$q98      <- q98
        self$n_times  <- n_times
        self$clip     <- clip
        self$eps      <- eps

    },

    .length = function() nrow(self$triplets),

    .getitem = function(idx) {

        # Helper fn to normalize the data
        normalize_mat <- function(mat) {
            # mat: [n_times, n_bands]
            if (is.null(self$q02)) return(mat)
            centered <- sweep(mat, 2, self$q02, `-`)
            denom    <- pmax(self$q98 - self$q02, self$eps)
            scaled   <- sweep(centered, 2, denom, `/`)
            if (self$clip) scaled <- pmin(pmax(scaled, 0), 1)
            scaled
        }

        triplet <- self$triplets[idx, ]

        anchor_mat <- triplet$anchor[[1]] |>
            dplyr::select(-Index) |>
            as.matrix() |>
            normalize_mat()

        pos_mat <- triplet$positive[[1]] |>
            dplyr::select(-Index) |>
            as.matrix() |>
            normalize_mat()

        neg_mat <- triplet$negative[[1]] |>
            dplyr::select(-Index) |>
            as.matrix() |>
            normalize_mat()

        anchor_t <- torch::torch_tensor(anchor_mat, dtype = torch::torch_float())
        pos_t    <- torch::torch_tensor(pos_mat,    dtype = torch::torch_float())
        neg_t    <- torch::torch_tensor(neg_mat,    dtype = torch::torch_float())
        margin <- torch::torch_tensor(self$margin,  dtype = torch::torch_float())

        list(x = torch::torch_stack(list(anchor_t, pos_t, neg_t), dim = 1), y = margin)
    }
)
