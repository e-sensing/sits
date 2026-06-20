# ---- Contrastive Learning triplet-building helpers ----

#' @title Split samples into train/val triplets for contrastive pre-training
#' @name .contrastive_learning_data_split
#' @keywords internal
#' @noRd
#' @author Alexandre Assuncao, \email{alexcarssuncao@@gmail.com}
#'
#' @description
#' Performs a train/validation split on the supplied \code{samples} and
#' builds (anchor, positive, negative) triplets using label information.
#' Three sampling strategies are supported:
#' \describe{
#'   \item{\code{"random"}}{Anchor and positive are drawn from the same class
#'     at random; the negative is drawn from a different class.}
#'   \item{\code{"hard"}}{Mini-batches of P classes x K samples are formed,
#'     embedded, and pairwise distances computed. The hardest positive
#'     (farthest same-class) and hardest negative (closest different-class)
#'     are selected.}
#'   \item{\code{"semi-hard"}}{Like hard, but the positive is restricted to
#'     same-class samples that are closer than the negative.}
#' }
#'
#' @param samples          \code{sits} tibble of labelled time-series samples.
#' @param sampling_method  Character. One of \code{"random"}, \code{"hard"},
#'   or \code{"semi-hard"}.
#' @param validation_split Numeric in (0, 1). Fraction held out for validation.
#' @param skip_singletons  Logical. Skip classes with a single sample?
#' @param classes_per_batch Integer or NULL. Classes per mini-batch (hard/semi-hard).
#' @param samples_per_class Integer or NULL. Samples per class in a mini-batch.
#' @param num_triplets     Integer or NULL. Total triplets to generate.
#' @param target_batch_size Integer. Target batch size for auto-tuning
#'   \code{samples_per_class}.
#' @param embed_fn         Function. Embeds a single time-series tibble into a
#'   numeric vector.
#' @param dist_fn          Function. Computes distance between two embeddings.
#' @param seed             Integer or NULL. Random seed.
#'
#' @return A named list with elements \code{train} and \code{val}, each a
#'   tibble with columns \code{anchor}, \code{positive}, \code{negative}
#'   (list-columns of time-series tibbles) and index columns.
#'
.contrastive_learning_data_split <- function(samples,
                                             sampling_method,
                                             validation_split,
                                             skip_singletons,
                                             classes_per_batch,
                                             samples_per_class,
                                             num_triplets,
                                             target_batch_size,
                                             embed_fn,
                                             dist_fn,
                                             seed) {

    if (sampling_method == "hard") {
        triplets <- .contrastive_learning_hard_triplets(
            samples = samples,
            classes_per_batch = classes_per_batch,
            samples_per_class = samples_per_class,
            num_triplets      = num_triplets,
            target_batch_size = target_batch_size,
            embed_fn          = embed_fn,
            dist_fn           = dist_fn
        )
    } else if (sampling_method == "semi-hard") {
        triplets <- .contrastive_learning_semi_hard_triplets(
            samples, classes_per_batch, samples_per_class,
            num_triplets, target_batch_size, embed_fn, dist_fn
        )
    } else {
        triplets <- .contrastive_learning_random_triplets(
            samples, num_triplets, skip_singletons, seed
        )
    }

    n <- nrow(triplets)
    all_idx <- seq_len(n)
    n_val <- floor(n * validation_split)

    val_idx <- if (n_val > 0L) sample(all_idx, size = n_val) else integer(0)
    is_val <- logical(n)
    is_val[val_idx] <- TRUE

    train_data <- triplets[!is_val, , drop = FALSE]
    val_data   <- triplets[ is_val, , drop = FALSE]

    list(train = train_data, val = val_data)
}

#' @title Hard triplet sampling method
#' @name .contrastive_learning_hard_triplets
#' @keywords internal
#' @noRd
#' @author Alexandre Assuncao, \email{alexcarssuncao@@gmail.com}
.contrastive_learning_hard_triplets <- function(samples,
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

    if (is.null(classes_per_batch)) {
        classes_per_batch <- min(n_classes, 8)
    }
    if (is.null(samples_per_class)) {
        samples_per_class <- max(2, floor(target_batch_size / classes_per_batch))
    }

    while (length(triplets) < num_triplets) {
        chosen_classes <- sample(classes, classes_per_batch)
        batch_idx <- unlist(
            purrr::map(chosen_classes, function(cls) {
                idxs <- which(all_labels == cls)
                if (length(idxs) >= samples_per_class) {
                    sample(idxs, samples_per_class)
                } else {
                    sample(idxs, samples_per_class, replace = TRUE)
                }
            })
        )

        feats <- purrr::map(batch_idx, ~ embed_fn(samples$time_series[[.x]]))
        feats <- do.call(rbind, feats)
        labs  <- all_labels[batch_idx]

        n <- nrow(feats)
        D <- matrix(0, n, n)
        for (i in seq_len(n)) {
            D[i, ] <- vapply(
                seq_len(n),
                function(j) dist_fn(feats[i, ], feats[j, ]),
                numeric(1)
            )
        }

        order_j <- sample(seq_along(batch_idx))

        for (j in order_j) {
            same_cls  <- which(labs == labs[j] & seq_along(labs) != j)
            other_cls <- which(labs != labs[j])
            if (length(same_cls) == 0 || length(other_cls) == 0) next

            neg_j <- other_cls[which.min(D[j, other_cls])]
            pos_j <- same_cls[which.max(D[j, same_cls])]

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

    dplyr::bind_rows(triplets[seq_len(num_triplets)])
}

#' @title Semi-hard triplet sampling method
#' @name .contrastive_learning_semi_hard_triplets
#' @keywords internal
#' @noRd
#' @author Alexandre Assuncao, \email{alexcarssuncao@@gmail.com}
.contrastive_learning_semi_hard_triplets <- function(samples,
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

    if (is.null(classes_per_batch)) {
        classes_per_batch <- min(n_classes, 8)
    }
    if (is.null(samples_per_class)) {
        samples_per_class <- max(2, floor(target_batch_size / classes_per_batch))
    }

    while (length(triplets) < num_triplets) {
        chosen_classes <- sample(classes, classes_per_batch)
        batch_idx <- unlist(
            purrr::map(chosen_classes, function(cls) {
                idxs <- which(all_labels == cls)
                if (length(idxs) >= samples_per_class) {
                    sample(idxs, samples_per_class)
                } else {
                    sample(idxs, samples_per_class, replace = TRUE)
                }
            })
        )

        feats <- purrr::map(batch_idx, ~ embed_fn(samples$time_series[[.x]]))
        feats <- do.call(rbind, feats)
        labs  <- all_labels[batch_idx]

        n <- nrow(feats)
        D <- matrix(0, n, n)
        for (i in seq_len(n)) {
            D[i, ] <- vapply(
                seq_len(n),
                function(j) dist_fn(feats[i, ], feats[j, ]),
                numeric(1)
            )
        }

        order_j <- sample(seq_along(batch_idx))

        for (j in order_j) {
            same_cls   <- which(labs == labs[j] & seq_along(labs) != j)
            other_cls  <- which(labs != labs[j])
            if (length(same_cls) == 0 || length(other_cls) == 0) next

            neg_j <- other_cls[which.min(D[j, other_cls])]
            d_neg <- D[j, neg_j]

            d_pos_all <- D[j, same_cls]
            semi_idx <- same_cls[d_pos_all < d_neg]
            if (length(semi_idx) > 0) {
                pos_j <- semi_idx[which.max(D[j, semi_idx])]
            } else {
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
    dplyr::bind_rows(triplets[seq_len(num_triplets)])
}

#' @title Random triplet sampling method
#' @name .contrastive_learning_random_triplets
#' @keywords internal
#' @noRd
#' @author Alexandre Assuncao, \email{alexcarssuncao@@gmail.com}
.contrastive_learning_random_triplets <- function(samples,
                                                  num_triplets,
                                                  skip_singletons,
                                                  seed) {
    if (!is.null(seed)) set.seed(seed)

    n <- nrow(samples)
    if (is.null(num_triplets)) num_triplets <- n

    labels <- samples$label
    if (is.factor(labels)) labels <- as.character(labels)
    idx_by_class <- split(seq_len(n), labels)
    classes <- names(idx_by_class)

    if (length(classes) < 2L) {
        stop(
            .conf("messages", "sits_contrastive_learning_insufficient_classes"),
            call. = FALSE
        )
    }

    class_sizes <- vapply(idx_by_class, length, integer(1))
    if (skip_singletons) {
        eligible_classes <- names(class_sizes[class_sizes >= 2L])
        anchor_pool <- unlist(
            idx_by_class[eligible_classes], use.names = FALSE
        )
        if (length(anchor_pool) == 0L) {
            stop(
                .conf("messages",
                      "sits_contrastive_learning_singleton_classes"),
                call. = FALSE
            )
        }
    } else {
        anchor_pool <- seq_len(n)
    }

    sample_positive <- function(anchor_idx, pool) {
        if (length(pool) == 1L) {
            if (skip_singletons) return(NA_integer_)
            return(pool)
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

    triplets <- vector("list", num_triplets)
    wrote <- 0L
    max_tries <- max(10L * num_triplets, 10000L)
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
        stop(
            .conf("messages", "sits_contrastive_learning_no_triplets"),
            call. = FALSE
        )
    }

    out <- dplyr::bind_rows(triplets[seq_len(wrote)])
    if (nrow(out) > num_triplets) out <- out[seq_len(num_triplets), ]
    out
}

#' @title Torch dataset for triplet-based contrastive learning
#' @name .triplet_dataset
#' @keywords internal
#' @noRd
#' @author Alexandre Assuncao, \email{alexcarssuncao@@gmail.com}
#'
#' @description
#' A \code{torch::dataset} that yields one item per triplet. Each item is a
#' list with:
#' \describe{
#'   \item{\code{x}}{A \code{float} tensor of shape
#'     \code{[3, n_times, n_bands]}: anchor, positive, negative.}
#'   \item{\code{y}}{A scalar margin tensor (used by the triplet loss).}
#' }
#'
#' @param triplets Tibble with list-columns \code{anchor}, \code{positive},
#'   \code{negative}.
#' @param n_times  Integer. Number of time steps.
#' @param margin   Numeric. Triplet loss margin.
#'
.triplet_dataset <- torch::dataset(
    name = "TripletDataset",

    initialize = function(triplets, n_times, margin = 1.0) {
        self$triplets <- triplets
        self$margin   <- margin
        self$n_times  <- n_times
    },

    .length = function() nrow(self$triplets),

    .getitem = function(idx) {
        triplet <- self$triplets[idx, ]

        anchor_mat <- triplet$anchor[[1]] |>
            dplyr::select(-"Index") |>
            as.matrix()

        pos_mat <- triplet$positive[[1]] |>
            dplyr::select(-"Index") |>
            as.matrix()

        neg_mat <- triplet$negative[[1]] |>
            dplyr::select(-"Index") |>
            as.matrix()

        anchor_t <- torch::torch_tensor(
            anchor_mat, dtype = torch::torch_float()
        )
        pos_t <- torch::torch_tensor(
            pos_mat, dtype = torch::torch_float()
        )
        neg_t <- torch::torch_tensor(
            neg_mat, dtype = torch::torch_float()
        )
        margin <- torch::torch_tensor(
            self$margin, dtype = torch::torch_float()
        )

        list(
            x = torch::torch_stack(list(anchor_t, pos_t, neg_t), dim = 1),
            y = margin
        )
    }
)
