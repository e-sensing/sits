# ---- Barlow Twins pair-building helpers ----

#' @title Split samples into train/val pairs for Barlow Twins pre-training
#' @name .barlow_twins_data_split
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description
#' Normalises \code{samples}, performs a stratified-style train/validation
#' split, and builds view-pairs used as input to the Barlow Twins loss.
#' For each anchor sample, a positive view is drawn
#' uniformly at random from **the same class label**. Singletons
#' (classes with only one sample in the split) fall back to self-pairing
#' and trigger a warning.
#'
#' @param samples          \code{sits} tibble of labelled time-series samples.
#' @param validation_split Numeric in (0, 1). Fraction held out for validation.
#' @param num_pairs        Integer or \code{NULL}. Number of pairs per split.
#'   Defaults to the number of samples in each split when \code{NULL}.
#'
#' @return A named list with elements \code{train} and \code{val}.  Each
#'   element is itself a list with components:
#'   \describe{
#'     \item{\code{a}}{Numeric matrix of shape
#'       \code{[n_pairs, n_times * n_bands]} for view A.}
#'     \item{\code{b}}{Numeric matrix of same shape for view B.}
#'   }
#'
.barlow_twins_data_split <- function(samples,
                                     validation_split,
                                     num_pairs       = NULL) {
    # Compute normalisation statistics and build normalised feature matrix
    ml_stats <- .samples_stats(samples)
    preds    <- .predictors(samples)
    # [n, n_times*n_bands]
    feats    <- .pred_features_normalize(preds, stats = ml_stats)
    # length-n character vector
    labels   <- .pred_references(preds)

    n_samples <- nrow(feats)

    # Train / validation split (same logic as sits_ssl_mae)
    idx   <- sample.int(n_samples)
    n_val <- floor(length(idx) * validation_split)
    if (n_val > 0L) {
        val_idx   <- idx[seq_len(n_val)]
        train_idx <- idx[-seq_len(n_val)]
    } else {
        val_idx   <- integer(0L)
        train_idx <- idx
    }

    # Resolve number of pairs for each split
    np_train <- if (.has(num_pairs)) {
        min(num_pairs, length(train_idx))
    } else {
        length(train_idx)
    }
    np_val <- if (length(val_idx) > 0L) {
        if (.has(num_pairs)) {
            max(1L, floor(num_pairs * validation_split))
        } else {
            length(val_idx)
        }
    } else {
        0L
    }

    # ------------------------------------------------------------------
    # Inner helper: build one set of pairs from a given set of indices
    # ------------------------------------------------------------------
    make_pairs <- function(split_idx, n_pairs) {
        n <- length(split_idx)

        # Handle degenerate case
        if (n == 0L) {
            empty <- matrix(numeric(0L), nrow = 0L, ncol = ncol(feats))
            return(list(a = empty, b = empty))
        }

        # Sample anchors (view A)
        a_idx <- sample(split_idx, n_pairs, replace = TRUE)

        # Sample positives (view B)
        # Label-based: positive must share the same class as the anchor
        split_labels <- labels[split_idx]
        has_singleton <- FALSE

        b_idx <- vapply(a_idx, function(ai) {
            same_class <- split_idx[split_labels == labels[ai]]
            # Exclude self when at least one alternative exists
            candidates <- same_class[same_class != ai]
            if (length(candidates) >= 1L) {
                sample(candidates, 1L)
            } else {
                # Singleton class: fall back to self-pairing
                has_singleton <<- TRUE
                ai
            }
        }, integer(1L))

        # Warn once if any singletons forced a self-pair
        if (has_singleton && .message_warnings()) {
            warning(
                .conf("messages", "sits_barlow_twins_singleton_classes"),
                call. = FALSE
            )
        }

        list(
            a = feats[a_idx, , drop = FALSE],
            b = feats[b_idx, , drop = FALSE]
        )
    }

    list(
        train = make_pairs(train_idx, np_train),
        val   = make_pairs(val_idx,   np_val)
    )
}

#' @title Torch Dataset for Barlow Twins two-view pairs
#' @name .pair_dataset
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description
#' A \code{torch::dataset} that yields one item per view-pair. Each item is a
#' list with:
#' \describe{
#'   \item{\code{x}}{A \code{float} tensor of shape
#'     \code{[2, n_times, n_bands]}, where index 1 is the anchor (view A)
#'     and index 2 is the positive (view B).}
#'   \item{\code{y}}{A scalar zero tensor (dummy target; the Barlow Twins loss
#'     ignores the target).}
#' }
#'
#' @param pairs   List with elements \code{a} and \code{b}, each a numeric
#'   matrix of shape \code{[n_pairs, n_times * n_bands]}.
#' @param n_times Integer. Number of time steps in the time series.
#'
#' @return A \code{torch::dataset} object compatible with
#'   \code{torch::dataloader}.
#'
.pair_dataset <- torch::dataset(
    name = ".PairDataset",

    initialize = function(pairs, n_times) {
        self$a       <- pairs[["a"]]
        self$b       <- pairs[["b"]]
        self$n_times <- as.integer(n_times)
        # Derive n_bands from the feature width
        self$n_bands <- as.integer(ncol(pairs[["a"]]) / n_times)
    },

    .getitem = function(i) {
        nt <- self$n_times
        nb <- self$n_bands

        view_a <- torch::torch_tensor(
            array(self$a[i, ], dim = c(nt, nb)),
            dtype = torch::torch_float()
        )
        view_b <- torch::torch_tensor(
            array(self$b[i, ], dim = c(nt, nb)),
            dtype = torch::torch_float()
        )

        list(
            # Stack along dim 1 → [2, n_times, n_bands]
            # barlow_twins_model$forward() expects x[, 1, , ] and x[, 2, , ]
            x = torch::torch_stack(list(view_a, view_b)),
            y = torch::torch_zeros(1L, dtype = torch::torch_float())
        )
    },

    .length = function() {
        nrow(self$a)
    }
)
