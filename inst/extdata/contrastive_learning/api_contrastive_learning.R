# ---- Supervised Contrastive Learning pair-building helpers ----

#' @title Split samples into train/val pairs for supervised contrastive
#'   pre-training
#' @name .contrastive_learning_data_split
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Alexandre Assuncao, \email{alexcarssuncao@@gmail.com}
#'
#' @description
#' Normalises \code{samples}, performs a train/validation split, and builds
#' view-pairs used as input to the SupCon loss. Unlike Barlow Twins, the
#' returned structure also includes integer label vectors because the
#' supervised contrastive loss requires labels to build the positive mask.
#'
#' Two pairing strategies are supported:
#' \describe{
#'   \item{\code{"label"}}{For each anchor, the second view is drawn from
#'     the same class label, giving two semantically consistent views.
#'     Singletons fall back to self-pairing and trigger a warning.}
#'   \item{\code{"random"}}{Both views are drawn independently and uniformly
#'     at random from the split, regardless of label.}
#' }
#'
#' @param samples          \code{sits} tibble of labelled time-series samples.
#' @param validation_split Numeric in (0, 1). Fraction held out for validation.
#' @param num_pairs        Integer or \code{NULL}. Number of pairs per split.
#'   Defaults to the number of samples in each split when \code{NULL}.
#' @param pair_smp_method  Character. One of \code{"label"} or \code{"random"}.
#'
#' @return A named list with elements \code{train} and \code{val}.  Each
#'   element is itself a list with components:
#'   \describe{
#'     \item{\code{a}}{Numeric matrix of shape
#'       \code{[n_pairs, n_times * n_bands]} for view A.}
#'     \item{\code{b}}{Numeric matrix of same shape for view B.}
#'     \item{\code{labels}}{Integer vector of length \code{n_pairs} giving
#'       the class code (1-based) for each pair's anchor.}
#'   }
#'
.contrastive_learning_data_split <- function(samples,
                                             validation_split,
                                             num_pairs       = NULL,
                                             pair_smp_method = "label") {
    # Compute normalisation statistics and build normalised feature matrix
    ml_stats <- .samples_stats(samples)
    preds    <- .pred_normalize(.predictors(samples), stats = ml_stats)
    feats    <- as.matrix(.pred_features(preds))   # [n, n_times*n_bands]
    labels_chr <- .pred_references(preds)           # character vector

    # Build integer label codes (1-based)
    unique_labels <- .samples_labels(samples)
    code_labels   <- seq_along(unique_labels)
    names(code_labels) <- unique_labels
    labels_int <- unname(code_labels[labels_chr])

    n_samples <- nrow(feats)

    # Train / validation split
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
            return(list(a = empty, b = empty, labels = integer(0L)))
        }

        # Sample anchors (view A)
        a_idx <- sample(split_idx, n_pairs, replace = TRUE)

        # Sample positives (view B)
        if (pair_smp_method == "label") {
            split_labels <- labels_chr[split_idx]
            has_singleton <- FALSE

            b_idx <- vapply(a_idx, function(ai) {
                same_class <- split_idx[split_labels == labels_chr[ai]]
                candidates <- same_class[same_class != ai]
                if (length(candidates) >= 1L) {
                    sample(candidates, 1L)
                } else {
                    has_singleton <<- TRUE
                    ai
                }
            }, integer(1L))

            if (has_singleton && .message_warnings()) {
                warning(
                    .conf("messages",
                          "sits_contrastive_learning_singleton_classes"),
                    call. = FALSE
                )
            }
        } else {
            b_idx <- sample(split_idx, n_pairs, replace = TRUE)
        }

        list(
            a      = feats[a_idx, , drop = FALSE],
            b      = feats[b_idx, , drop = FALSE],
            labels = labels_int[a_idx]
        )
    }

    list(
        train = make_pairs(train_idx, np_train),
        val   = make_pairs(val_idx,   np_val)
    )
}

#' @title Torch Dataset for supervised contrastive two-view pairs with labels
#' @name .contrastive_supcon_dataset
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description
#' A \code{torch::dataset} that yields one item per view-pair. Each item is a
#' list with:
#' \describe{
#'   \item{\code{x}}{A \code{float} tensor of shape
#'     \code{[2, n_times, n_bands]}, where index 1 is view A and index 2
#'     is view B.}
#'   \item{\code{y}}{A scalar \code{long} tensor giving the integer class
#'     label of the anchor (used by the supervised contrastive loss to
#'     build the positive mask within the batch).}
#' }
#'
#' @param pairs   List with elements \code{a}, \code{b} (each a numeric
#'   matrix of shape \code{[n_pairs, n_times * n_bands]}) and \code{labels}
#'   (integer vector of length \code{n_pairs}).
#' @param n_times Integer. Number of time steps in the time series.
#'
#' @return A \code{torch::dataset} object compatible with
#'   \code{torch::dataloader}.
#'
.contrastive_supcon_dataset <- torch::dataset(
    name = ".SupConDataset",

    initialize = function(pairs, n_times) {
        self$a       <- pairs[["a"]]
        self$b       <- pairs[["b"]]
        self$labels  <- pairs[["labels"]]
        self$n_times <- as.integer(n_times)
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
            x = torch::torch_stack(list(view_a, view_b)),
            y = torch::torch_tensor(
                self$labels[i], dtype = torch::torch_long()
            )
        )
    },

    .length = function() {
        nrow(self$a)
    }
)
