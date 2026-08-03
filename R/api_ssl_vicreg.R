# ---- VICReg self-supervised learning helpers ----

#' @title Split samples into train/val for VICReg pre-training
#' @name .vicreg_data_split
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description
#' Normalises \code{samples} and performs a train/validation split of sample
#' indices. No pre-computed pairs are built because VICReg creates
#' views on-the-fly via time-warping augmentation.
#'
#' @param samples          \code{sits} tibble of time-series samples.
#' @param validation_split Numeric in (0, 1). Fraction held out for
#'   validation.
#'
#' @return A named list with elements \code{train} and \code{val}. Each
#'   element is itself a list with components:
#'   \describe{
#'     \item{\code{feats}}{Numeric matrix of shape
#'       \code{[n_split, n_times * n_bands]} — normalized features.}
#'   }
#'
.vicreg_data_split <- function(samples, validation_split) {
    # Compute normalization statistics and build normalized feature matrix
    ml_stats <- .samples_stats(samples)
    preds    <- .predictors(samples)
    # [n, n_times*n_bands]
    feats    <- .pred_features_normalize(preds, stats = ml_stats)

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

    list(
        train = list(feats = feats[train_idx, , drop = FALSE]),
        val   = list(feats = feats[val_idx,   , drop = FALSE])
    )
}

#' @title Torch Dataset for VICReg with resampling augmentation
#' @name .vicreg_resampling_dataset
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description
#' A \code{torch::dataset} that yields one item per sample.  For each
#' sample, two augmented views are created on-the-fly using the
#' resampling strategy of Saget et al. (2025) via
#' \code{.ssl_apply_resampling()}.
#'
#' Each item is a list with:
#' \describe{
#'   \item{\code{x}}{A \code{float} tensor of shape
#'     \code{[2, n_times, n_bands]}.}
#'   \item{\code{y}}{A scalar zero tensor (dummy target; the VICReg
#'     loss ignores it).}
#' }
#'
#' @param split   List with element \code{feats}, a numeric matrix
#'   of shape \code{[n_samples, n_times * n_bands]}.
#' @param n_times Integer. Number of time steps in the time series.
#'
#' @return A \code{torch::dataset} object compatible with
#'   \code{torch::dataloader}.
#'
.vicreg_resampling_dataset <- torch::dataset(
    name = ".VICRegResamplingDataset",

    initialize = function(split, n_times) {
        self$feats   <- split[["feats"]]
        self$n_times <- as.integer(n_times)
        self$n_bands <- as.integer(ncol(split[["feats"]]) / n_times)
    },

    .getitem = function(i) {
        nt <- self$n_times
        nb <- self$n_bands

        # Reshape flat feature vector to [n_times, n_bands]
        sample_mat <- matrix(self$feats[i, ], nrow = nt, ncol = nb)

        # Apply resampling augmentation (returns both views)
        views <- .ssl_apply_resampling(sample_mat)

        list(
            x = torch::torch_stack(list(
                torch::torch_tensor(views$view1, dtype = torch::torch_float()),
                torch::torch_tensor(views$view2, dtype = torch::torch_float())
            )),
            y = torch::torch_zeros(1L, dtype = torch::torch_float())
        )
    },

    .length = function() {
        nrow(self$feats)
    }
)

#' @title Extract off-diagonal elements of a square tensor
#' @name .vicreg_off_diagonal
#' @keywords internal
#' @noRd
#'
#' @description
#' Returns a 1-D tensor containing every element of the square matrix
#' \code{x} that is \emph{not} on the main diagonal.  Used by the VICReg
#' covariance regularization term.
#'
#' The implementation mirrors the reference \code{off_diagonal()} function
#' from the Facebook Research VICReg codebase: flatten, drop the last
#' element, reshape to \code{(n-1, n+1)}, take columns \code{2:(n+1)},
#' and flatten again.
#'
#' @param x A square \code{torch_tensor} of shape \code{[n, n]}.
#'
#' @return A 1-D \code{torch_tensor} of length \code{n*(n-1)}.
#'
.vicreg_off_diagonal <- function(x) {
    n <- x$size(1)
    x$flatten()[1:(n * n - 1)]$view(c(n - 1L, n + 1L))[, 2:(n + 1)]$flatten()
}
