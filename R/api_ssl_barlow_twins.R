# ---- SSL Barlow Twins helpers (augmentation-based) ----

#' @title Split samples into train/val for SSL Barlow Twins pre-training
#' @name .ssl_bt_data_split
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description
#' Normalises \code{samples} and performs a train/validation split of sample
#' indices.
#' Unlike \code{.barlow_twins_data_split}, no pre-computed pairs are built
#' because the SSL variant creates views on-the-fly via augmentation.
#'
#' @param samples          \code{sits} tibble of time-series samples.
#' @param validation_split Numeric in (0, 1). Fraction held out for validation.
#'
#' @return A named list with elements \code{train} and \code{val}. Each
#'   element is itself a list with components:
#'   \describe{
#'     \item{\code{feats}}{Numeric matrix of shape
#'       \code{[n_split, n_times * n_bands]} — normalised features.}
#'   }
#'
.ssl_bt_data_split <- function(samples, validation_split) {
    # Compute normalisation statistics and build normalised feature matrix
    ml_stats <- .samples_stats(samples)
    preds    <- .pred_normalize(.predictors(samples), stats = ml_stats)
    feats    <- as.matrix(.pred_features(preds))   # [n, n_times*n_bands]

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

#' @title Torch Dataset for SSL Barlow Twins with scaling augmentation
#' @name .ssl_bt_augment_dataset
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description
#' A \code{torch::dataset} that yields one item per sample. For each sample,
#' two augmented views are created on-the-fly by multiplying the original
#' time series by independent random scalars drawn from a Gaussian
#' distribution: \code{alpha ~ N(augment_mean, sqrt(augment_variance))}.
#'
#' Each item is a list with:
#' \describe{
#'   \item{\code{x}}{A \code{float} tensor of shape
#'     \code{[2, n_times, n_bands]}.}
#'   \item{\code{y}}{A scalar zero tensor (dummy target; the Barlow Twins
#'     loss ignores it).}
#' }
#'
#' @param split             List with element \code{feats}, a numeric matrix
#'   of shape \code{[n_samples, n_times * n_bands]}.
#' @param n_times           Integer. Number of time steps in the time series.
#' @param augment_mean      Numeric. Mean of the Gaussian distribution for the
#'   scaling factor.
#' @param augment_variance  Numeric. Variance of the Gaussian distribution for
#'   the scaling factor.
#'
#' @return A \code{torch::dataset} object compatible with
#'   \code{torch::dataloader}.
#'
.ssl_bt_augment_dataset <- torch::dataset(
    name = ".SSLBTAugmentDataset",

    initialize = function(split, n_times, augment_mean, augment_variance) {
        self$feats            <- split[["feats"]]
        self$n_times          <- as.integer(n_times)
        self$n_bands          <- as.integer(ncol(split[["feats"]]) / n_times)
        self$augment_mean     <- augment_mean
        self$augment_sd       <- sqrt(augment_variance)
    },

    .getitem = function(i) {
        nt <- self$n_times
        nb <- self$n_bands

        # Reshape flat feature vector to [n_times, n_bands]
        sample_mat <- matrix(self$feats[i, ], nrow = nt, ncol = nb)

        # Draw two independent scaling factors
        alpha_a <- stats::rnorm(1L, mean = self$augment_mean,
                                sd = self$augment_sd)
        alpha_b <- stats::rnorm(1L, mean = self$augment_mean,
                                sd = self$augment_sd)

        view_a <- torch::torch_tensor(
            sample_mat * alpha_a, dtype = torch::torch_float()
        )
        view_b <- torch::torch_tensor(
            sample_mat * alpha_b, dtype = torch::torch_float()
        )

        list(
            x = torch::torch_stack(list(view_a, view_b)),
            y = torch::torch_zeros(1L, dtype = torch::torch_float())
        )
    },

    .length = function() {
        nrow(self$feats)
    }
)
