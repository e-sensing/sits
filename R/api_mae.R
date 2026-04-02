#' @title Masked Autoencoder Time Series Dataset
#' @name .mae_dataset
#'
#' @description
#' Internal Torch dataset for masked autoencoder (MAE) training on time
#' series. Each item returns a masked input tensor and a target structure
#' containing the original (unmasked) time series and the associated mask.
#'
#' The dataset is intended for self-supervised reconstruction: the model
#' receives \code{x} (masked input) and learns to reconstruct the original
#' signal from \code{y$y}, using \code{y$mask} to identify masked positions
#' for loss computation.
#'
#' @param masked A numeric 3D array of shape
#' \code{[n_samples, n_times, n_bands]} containing masked time series.
#' @param original A numeric 3D array of shape
#' \code{[n_samples, n_times, n_bands]} containing the corresponding
#' original (unmasked) time series.
#' @param mask A numeric 3D array of shape \code{[n_samples, n_times, 1]}
#' containing binary indicators for masked positions, where \code{1}
#' indicates masked and \code{0} indicates visible.
#'
#' @details
#' The dataset stores the provided arrays and converts slices to
#' \code{torch_tensor}s on demand in \code{.getitem}. Returned tensors are
#' reshaped to remove the leading singleton dimension introduced by
#' \code{drop = FALSE}, resulting in:
#' \itemize{
#' \item \code{x}: \code{[n_times, n_bands]}
#' \item \code{y$y}: \code{[n_times, n_bands]}
#' \item \code{y$mask}: \code{[n_times, 1]}
#' }
#'
#' The nested target structure (\code{y = list(y = ..., mask = ...)}) is
#' designed to work with dataloaders and training loops where the loss
#' function expects both the reconstruction target and the mask.
#'
#' @return A Torch dataset object compatible with \code{torch::dataloader}.
#' Calling \code{.getitem(i)} returns a list with:
#' \describe{
#' \item{x}{Masked input tensor of shape \code{[n_times, n_bands]}.}
#' \item{y}{A list with two tensors: \code{y} (original time series,
#' \code{[n_times, n_bands]}) and \code{mask} (binary mask,
#' \code{[n_times, 1]}).}
#' }
#'
#' @author Alexandre Assuncao \email{alexcarssuncao@@gmail.com}
#'
#' @keywords internal
#' @noRd
.mae_mask_index <- function(n_samples,
                            timeline,
                            mask_ratio,
                            masking_method) {
    n_times <- length(timeline)

    rlen <- max(1L, round(mask_ratio * n_times))
    rlen <- min(rlen, n_times)

    if (masking_method == "contiguous") {
        start_mask <- sample.int(n_times - rlen + 1L, n_samples, replace = TRUE)
        offsets <- unlist(lapply(start_mask, function(s) seq(s, (s + rlen - 1L))), use.names = FALSE)

        base <- rep(((seq_len(n_samples) - 1L) * n_times), each = rlen)
        masked_idx <- base + offsets
    } else {
        masked_idx <- unlist(lapply(seq_len(n_samples), function(s) {
            ((s - 1L) * n_times) + sample.int(n_times, rlen, replace = FALSE)
        }), use.names = FALSE)
    }

    mask <- matrix(0, nrow = n_samples * n_times, ncol = 1)
    mask[masked_idx, 1] <- 1

    list(
        masked_idx = masked_idx,
        mask = mask
    )
}

.mae_dataset_lazy <- torch::dataset(
    name = ".MaeDatasetLazy",

    initialize = function(samples,
                          indices,
                          stats,
                          bands,
                          timeline,
                          mask_ratio,
                          masking_method,
                          mask_value,
                          masked_bands) {
        self$samples <- samples
        self$indices <- indices
        self$stats <- stats
        self$bands <- bands
        self$timeline <- timeline
        self$mask_ratio <- mask_ratio
        self$masking_method <- masking_method
        self$mask_value <- mask_value
        self$masked_bands <- masked_bands
    },

    .getitem = function(i) {
        idx <- self$indices[i]
        data <- self$samples[idx, , drop = FALSE]

        pred <- .predictors(data)
        pred <- .pred_normalize(pred, self$stats)

        ts <- .pred_as_ts(pred, self$bands, self$timeline)

        # Original data normalized
        .ts(data) <- ts

        # Mask time-series
        masked <- .mae_mask_index(
            n_samples = length(idx),
            timeline = self$timeline,
            mask_ratio = self$mask_ratio,
            masking_method = self$masking_method
        )
        ts_masked <- ts
        ts_masked[masked$masked_idx, self$masked_bands] <- self$mask_value

        # Masked data
        data_masked <- data
        .ts(data_masked) <- ts_masked

        if (length(idx) == 1) {
            dim <- c(length(self$timeline), length(self$bands))
        } else {
            dim <- c(length(idx), length(self$timeline), length(self$bands))
        }

        x <- torch::torch_tensor(
            array(
                data = as.matrix(.pred_features(.predictors(data_masked))),
                dim = dim
            ),
            dtype = torch::torch_float()
        )

        y <- torch::torch_tensor(
            array(
                data = as.matrix(.pred_features(.predictors(data))),
                dim = dim
            ),
            dtype = torch::torch_float()
        )
        mask_dim <- if (length(idx) == 1) {
            c(length(self$timeline), 1L)
        } else {
            c(length(idx), length(self$timeline), 1L)
        }

        mask <- torch::torch_tensor(
            array(masked$mask, dim = mask_dim),
            dtype = torch::torch_float()
        )

        list(
            x = x,
            y = list(
                y = y,
                mask = mask
            )
        )
    },
    .getbatch = function(i) {
        self$.getitem(i)
    },
    .length = function() {
        length(self$indices)
    }
)

