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
        offsets <- unlist(
            lapply(start_mask,
                   function(s) seq(s, (s + rlen - 1L))
            ), use.names = FALSE
        )

        base <- rep(((seq_len(n_samples) - 1L) * n_times), each = rlen)
        masked_idx <- base + offsets
    } else {
        masked_idx <- unlist(
            lapply(seq_len(n_samples),
                   function(s) {
                       ((s - 1L) * n_times) +
                           sample.int(n_times, rlen, replace = FALSE)
                   }
            ), use.names = FALSE
        )
    }

    mask <- matrix(0, nrow = n_samples * n_times, ncol = 1)
    mask[masked_idx, 1] <- 1

    list(
        masked_idx = masked_idx,
        mask = mask
    )
}

#' @title Lazy Torch Dataset for Masked Autoencoder Training
#' @name .mae_dataset_lazy
#'
#' @description
#' Internal \code{torch::dataset} that supplies masked/unmasked time-series
#' pairs for masked autoencoder (MAE) pre-training.  Unlike an eager dataset
#' that pre-computes all masks once, this implementation normalizes and masks
#' each sample \emph{on the fly} inside \code{.getitem}, so every epoch sees
#' a fresh set of random masks.  This acts as an implicit data augmentation
#' and helps prevent the encoder from memorising specific mask patterns.
#'
#' @param samples   A \code{sits} tibble of labelled time-series samples.
#'   The full sample table is stored; only rows selected by \code{indices}
#'   are accessed during training.
#' @param indices   Integer vector of row positions into \code{samples} that
#'   form this split (training or validation).
#' @param stats     List of normalisation statistics returned by
#'   \code{.samples_stats()}, used by \code{.pred_normalize()} to
#'   standardise each sample before masking.
#' @param bands     Character vector of band names present in
#'   \code{samples}.
#' @param timeline  \code{Date} vector giving the temporal dimension of
#'   the time series.
#' @param mask_ratio Numeric in (0, 1).  Fraction of timesteps to mask in
#'   each sample.
#' @param masking_method Character.  Either \code{"contiguous"} (a single
#'   contiguous block of timesteps is masked) or \code{"random"} (timesteps
#'   are chosen uniformly at random).
#' @param mask_value Numeric.  The fill value written into masked positions
#'   of the input tensor (typically 0).
#' @param masked_bands Character vector indicating which bands are
#'   overwritten at masked timesteps.  May be a subset of \code{bands}.
#'
#' @details
#' Each call to \code{.getitem(i)} performs the following pipeline:
#' \enumerate{
#'   \item Retrieves the sample(s) at \code{indices[i]} from the stored
#'     \code{sits} tibble.
#'   \item Normalises the predictors using the pre-computed \code{stats}.
#'   \item Generates a fresh binary mask via \code{.mae_mask_index()}.
#'   \item Overwrites the masked positions in the selected
#'     \code{masked_bands} with \code{mask_value}.
#'   \item Returns three tensors: the masked input (\code{x}), the
#'     original normalised time series (\code{y}), and the binary mask.
#' }
#'
#' When a single index is requested (\code{length(i) == 1}), the returned
#' tensors have shape \code{[n_times, n_bands]} (2-D).  When multiple
#' indices are requested via \code{.getbatch(i)}, tensors have shape
#' \code{[length(i), n_times, n_bands]} (3-D).  The mask tensor has a
#' trailing singleton band dimension (\code{1}) in both cases.
#'
#' \code{.getbatch()} delegates to \code{.getitem()}, which already handles
#' vectorised index arguments, so the dataloader can collate items
#' efficiently.
#'
#' @return A \code{torch::dataset} object compatible with
#'   \code{torch::dataloader}.  Each item returned by \code{.getitem(i)} is
#'   a named list:
#'   \describe{
#'     \item{\code{x}}{Float tensor of the masked input
#'       (\code{[n_times, n_bands]} for a single item).}
#'     \item{\code{y}}{A list containing:
#'       \describe{
#'         \item{\code{y}}{Float tensor of the original (unmasked)
#'           normalised time series (\code{[n_times, n_bands]}).}
#'         \item{\code{mask}}{Float tensor with \code{1} at masked
#'           positions and \code{0} elsewhere
#'           (\code{[n_times, 1]}).}
#'       }
#'     }
#'   }
#'
#' @author Alexandre Assuncao \email{alexcarssuncao@@gmail.com}
#'
#' @keywords internal
#' @noRd
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
#' @title Multilayer Perceptron (MAE decoder variant)
#' @name .mae_decoder_mlp
#' @description
#' Internal implementation of the MLP decoder used in the masked
#' autoencoder (MAE) workflow. The decoder maps a per-sample embedding
#' vector to a reconstructed time series with shape
#' \code{[n_times, n_bands]}.
#'
#' The module consists of a hidden block created by
#' \code{.torch_linear_batch_norm_relu_dropout()} followed by a linear
#' projection to \code{n_times * n_bands} and a final reshape to
#' \code{(batch, n_times, n_bands)}. It is intended to be used together
#' with an MAE encoder that produces embeddings of size
#' \code{embedding_dim}.
#'
#' For the public-facing description of the MLP architecture and its
#' parameters, see \code{\link{sits_mlp}}.
#'
#' @param embedding_dim Integer. Size of the input embedding vector
#' produced by the encoder (last dimension of \code{x}).
#' @param decoder_width Integer. Number of hidden units in the decoder
#' hidden layer.
#' @param n_times Integer. Number of timesteps in the reconstructed time
#' series.
#' @param n_bands Integer. Number of bands (features) per timestep in the
#' reconstructed time series.
#'
#' @details
#' Inputs to \code{forward()} are expected to be a 2D tensor of shape
#' \code{[batch, embedding_dim]}. The output is a 3D tensor of shape
#' \code{[batch, n_times, n_bands]} containing reconstructed values for
#' each timestep and band.
#'
#' This function returns an instantiated module, ready to be inserted into
#' a larger \code{torch::nn_module} graph.
#'
#' @return A Torch module implementing an MLP decoder for masked
#' autoencoding. The returned module exposes a \code{forward()} method
#' mapping \code{[batch, embedding_dim]} to
#' \code{[batch, n_times, n_bands]}.
#'
#' @author Alexandre Assuncao \email{alexcarssuncao@@gmail.com}
#'
#' @keywords internal
#' @noRd
.mae_decoder_mlp <- function(embedding_dim,
                             decoder_width,
                             n_times,
                             n_bands) {
    decoder <- torch::nn_module(
        classname = "mae_mlp_decoder",
        initialize = function(embedding_dim, decoder_width, n_times, n_bands) {
            self$n_times <- n_times
            self$n_bands <- n_bands

            # Define each layer using sits-style wrappers
            self$fc1 <- .torch_linear_batch_norm_relu_dropout(
                input_dim = embedding_dim,
                output_dim = decoder_width,
                dropout_rate = 0.2
            )
            self$fc2 <- torch::nn_linear(decoder_width, n_times * n_bands)
        },
        forward = function(x) {
            x <- self$fc1(x)
            x <- self$fc2(x)
            # reshape to (batch, n_times, n_bands)
            x <- x$view(c(-1, self$n_times, self$n_bands))
            x
        }
    )
    return(decoder(embedding_dim, decoder_width, n_times, n_bands))
}

