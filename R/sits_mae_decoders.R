#' @title Multilayer Perceptron (MAE decoder variant)
#'
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
.sits_mae_decoder_mlp <- function(embedding_dim,
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
