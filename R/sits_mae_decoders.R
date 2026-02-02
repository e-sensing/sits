

#' @title Multilayer Perceptron (MAE decoder variant)
#' @author Alexandre Assuncao \email{alexcarssuncao@@gmail.com}
#' @description
#' Internal implementation of the MLP decoder used in the MAE framework.
#' For full details on the architecture, parameters, and usage, see [sits_mlp()].
#'
#' @return A torch module implementing the MLP decoder for masked autoencoding.
#'
#' @keywords internal
#' @noRd
.sits_mae_decoder_mlp <- torch::nn_module(
    classname = "mae_mlp_decoder",

    initialize = function(embedding_dim, n_times, n_bands) {
        self$n_times <- n_times
        self$n_bands <- n_bands

        # Define each layer using sits-style wrappers
        self$fc1 <- .torch_linear_batch_norm_relu_dropout(
            input_dim  = embedding_dim,
            output_dim = 128,
            dropout_rate = 0.2
        )

        self$fc2 <- torch::nn_linear(128, n_times * n_bands)
    },

    forward = function(x) {
        x <- self$fc1(x)
        x <- self$fc2(x)
        x <- x$view(c(-1, self$n_times, self$n_bands))  # reshape to (batch, n_times, n_bands)
        x
    }
)

#' @title Linear Decoder (MAE decoder variant)
#' @author Alexandre Assuncao \email{alexcarssuncao@@gmail.com}
#' @description
#' Internal implementation of the linear decoder used in the MAE framework.
#' For full details on the architecture, parameters, and usage, see [sits_mlp()].
#'
#' @return A torch module implementing the linear decoder for masked autoencoding.
#'
#' @keywords internal
#' @noRd
.sits_mae_decoder_linear <- torch::nn_module(
    classname = "mae_linear_decoder",

    initialize = function(embedding_dim, n_times, n_bands) {
        self$n_times <- n_times
        self$n_bands <- n_bands
        self$fc <- torch::nn_linear(embedding_dim, n_times * n_bands)
    },

    forward = function(x) {
        x <- self$fc(x)
        x <- x$view(c(-1, self$n_times, self$n_bands))  # reshape to (batch, n_times, n_bands)
        x
    }
)
