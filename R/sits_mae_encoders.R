#' @title Lightweight Temporal Attention Encoder (MAE variant)
#' @description
#' Internal implementation of the L-TAE encoder used in the MAE framework.
#' For full details on the architecture, parameters, and usage, see [sits_lighttae()].
#'
#' @return A torch module implementing the L-TAE encoder for masked autoencoding.
#'
#' @keywords internal
#' @noRd
.sits_mae_encoder_lighttae <- function(samples, n_bands, timeline) {

    light_tae_model <- torch::nn_module(
        classname = "model_ltae_encoder",

        initialize = function(n_bands,
                              timeline,
                              layers_spatial_encoder = c(32L, 64L, 128L),
                              n_heads = 16L,
                              n_neurons = c(256L, 128L),
                              dropout_rate = 0.2) {

            self$spatial_encoder <- .torch_pixel_spatial_encoder(
                n_bands = n_bands,
                layers_spatial_encoder = layers_spatial_encoder
            )
            in_channels <- layers_spatial_encoder[length(layers_spatial_encoder)]

            self$temporal_encoder <- .torch_light_temporal_attention_encoder(
                timeline     = timeline,
                in_channels  = in_channels,
                n_heads      = n_heads,
                n_neurons    = n_neurons,
                dropout_rate = dropout_rate
            )

            # Store embedding dim
            self$embedding_dim <- n_neurons[length(n_neurons)]
        },

        forward = function(input) {
            #cat("Input shape: ", paste(dim(input), collapse = " x "), "\n")
            spatial_out <- self$spatial_encoder(input)
            #cat("After spatial_encoder: ", paste(dim(spatial_out), collapse = " x "), "\n")
            temporal_out <- self$temporal_encoder(spatial_out)
            #cat("After temporal_encoder: ", paste(dim(temporal_out), collapse = " x "), "\n")
            return(temporal_out)
        }
    )
    return(light_tae_model(n_bands = n_bands, timeline = timeline))
}

#' @keywords internal
#' @noRd
.sits_mae_encoder_mlp <- function(samples, n_bands, timeline = NULL, embedding_dim = 64) {
    n_times   <- .samples_ntimes(samples)
    input_dim <- n_times * n_bands

    mlp_encoder_model <- torch::nn_module(
        classname = "torch_mlp_encoder",

        initialize = function(input_dim, embedding_dim) {
            self$embedding_dim <- embedding_dim

            self$fc1 <- .torch_linear_batch_norm_relu_dropout(
                input_dim  = input_dim,
                output_dim = 32,
                dropout_rate = 0.2
            )

            self$fc2 <- .torch_linear_batch_norm_relu_dropout(
                input_dim  =32,
                output_dim = 64,
                dropout_rate = 0.2
            )

            self$fc3 <- .torch_linear_batch_norm_relu_dropout(
                input_dim  = 64,
                output_dim = embedding_dim,
                dropout_rate = 0.2
            )
        },

        forward = function(x) {
            if (x$ndim == 3) {
                x <- x$flatten(start_dim = 2)
            }
            x <- self$fc1(x)
            x <- self$fc2(x)
            x <- self$fc3(x)
            x
        }
    )

    return(mlp_encoder_model(input_dim = input_dim, embedding_dim = embedding_dim))
}



#' @keywords internal
#' @noRd
.sits_mae_encoder_tempcnn <- function(samples, n_bands, timeline = NULL) {
    n_times <- .samples_ntimes(samples)
    sample_labels <- .samples_labels(samples)
    n_labels <- length(sample_labels)

    tempcnn_encoder_model <- torch::nn_module(
        classname = "torch_tempcnn_encoder",

        initialize = function(n_bands = n_bands,
                              n_times = n_times,
                              n_labels = n_labels,
                              kernel_sizes = c(5L, 5L, 5L),
                              hidden_dims = c(64L, 64L, 64L),
                              dropout_rates = c(0.20, 0.20, 0.20),
                              dense_layer_nodes = 64,
                              dense_layer_dropout_rate = 0.2) {

            self$hidden_dims <- hidden_dims
            self$embedding_dim <- dense_layer_nodes

            self$conv_bn_relu1 <- .torch_conv1D_batch_norm_relu_dropout(
                input_dim    = n_bands,
                output_dim   = hidden_dims[[1]],
                kernel_size  = kernel_sizes[[1]],
                padding      = as.integer(kernel_sizes[[1]] %/% 2),
                dropout_rate = dropout_rates[[1]]
            )

            self$conv_bn_relu2 <- .torch_conv1D_batch_norm_relu_dropout(
                input_dim    = hidden_dims[[1]],
                output_dim   = hidden_dims[[2]],
                kernel_size  = kernel_sizes[[2]],
                padding      = as.integer(kernel_sizes[[2]] %/% 2),
                dropout_rate = dropout_rates[[2]]
            )

            self$conv_bn_relu3 <- .torch_conv1D_batch_norm_relu_dropout(
                input_dim    = hidden_dims[[2]],
                output_dim   = hidden_dims[[3]],
                kernel_size  = kernel_sizes[[3]],
                padding      = as.integer(kernel_sizes[[3]] %/% 2),
                dropout_rate = dropout_rates[[3]]
            )

            self$flatten <- torch::nn_flatten()

            self$dense <- .torch_linear_batch_norm_relu_dropout(
                input_dim    = hidden_dims[[3]] * n_times,
                output_dim   = dense_layer_nodes,
                dropout_rate = dense_layer_dropout_rate
            )
        },

        forward = function(x) {
            # input: [batch, time, bands] -> transpose to [batch, bands, time]
            x <- x |>
                torch::torch_transpose(2, 3) |>  # [batch, bands, time]
                self$conv_bn_relu1() |>
                self$conv_bn_relu2() |>
                self$conv_bn_relu3() |>
                self$flatten() |>               # [batch, features]
                self$dense()                    # [batch, embedding_dim]
            x
        }
    )

    return(
        tempcnn_encoder_model(n_bands = n_bands, n_times = n_times, n_labels = n_labels)
    )
}
