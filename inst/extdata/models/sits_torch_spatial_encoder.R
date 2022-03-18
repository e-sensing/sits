#' @title Torch module for spatial encoder
#' @name .torch_pixel_spatial_encoder
#'
#' @author Charlotte Pelletier, \email{charlotte.pelletier@@univ-ubs.fr}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Felipe Souza, \email{lipecaso@@gmail.com}
#' @keywords internal
#' @description Defines a torch module for spatial encoding.
#'
#' This function is adapted from the paper by Vivien Garnot referenced below
#' and code available on github at
#' https://github.com/VSainteuf/lightweight-temporal-attention-pytorch/blob/master/models/ltae.py.
#' If you use this method, please cite the original LTAE paper.
#'
#' There is an important difference: the model
#' proposed by Garnot assumes that the samples are available by parcel.
#' In his model, the samples from the same parcel are averaged using an MLP.
#' The current function implements an alternative to Garnot's pixel set
#' encoder for the case when only individual pixels are available.
#'
#' The spatial encoder is run for each temporal instance of the observations.
#' Thus it transforms a pixel with n bands to a pixel associated with an output
#' dimension of a linear encoder.
#'
#' The input of the PSE is a tuple of tensors of the form:
#' Pixel-Set: (Batch_size x Time Sequence Length) x Number of Bands
#'
#'  Since the input tensors have a temporal dimension, this dimension
#'  will be combined with the batch dimension so that the complete sequences
#'  are processed at once.
#'  Then the temporal dimension is separated back to produce a tensor of
#'  shape Batch_size x Time Sequence length x Embedding dimension
#'
#'
#' @references
#' Vivien Sainte Fare Garnot and Loic Landrieu,
#' "Lightweight Temporal Self-Attention
#' for Classifying Satellite Image Time Series", https://arxiv.org/abs/2007.00586
#'
#' @param n_bands           Number of bands per pixel.
#' @param hidden_dims       Output dimensions of MLPs to be created
#'
#' @return A linear tensor block.
#'

# module for spatial encoding
.torch_pixel_spatial_enconder <- torch::nn_module(
    classname = "torch_pixel_spatial_encoder",
    initialize = function(n_bands,
                          hidden_dims = c(64, 128)) {

        self$hidden_dims <- hidden_dims
        tensors <- list()

        # input layer
        tensors[[1]] <- .torch_linear_batch_norm_relu(
            input_dim = n_bands,
            output_dim = hidden_dims[1]
        )

        # if hidden layers is a vector then we add those layers
        if (length(hidden_dims) > 1) {
            for (i in 2:length(hidden_dims)) {
                tensors[[length(tensors) + 1]] <-
                    .torch_linear_batch_norm_relu(
                        input_dim = hidden_dims[i - 1],
                        output_dim = hidden_dims[i]
                    )
            }
        }
        # create a sequential module that calls the layers in the same order.
        self$mlp <- torch::nn_sequential(!!!tensors)

    },
    forward = function(input, hidden_dims) {
        batch_size <- input$shape[[1]]
        n_times    <- input$shape[[2]]
        n_bands    <-  input$shape[[3]]
        input      <- input$view(batch_size * n_times, n_bands)
        output     <- self$mlp(input)
        dim_encoder <- self$hidden_dims[[length(hidden_dims)]]
        output     <- output$view(batch_size, n_times, dim_encoder)
        return(output)
    }
)



