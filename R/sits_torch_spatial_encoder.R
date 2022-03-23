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
#' @param n_bands             Number of bands per pixel.
#' @param layers_mlp2         Layers of MLP2 spatial encoder
#'
#' @return A linear tensor block.
#'

# module for spatial encoding
.torch_pixel_spatial_enconder <- torch::nn_module(
    classname = "torch_pixel_spatial_encoder",
    initialize = function(n_bands,
                          layers_mlp2 = c(32, 64, 128)) {

        self$layers_mlp2 <- layers_mlp2
        self$mlp2 <- .torch_multi_linear_batch_norm_relu(
            input_dim = n_bands,
            hidden_dim = layers_mlp2
        )
    },
    forward = function(input) {
        # batch size is the first dimension of the input tensor
        batch_size <- input$shape[[1]]
        # n_times is the second dimension
        n_times    <- input$shape[[2]]
        # n_bands is the third dimension
        n_bands    <- input$shape[[3]]
        # reshape the input
        # from a 3D shape [batch_size, n_times, n_bands]
        # to a 2D shape [(batch_size * n_times), n_bands]
        input      <- input$view(c(batch_size * n_times, n_bands))
        # run the the 2D shape by a multi-layer perceptron
        # input is 2D shape [(batch_size * n_times), n_bands]
        dim_enc    <- self$layers_mlp2[[length(self$layers_mlp2)]]
        output     <- self$mlp2(input)
        # output is a 2D shape[(batch_size * n_times), dim_enc]
        # reshape the output
        # from a 2D shape [(batch_size * n_times), n_bands]
        # to a 3D shape [batch_size, n_times, dim_enc]
        output     <- output$view(c(batch_size, n_times, dim_enc))
        return(output)
    }
)



