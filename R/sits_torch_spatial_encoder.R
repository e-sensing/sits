#' @title Torch module for spatial encoder
#' @name torch_pixel_spatial_encoder
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
#'
#' @references
#' Vivien Sainte Fare Garnot and Loic Landrieu,
#' "Lightweight Temporal Self-Attention
#' for Classifying Satellite Image Time Series", https://arxiv.org/abs/2007.00586
#'
#' @param input_dim         Input dimension of neural net.
#' @param output_dim        Output dimension of neural net.
#'
#' @return A linear tensor block.
#'

# module for linear transformation with batch normalization and dropout
torch_pixel_spatial_enconder <- torch::nn_module(
    classname = "torch_pixel_spatial_encoder",
    initialize = function(input_dim,
                          hidden_dims) {

        tensors <- list()

        # input layer
        tensors[[1]] <- torch_linear_batch_norm_relu(
            input_dim = num_pred,
            output_dim = hidden_dims[1]
        )

        # if hidden layers is a vector then we add those layers
        if (length(hidden_dims) > 1) {
            for (i in 2:length(hidden_dims)) {
                tensors[[length(tensors) + 1]] <-
                    torch_linear_batch_norm_relu(
                        input_dim = hidden_dims[i - 1],
                        output_dim = hidden_dims[i]
                    )
            }
        }
        # create a sequential module that calls the layers in the same order.
        self$pixel_encoder <- torch::nn_sequential(!!!tensors)

    },
    forward = function(x) {
        self$pixel_encoder(x)
    }
)

#' @title Torch module for spatial encoder
#' @name torch_pixel_positional_encoder
#'
#' @author Charlotte Pelletier, \email{charlotte.pelletier@@univ-ubs.fr}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Felipe Souza, \email{lipecaso@@gmail.com}
#' @keywords internal
#' @description Defines a torch module for positional encoding based on the
#' work by Vivien Garnot.
#'
#' This function part of the implementation of the paper by Vivien Garnot
#' referenced below. The particular implementation of the positional encoder
#'
#' We used the code made available by Maja Schneider in her work with
#' Marco Körner referenced below and available at
#' https://github.com/maja601/RC2020-psetae.
#'
#' @references
#' Vivien Sainte Fare Garnot and Loic Landrieu,
#' "Lightweight Temporal Self-Attention
#' for Classifying Satellite Image Time Series", https://arxiv.org/abs/2007.00586
#'
#' @param input_dim         Input dimension of neural net.
#' @param output_dim        Output dimension of neural net.
#'
#' @return A linear tensor block.
#'

# # Positional Encoder
# positional_encoding <- torch::nn_module(
#     classname = "positional_encoding",
#     # dates is a vector with the number of days since
#     # the first observation
#     initialize = function(d_e = 128, dates){
#
#         # length of positional encoder is the length of dates vector
#         max_len <- length(dates)
#         # keep the dates vector
#         self$dates <- dates
#         # initialize the torch 'days' tensor
#         days <- torch::torch_tensor(dates)
#         days <- torch::torch_unsqueeze(days, 2)
#
#         # Calculate the positional encoding p
#         p <- torch::torch_zeros(max_len, d_e)
#         div_term <-  torch::torch_exp(torch::torch_arange(0, d_e, 2)
#                                       * (-log(1000.0) / d_e))
#         p <-  torch::torch_sin(days * div_term)
#         p[:, 1::2] = torch.cos(days * div_term)
#         p = p.unsqueeze(0)
#         self.register_buffer('p', p)
#     }
# )
#
#
# def forward(self, x):
#     x = x + self.p
# return x
