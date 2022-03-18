#' @title Torch module for linear MLP
#' @name .torch_linear_batch_norm_relu_dropout
#'
#' @author Charlotte Pelletier, \email{charlotte.pelletier@@univ-ubs.fr}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Felipe Souza, \email{lipecaso@@gmail.com}
#' @keywords internal
#' @description Defines a torch module composed of; (a) linear transformation;
#' (b) batch normalization; (c) relu activation; (d) dropout
#'
#' @param input_dim         Input dimension of neural net.
#' @param output_dim        Output dimension of neural net.
#' @param dropout_rate      Dropout rate for linear module.
#'
#' @return A linear tensor block.
#'

# module for linear transformation with batch normalization and dropout
.torch_linear_batch_norm_relu_dropout <- torch::nn_module(
    classname = "torch_linear_batch_norm_relu_dropout",
    initialize = function(input_dim,
                          output_dim,
                          dropout_rate) {
        self$block <- torch::nn_sequential(
            torch::nn_linear(
                in_features = input_dim,
                out_features = output_dim),
            torch::nn_batch_norm1d(
                num_features = output_dim),
            torch::nn_relu(),
            torch::nn_dropout(
                p = dropout_rate)
        )
    },
    forward = function(x) {
        self$block(x)
    }
)
#' @title Torch module for linear transformation with relu activation and dropout
#' @name .torch_linear_relu_dropout
#'
#' @author Charlotte Pelletier, \email{charlotte.pelletier@@univ-ubs.fr}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Felipe Souza, \email{lipecaso@@gmail.com}
#' @keywords internal
#' @description Defines a torch module composed of; (a) linear transformation;
#' (b) relu activation; (c) dropout
#'
#' @param input_dim         Input dimension of neural net.
#' @param output_dim        Output dimension of neural net.
#' @param dropout_rate      Dropout rate for linear module.
#'
#' @return A linear tensor block.
#'
#
.torch_linear_relu_dropout <- torch::nn_module(
    classname = "torch_linear_batch_norm_relu_dropout",
    initialize = function(input_dim,
                          output_dim,
                          dropout_rate) {
        self$block <- torch::nn_sequential(
            torch::nn_linear(input_dim, output_dim),
            torch::nn_relu(),
            torch::nn_dropout(dropout_rate)
        )
    },
    forward = function(x) {
        self$block(x)
    }
)
#' @title Torch module for linear MLP
#' @name .torch_linear_batch_norm_relu
#'
#' @author Charlotte Pelletier, \email{charlotte.pelletier@@univ-ubs.fr}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Felipe Souza, \email{lipecaso@@gmail.com}
#' @keywords internal
#' @description Defines a torch module composed of; (a) linear transformation;
#' (b) batch normalization; (c) relu activation
#'
#' @param input_dim         Input dimension of neural net.
#' @param output_dim        Output dimension of neural net.
#'
#' @return A linear tensor block.
#'

# module for linear transformation with batch normalization and relu activation
.torch_linear_batch_norm_relu <- torch::nn_module(
    classname = "torch_linear_batch_norm_relu_dropout",
    initialize = function(input_dim,
                          output_dim) {
        self$block <- torch::nn_sequential(
            torch::nn_linear(
                in_features = input_dim,
                out_features = output_dim),
            torch::nn_batch_norm1d(
                num_features = output_dim),
            torch::nn_relu()
        )
    },
    forward = function(x) {
        self$block(x)
    }
)
