#' @title Torch module for Conv1D + Batch Norm + Relu + Dropout
#' @name .torch_conv1D_batch_norm_relu_dropout
#'
#' @author Charlotte Pelletier, \email{charlotte.pelletier@@univ-ubs.fr}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Felipe Souza, \email{lipecaso@@gmail.com}
#' @keywords internal
#' @description Defines a torch conv1d module composed of:
#' (a) 1d convolution; (b) batch normalization;
#' (c) relu activation; (d) dropout;
#'
#' @param input_dim         Input dimension of neural net.
#' @param output_dim        Output dimension of neural net.
#' @param kernel_size       Size of 1D convolutional kernel.
#' @param padding           Padding added to both sides of the input.
#' @param dropout_rate      Dropout rate for linear module.
#'
#' @return A conv1D tensor block.
#'
# module for 1D convolution with batch normalization and dropout
.torch_conv1D_batch_norm_relu_dropout <- function() {
    torch::nn_module(
        classname = "conv1D_batch_norm_relu_dropout",
        initialize = function(input_dim,
                              output_dim,
                              kernel_size,
                              padding = 0,
                              dropout_rate) {
            self$block <- torch::nn_sequential(
                torch::nn_conv1d(in_channels  = input_dim,
                                 out_channels = output_dim,
                                 kernel_size  = kernel_size,
                                 padding      = padding
                ),
                torch::nn_batch_norm1d(num_features = output_dim),
                torch::nn_relu(),
                torch::nn_dropout(p = dropout_rate)
            )
        },
        forward = function(x) {
            self$block(x)
        }
    )
}
#' @title Torch module for Conv1D + Batch Norm + Relu
#' @name .torch_conv1D_batch_norm_relu
#'
#' @author Charlotte Pelletier, \email{charlotte.pelletier@@univ-ubs.fr}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Felipe Souza, \email{lipecaso@@gmail.com}
#' @keywords internal
#' @description Defines a torch conv1d module composed of:
#' (a) 1d convolution; (b) batch normalization;
#' (c) relu activation
#'
#' @param input_dim         Input dimension of neural net.
#' @param output_dim        Output dimension of neural net.
#' @param kernel_size       Size of 1D convolutional kernel.
#' @param padding           Padding added to both sides of the input.
#'
#' @return A conv1D tensor block.
#'
# module for 1D convolution with batch normalization and dropout
.torch_conv1D_batch_norm_relu <- function() {
    torch::nn_module(
        classname = "conv1D_batch_norm_relu",
        initialize = function(input_dim,
                              output_dim,
                              kernel_size,
                              padding = 0) {
            self$block <- torch::nn_sequential(
                torch::nn_conv1d(in_channels  = input_dim,
                                 out_channels = output_dim,
                                 kernel_size  = kernel_size,
                                 padding      = padding,
                ),
                torch::nn_batch_norm1d(num_features = output_dim),
                torch::nn_relu()
            )
        },
        forward = function(x) {
            self$block(x)
        }
    )
}
#' @title Torch module for BatchNorm + Conv1D + Batch Norm + Relu
#' @name .torch_batch_conv1D_batch_norm_relu
#'
#' @author Charlotte Pelletier, \email{charlotte.pelletier@@univ-ubs.fr}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Felipe Souza, \email{lipecaso@@gmail.com}
#' @keywords internal
#' @description Defines a torch conv1d module composed of:
#' (a) 1d convolution; (b) batch normalization;
#' (c) relu activation
#'
#' @param input_dim         Input dimension of neural net.
#' @param output_dim        Output dimension of neural net.
#' @param kernel_size       Size of 1D convolutional kernel.
#' @param padding           Padding added to both sides of the input.
#'
#' @return A conv1D tensor block.
#'
# module for 1D convolution with batch normalization and dropout
.torch_batch_conv1D_batch_norm_relu <- function() {
    torch::nn_module(
        classname = "conv1D_batch_norm_relu",
        initialize = function(input_dim,
                              output_dim,
                              kernel_size,
                              padding = 0) {
            self$block <- torch::nn_sequential(
                torch::nn_batch_norm1d(num_features = input_dim),
                torch::nn_conv1d(in_channels  = input_dim,
                                 out_channels = output_dim,
                                 kernel_size  = kernel_size,
                                 padding      = padding
                ),
                torch::nn_batch_norm1d(num_features = output_dim),
                torch::nn_relu()
            )
        },
        forward = function(x) {
            self$block(x)
        }
    )
}
#' @title Torch module for Conv1D + Batch Norm
#' @name .torch_conv1D_batch_norm
#'
#' @author Charlotte Pelletier, \email{charlotte.pelletier@@univ-ubs.fr}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Felipe Souza, \email{lipecaso@@gmail.com}
#' @keywords internal
#' @description Defines a torch conv1d module composed of:
#' (a) 1d convolution; (b) batch normalization.
#'
#' @param input_dim         Input dimension of neural net.
#' @param output_dim        Output dimension of neural net.
#' @param kernel_size       Size of 1D convolutional kernel.
#' @param padding           Padding added to both sides of the input.
#'
#' @return A conv1D tensor block.
#'
# module for 1D convolution with batch normalization and dropout
.torch_conv1D_batch_norm <- function() {
    torch::nn_module(
        classname = "conv1D_batch_norm",
        initialize = function(input_dim,
                              output_dim,
                              kernel_size,
                              padding = 0) {
            self$block <- torch::nn_sequential(
                torch::nn_conv1d(in_channels  = input_dim,
                                 out_channels = output_dim,
                                 kernel_size  = kernel_size,
                                 padding      = padding,
                ),
                torch::nn_batch_norm1d(num_features = output_dim),
            )
        },
        forward = function(x) {
            self$block(x)
        }
    )
}
