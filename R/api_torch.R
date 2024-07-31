#' @title Serialize torch model
#' @name .torch_serialize_model
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @keywords internal
#' @noRd
#' @description Serializes a torch model to be used in parallel processing
#' @param model        Torch model
#' @return serialized model
.torch_serialize_model <- function(model) {
    # Open raw connection
    con <- rawConnection(raw(), open = "wr")
    # Close connection on exit
    on.exit(close(con), add = TRUE)
    # Serialize and save torch model on connection
    torch::torch_save(model, con)
    # Read serialized model and return
    rawConnectionValue(con)
}
#' @title Unserialize torch model
#' @name .torch_unserialize_model
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @keywords internal
#' @noRd
#' @description Unserializes a torch model
#' @param raw     Serialized Torch model
#' @return Torch model
.torch_unserialize_model <- function(raw) {
    # Open raw connection to read model
    con <- rawConnection(raw)
    # Close connection on exit
    on.exit(close(con), add = TRUE)
    # Unserialize and load torch model from connection and return
    torch::torch_load(con)
}
#' @title Torch module for Conv1D + Batch Norm + Relu + Dropout
#' @name .torch_conv1D_batch_norm_relu_dropout
#'
#' @author Charlotte Pelletier, \email{charlotte.pelletier@@univ-ubs.fr}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Felipe Souza, \email{lipecaso@@gmail.com}
#' @keywords internal
#' @noRd
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
.torch_conv1D_batch_norm_relu_dropout <- torch::nn_module(
    classname = "conv1D_batch_norm_relu_dropout",
    initialize = function(input_dim,
                          output_dim,
                          kernel_size,
                          padding,
                          dropout_rate) {
        self$block <- torch::nn_sequential(
            torch::nn_conv1d(
                in_channels = input_dim,
                out_channels = output_dim,
                kernel_size = kernel_size,
                padding = padding
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

#' @title Torch module for Conv1D + Batch Norm + Relu
#' @name .torch_conv1D_batch_norm_relu
#'
#' @author Charlotte Pelletier, \email{charlotte.pelletier@@univ-ubs.fr}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Felipe Souza, \email{lipecaso@@gmail.com}
#' @keywords internal
#' @noRd
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
.torch_conv1D_batch_norm_relu <- torch::nn_module(
    classname = "conv1D_batch_norm_relu",
    initialize = function(input_dim,
                          output_dim,
                          kernel_size,
                          padding = 0) {
        self$block <- torch::nn_sequential(
            torch::nn_conv1d(
                in_channels = input_dim,
                out_channels = output_dim,
                kernel_size = kernel_size,
                padding = padding
            ),
            torch::nn_batch_norm1d(num_features = output_dim),
            torch::nn_relu()
        )
    },
    forward = function(x) {
        self$block(x)
    }
)

#' @title Torch module for BatchNorm + Conv1D + Batch Norm + Relu
#' @name .torch_batch_conv1D_batch_norm_relu
#'
#' @author Charlotte Pelletier, \email{charlotte.pelletier@@univ-ubs.fr}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Felipe Souza, \email{lipecaso@@gmail.com}
#' @keywords internal
#' @noRd
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
.torch_batch_conv1D_batch_norm_relu <- torch::nn_module(
    classname = "conv1D_batch_norm_relu",
    initialize = function(input_dim,
                          output_dim,
                          kernel_size,
                          padding = 0) {
        self$block <- torch::nn_sequential(
            torch::nn_batch_norm1d(num_features = input_dim),
            torch::nn_conv1d(
                in_channels = input_dim,
                out_channels = output_dim,
                kernel_size = kernel_size,
                padding = padding
            ),
            torch::nn_batch_norm1d(num_features = output_dim),
            torch::nn_relu()
        )
    },
    forward = function(x) {
        self$block(x)
    }
)
#' @title Torch module for Conv1D + Batch Norm
#' @name .torch_conv1D_batch_norm
#'
#' @author Charlotte Pelletier, \email{charlotte.pelletier@@univ-ubs.fr}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Felipe Souza, \email{lipecaso@@gmail.com}
#' @keywords internal
#' @noRd
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
.torch_conv1D_batch_norm <- torch::nn_module(
    classname = "conv1D_batch_norm",
    initialize = function(input_dim,
                          output_dim,
                          kernel_size,
                          padding = 0) {
        self$block <- torch::nn_sequential(
            torch::nn_conv1d(
                in_channels = input_dim,
                out_channels = output_dim,
                kernel_size = kernel_size,
                padding = padding
            ),
            torch::nn_batch_norm1d(num_features = output_dim)
        )
    },
    forward = function(x) {
        self$block(x)
    }
)

#' @title Torch module for linear MLP
#' @name .torch_linear_batch_norm_relu_dropout
#'
#' @author Charlotte Pelletier, \email{charlotte.pelletier@@univ-ubs.fr}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Felipe Souza, \email{lipecaso@@gmail.com}
#' @keywords internal
#' @noRd
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
                out_features = output_dim
            ),
            torch::nn_batch_norm1d(
                num_features = output_dim
            ),
            torch::nn_relu(),
            torch::nn_dropout(
                p = dropout_rate
            )
        )
    },
    forward = function(x) {
        self$block(x)
    }
)
#' @title Torch module for linear transformation with relu activation and
#' dropout
#' @name .torch_linear_relu_dropout
#'
#' @author Charlotte Pelletier, \email{charlotte.pelletier@@univ-ubs.fr}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Felipe Souza, \email{lipecaso@@gmail.com}
#' @keywords internal
#' @noRd
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
#' @noRd
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
                out_features = output_dim
            ),
            torch::nn_batch_norm1d(
                num_features = output_dim
            ),
            torch::nn_relu()
        )
    },
    forward = function(x) {
        self$block(x)
    }
)
#' @title Torch module for linear MLP
#' @name .torch_multi_linear_batch_norm_relu
#'
#' @author Charlotte Pelletier, \email{charlotte.pelletier@@univ-ubs.fr}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Felipe Souza, \email{lipecaso@@gmail.com}
#' @keywords internal
#' @noRd
#' @description Defines a set of torch modules composed of:
#' (a) linear transformation; (b) batch normalization; (c) relu activation
#'
#' @param input_dim         Input dimension of neural net.
#' @param hidden_dims       Hidden dimensions of neural net.
#'
#' @return A linear tensor block.
#'
.torch_multi_linear_batch_norm_relu <- torch::nn_module(
    classname = "torch_multi_linear_batch_norm_relu",
    initialize = function(input_dim, hidden_dims) {
        tensors <- list()
        # input layer
        tensors[[1]] <- .torch_linear_batch_norm_relu(
            input_dim = input_dim,
            output_dim = hidden_dims[[1]]
        )
        # if hidden layers is a vector then we add those layers
        if (length(hidden_dims) > 1) {
            for (i in 2:length(hidden_dims)) {
                tensors[[length(tensors) + 1]] <-
                    .torch_linear_batch_norm_relu(
                        input_dim  = hidden_dims[[i - 1]],
                        output_dim = hidden_dims[[i]]
                    )
            }
        }
        # create a sequential module that calls the layers in the same order.
        self$model <- torch::nn_sequential(!!!tensors)
    },
    forward = function(x) {
        self$model(x)
    }
)

.is_torch_model <- function(ml_model) {
    inherits(ml_model, "torch_model")
}

.torch_has_cuda <- function(){
    torch::cuda_is_available()
}

.torch_has_mps <- function(){
    torch::backends_mps_is_available()
}

.torch_mem_info <- function() {
    mem_sum <-  0

    if (.torch_has_cuda()) {
        # get current memory info in GB
        mem_sum <- torch::cuda_memory_stats()
        mem_sum <- mem_sum[["allocated_bytes"]][["all"]][["current"]] / 10^9
    }

    return(mem_sum)
}

#' @title Verify if torch works on CUDA
#' @name .torch_cuda_enabled
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @keywords internal
#' @noRd
#' @description Use CPU or GPU for torch models depending on
#' availability
#'
#' @param ml_model   ML model
#'
#' @return TRUE/FALSE
#'
.torch_cuda_enabled <- function(ml_model){
    cuda_enabled <- (
        inherits(ml_model, "torch_model") &&
        .torch_has_cuda()
    )
    return(cuda_enabled)
}
#' @title Verify if torch works on MPS
#' @name .torch_mps_enabled
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @keywords internal
#' @noRd
#' @description Use CPU or GPU for torch models depending on
#' availability
#'
#' @param ml_model   ML model
#'
#' @return TRUE/FALSE
#'
.torch_mps_enabled <- function(ml_model){
    mps_enabled <- (
        inherits(ml_model, "torch_model") &&
            .torch_has_mps()
    )
    return(mps_enabled)
}
#' @title Use GPU or CPU train for MPS Apple
#' @name .torch_cpu_train
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @keywords internal
#' @noRd
#' @description Use CPU or GPU for torch models depending on
#' availability
#'
#' @return TRUE/FALSE
#'
.torch_cpu_train <- function() {
    if (torch::cuda_is_available())
        cpu_train <-  FALSE
    else
        cpu_train <-  TRUE
    return(cpu_train)
}

.as_dataset <- torch::dataset(
    "dataset",
    initialize = function(x) {
        self$x <- x
        self$dim <- dim(x)
    },
    .getitem = function(i) {
        if (length(self$dim) == 3)
            item_data <- self$x[i,,, drop = FALSE]
        else
            item_data <- self$x[i,, drop = FALSE]

        list(torch::torch_tensor(
            array(item_data, dim = c(
                nrow(item_data), self$dim[2:length(self$dim)]
            ))
        ))
    },
    .getbatch = function(i) {
        self$.getitem(i)
    },
    .length = function() {
        dim(self$x)[[1]]
    }
)
