#' @title Build optimizer hyperparameters list
#' @name .torch_optim_params
#' @keywords internal
#' @noRd
#' @description Extracts the optimizer formals (minus the `params` argument)
#' and overrides them with any user-supplied optimizer hyperparameters.
#' @param optimizer   Torch optimizer function.
#' @param opt_hparams User-supplied optimizer hyperparameters.
#' @return Named list of optimizer hyperparameters.
.torch_optim_params <- function(optimizer, opt_hparams) {
    optim_params_function <- formals(optimizer)[-1L]
    .check_opt_hparams(opt_hparams, optim_params_function)
    optim_params_function <- utils::modifyList(
        x = optim_params_function,
        val = opt_hparams
    )
    optim_params_function
}
#' @title Set the torch random seed
#' @name .torch_set_seed
#' @keywords internal
#' @noRd
#' @description Resolves a torch seed (creating a random one if needed) and
#' sets it as the manual seed.
#' @param seed Optional integer seed.
#' @return The resolved seed (invisibly usable from the model environment).
.torch_set_seed <- function(seed) {
    torch_seed <- .torch_seed(seed)
    torch::torch_manual_seed(torch_seed)
    torch_seed
}
#' @title Build luz training callbacks
#' @name .torch_callbacks
#' @keywords internal
#' @noRd
#' @description Builds the list of `luz` callbacks. Early stopping is always
#' included; a learning-rate scheduler is added only when both decay
#' parameters are supplied.
#' @param patience         Early-stopping patience.
#' @param min_delta        Early-stopping minimum delta.
#' @param lr_decay_epochs  Step size for the LR scheduler (optional).
#' @param lr_decay_rate    Gamma for the LR scheduler (optional).
#' @return A list of `luz` callbacks.
.torch_callbacks <- function(patience, min_delta,
                             lr_decay_epochs = NULL, lr_decay_rate = NULL) {
    callbacks <- list(
        luz::luz_callback_early_stopping(
            monitor = "valid_loss",
            patience = patience,
            min_delta = min_delta,
            mode = "min"
        )
    )
    if (.has(lr_decay_epochs) && .has(lr_decay_rate)) {
        callbacks <- c(callbacks, list(
            luz::luz_callback_lr_scheduler(
                torch::lr_step,
                step_size = lr_decay_epochs,
                gamma = lr_decay_rate
            )
        ))
    }
    callbacks
}
#' @title Fit a torch model with luz
#' @name .torch_fit_model
#' @keywords internal
#' @noRd
#' @description Runs the shared `luz::setup |> set_hparams |> set_opt_hparams |>
#' fit` pipeline used by all supervised torch models. Loss is cross-entropy and
#' the tracked metric is accuracy.
#' @param module        Torch module definition.
#' @param optimizer     Torch optimizer function.
#' @param optim_params  Optimizer hyperparameters from `.torch_optim_params()`.
#' @param hparams       Named list of module hyperparameters.
#' @param arrays        Train/test arrays from `.torch_build_arrays()`.
#' @param epochs        Number of training epochs.
#' @param batch_size    Batch size.
#' @param callbacks     List of `luz` callbacks.
#' @param verbose       Whether to print training progress.
#' @param cpu_train     Whether to train on CPU.
#' @return A fitted `luz` model.
.torch_fit_model <- function(module, optimizer, optim_params, hparams,
                             arrays, epochs, batch_size, callbacks, verbose,
                             cpu_train = .torch_cpu_train()) {
    luz::setup(
        module = module,
        loss = torch::nn_cross_entropy_loss(),
        metrics = list(luz::luz_metric_accuracy()),
        optimizer = optimizer
    ) |>
        luz::set_hparams(!!!hparams) |>
        luz::set_opt_hparams(!!!optim_params) |>
        luz::fit(
            data = list(arrays[["train_x"]], arrays[["train_y"]]),
            epochs = epochs,
            valid_data = list(arrays[["test_x"]], arrays[["test_y"]]),
            callbacks = callbacks,
            accelerator = luz::accelerator(cpu = cpu_train),
            dataloader_options = list(batch_size = batch_size),
            verbose = verbose
        )
}

#' @title Torch seed
#' @name .torch_seed
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @keywords internal
#' @noRd
#'
#' @return seed for torch
#'
.torch_seed <- function(seed = NULL) {
    # If seed it is not defined, create a random one
    if (!.has(seed)) {
        seed <- sample.int(100000L, 1L)
    }
    # return
    seed
}
#' @title Serialize torch model
#' @name .torch_serialize_model
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @keywords internal
#' @noRd
#' @description Serializes a torch model to be used in parallel processing
#' @param model        Torch model
#' @return serialized model
.torch_serialize_model <- function(model) {
    # Open raw connection
    con <- rawConnection(raw(), open = "wb")
    # Close connection on exit
    on.exit(close(con), add = TRUE)
    # Serialize and save torch model on connection
    torch::torch_save(model$state_dict(), con)
    # Read serialized model and return
    rawConnectionValue(con)
}
#' @title Unserialize torch model
#' @name .torch_unserialize_model
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @keywords internal
#' @noRd
#' @description Unserializes a torch model
#' @param raw     Serialized Torch model
#' @return Torch model
.torch_unserialize_model <- function(model, raw) {
    # If model is already loaded return model
    if (.has(.try(model$device, .default = NULL))) {
        return(model)
    }
    # Open raw connection to read model
    con <- rawConnection(raw)
    # Close connection on exit
    on.exit(close(con), add = TRUE)
    # Unserialize and load torch model from connection and return
    state <- torch::torch_load(con)
    model$load_state_dict(state)
    model
}
#' @title Torch module for Conv1D + Batch Norm + Relu + Dropout
#' @name .torch_conv1D_batch_norm_relu_dropout
#'
#' @author Charlotte Pelletier, \email{charlotte.pelletier@@univ-ubs.fr}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
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
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
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
                          padding = 0L) {
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
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
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
                          padding = 0L) {
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
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
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
                          padding = 0L) {
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
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
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
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
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
    classname = "torch_linear_relu_dropout",
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
#' @name .torch_batch_norm_linear
#'
#' @author Felipe Souza, \email{lipecaso@@gmail.com}
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}

#' @keywords internal
#' @noRd
#' @description Defines a torch module composed of: (a) batch normalization; (b) linear transformation;
#'
#'
#' @param input_dim         Input dimension of neural net.
#' @param output_dim        Output dimension of neural net.
#'
#' @return A linear tensor block.
#'

# module for linear transformation with batch normalization and relu activation
.torch_batch_norm_linear <- torch::nn_module(
    classname = "torch_batch_norm_linear",
    initialize = function(input_dim,
                          output_dim) {
        self$block <- torch::nn_sequential(
            torch::nn_batch_norm1d(
                num_features = input_dim
            ),
            torch::nn_linear(
                in_features = input_dim,
                out_features = output_dim
            )
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
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
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
    classname = "torch_linear_batch_norm_relu",
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
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
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
        tensors[[1L]] <- .torch_linear_batch_norm_relu(
            input_dim = input_dim,
            output_dim = hidden_dims[[1L]]
        )
        # if hidden layers is a vector then we add those layers
        if (length(hidden_dims) > 1L) {
            for (i in 2L:length(hidden_dims)) {
                tensors[[length(tensors) + 1L]] <-
                    .torch_linear_batch_norm_relu(
                        input_dim  = hidden_dims[[i - 1L]],
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
#' @title Verify if GPU classification is available
#' @name .torch_gpu_classification
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @keywords internal
#' @noRd
#' @description Find out if CUDA or MPS are available
#'
#' @return TRUE/FALSE
#'
.torch_gpu_classification <- function() {
    torch::cuda_is_available() || torch::backends_mps_is_available()
}

#' @title Verify if CUDA is available
#' @name .torch_cuda_enabled
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @keywords internal
#' @noRd
#' @description Find out if CUDA is enabled
#'
#' @param ml_model   ML model
#'
#' @return TRUE/FALSE
#'
.torch_cuda_enabled <- function() {
    torch::cuda_is_available()
}
#' @title Use GPU or CPU training?
#' @name .torch_cpu_train
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @keywords internal
#' @noRd
#' @description Use CPU or GPU for torch models depending on
#' availability. Do not use GPU for training in Apple MPS
#' because of bug in the "luz" package
#'
#' @return TRUE/FALSE
#'
.torch_cpu_train <- function() {
    !(torch::cuda_is_available()) &&
        !(torch::backends_mps_is_available())
}
#' @title Transform matrix to torch dataset
#' @name .torch_as_dataset
#' @keywords internal
#' @noRd
#' @description Transform input data to a torch dataset
#' @param x     Input matrix
#'
#' @return A torch dataset
#'
.torch_as_dataset <- torch::dataset(
    "dataset",
    initialize = function(x) {
        self$x <- x
        self$dim <- dim(x)
    },
    .getitem = function(i) {
        if (length(self$dim) == 3L) {
            item_data <- self$x[i, , , drop = FALSE]
        } else {
            item_data <- self$x[i, , drop = FALSE]
        }

        list(torch::torch_tensor(
            array(item_data, dim = c(
                nrow(item_data), self$dim[2L:length(self$dim)]
            ))
        ))
    },
    .getbatch = function(i) {
        self$.getitem(i)
    },
    .length = function() {
        dim(self$x)[[1L]]
    }
)
#' @title Restore torch model from closure
#' @name .torch_model_restore
#' @keywords internal
#' @noRd
#' @description Restore a serialized torch model stored in a model closure.
#' @param ml_model A sits model closure.
#'
#' @return A restored torch model, or `NULL` if `torch_model` is not found.
#'
.torch_model_restore <- function(ml_model) {
    if (!.ml_is_torch_model(ml_model)) {
        return(NULL)
    }

    env <- environment(ml_model)
    torch_model <- env[["torch_model"]]

    torch_model$model <- .torch_unserialize_model(
        model = torch_model$model,
        raw = env[["serialized_model"]]
    )

    env[["torch_model"]] <- torch_model

    torch_model
}
.torch_model_to_device <- function(ml_model) {
    if (!.ml_is_torch_model(ml_model)) {
        return(invisible(NULL))
    }
    torch_model <- .ml_model(ml_model)
    if (torch::cuda_is_available()) {
        torch_model$model <- torch_model$model$to(device = "cuda")
    } else if (torch::backends_mps_is_available()) {
        torch_model$model <- torch_model$model$to(device = "mps")
    }
    env <- environment(ml_model)
    env[["torch_model"]] <- torch_model
    invisible(NULL)
}
