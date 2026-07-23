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
.torch_cpu_train <- function() {
    !(torch::cuda_is_available()) &&
        !(torch::backends_mps_is_available())
}
#' @title Transform matrix to torch dataset
#' @name .torch_as_dataset
#' @keywords internal
#' @noRd
#' @description Transform input data to a torch dataset.
#'
#' @param x Raw feature matrix (n_samples x n_features)
#' @param stats Training data statistics used to normalize the features.
#'              When \code{NULL} no normalization is performed.
#' @param n_times Number of time steps. When both \code{n_times} and
#'                \code{n_bands} are informed, each batch is organized as a
#'                3D array (batch, n_times, n_bands).
#' @param n_bands  Number of bands.
#'
#' @return A torch dataset
.torch_as_dataset <- torch::dataset(
    "dataset",
    initialize = function(x, stats = NULL, n_times = NULL, n_bands = NULL) {
        self$x <- x
        self$n_times <- n_times
        self$n_bands <- n_bands

        # Pre-compute normalization tensors so that the normalization
        # can be applied per batch
        if (!is.null(stats)) {
            # Compute min and max from stats
            min <- as.numeric(.stats_q02(stats))
            max <- as.numeric(.stats_q98(stats))

            # Create tensors for min and range
            self$min <- torch::torch_tensor(min)
            self$range <- torch::torch_tensor(max - min)
        }

        # Otherwise, nothing to do
        else {
            self$min <- NULL
            self$range <- NULL
        }
    },
    .getbatch = function(i) {
        # Slice a batch of raw feature values -> (batch, n_features)
        item <- torch::torch_tensor(self$x[i, , drop = FALSE])

        # Normalize on the batch tensor
        if (!is.null(self$min)) {
            item <- torch::torch_clamp(
                ((item - self$min) / self$range), min = 0.0001, max = 1.0
            )
        }

        # Organize the features as a 3D array (batch, n_times, n_bands).
        if (!is.null(self$n_times) && !is.null(self$n_bands)) {
            # The reshape + permute reproduces the layout:
            # array(values, dim = c(batch, n_times, n_bands))
            item <- item$reshape(
                c(item$shape[[1L]], self$n_bands, self$n_times)
            )$permute(c(1L, 3L, 2L))$contiguous()
        }

        # Return!
        list(item)
    },
    .getitem = function(i) {
        self$.getbatch(i)
    },
    .length = function() {
        nrow(self$x)
    }
)

#' @title Torch predict wrapper for sits models
#' @name .torch_model_wrap
#' @keywords internal
#' @noRd
#' @description Wraps a fitted torch model to enhance GPU operations.
#'
#' @param model      Trained torch module.
#' @param batch_size Maximum number of rows per forward pass.
#'
#' @return A torch module.
.torch_model_wrap <- torch::nn_module(
    ".torch_model_wrap",
    initialize = function(model, batch_size) {
        self$model <- model
        self$batch_size <- batch_size
    },
    # luz calls the `predict` method of a module when it defines one, and
    # its `forward` method otherwise (see luz:::predict.luz_module_fitted).
    # https://github.com/mlverse/luz/blob/e148679b42e16c1ed03a4c4a0c8f3358e37d8247/R/module.R#L383
    # Resolve the same function the unwrapped module would have used
    model_fn = function() {
        if (is.null(self$model$predict)) {
            return(self$model)
        }

        self$model$predict
    },
    # Run the model over row slices of at most `batch_size` rows
    forward_batched = function(model_fn, values, batch_size) {
        # Get number of rows of the input values
        n_rows <- values$shape[[1L]]
        # Define the start values of each slice
        slices_start <- seq.int(1L, n_rows, by = batch_size)
        # Predict each slice
        outputs <- purrr::map(slices_start, function(start) {
            # Slices are views over the input, so no data is copied here
            model_fn(values$narrow(
                dim = 1L,
                start = start,
                length = min(batch_size, n_rows - start + 1L)
            ))
        })
        # Concatenate list elements into one tensor
        torch::torch_cat(outputs, dim = 1L)
    },
    forward_wrapped = function(model_fn, values) {
        # Inputs that already fit go directly to the model
        if (values$shape[[1L]] <= self$batch_size) {
            return(model_fn(values))
        }
        # Get user defined batch size
        batch_size <- self$batch_size
        # Get minimum reasonable pre-defined batch size
        min_batch_size <- .conf("torch_min_batch_size")
        # Define output value
        output <- NULL
        while (is.null(output)) {
            # Try to predict the input values
            output <- .try(
                expr = {
                    self$forward_batched(model_fn, values, batch_size)
                },
                .default = function(e) {
                    # Is not out of memory error
                    is_error_oom <- .torch_error_is_oom(e)
                    # Is batch size smaller than the minimum required
                    is_batch_smaller <- batch_size <= min_batch_size
                    # Verify if is not handleable
                    if (!is_error_oom || is_batch_smaller) {
                        stop(e)
                    }
                    # Otherwise flag operation to reduce the batch size
                    NULL
                }
            )
            # If the output is null got an error that is handleable
            if (is.null(output)) {
                # In this case, to help users, we split the batch size
                batch_size <- max(batch_size %/% 2L, min_batch_size)
                # Warning users the batch size was split
                warning(
                    .conf("messages", ".torch_module_bounded"), batch_size,
                    call. = FALSE
                )
                # Clean torch allocations
                if (.torch_cuda_enabled()) {
                    torch::cuda_empty_cache()
                }
            }
        }
        # Return!
        output
    },
    predict = function(values) {
        self$forward_wrapped(self$model_fn(), values)
    }
)

#' @title Verify if the error was caused by GPU memory overflow
#' @name .torch_error_is_oom
#' @keywords internal
#' @noRd
#' @description Identifies out-of-memory error raised by torch.
#'
#' @param error Condition error object
#'
#' @return TRUE/FALSE
.torch_error_is_oom <- function(error) {
    grepl("out of memory", conditionMessage(error), ignore.case = TRUE)
}

#' @title Predict chunks in GPU
#' @name .torch_predict_chunks
#' @keywords internal
#' @noRd
#'
#' @param torch_model Torch model.
#' @param dataset     Torch dataset.
#' @param callback    Post-processing callback.
#'
#' @return A character vector with files.
.torch_predict_chunks <- function(torch_model, dataset, callback) {
    # Wrap model with custom torch module which enhances GPU handling
    torch_model$model <- .torch_model_wrap(
        model = torch_model$model,
        batch_size = sits_env[["batch_size"]]
    )
    # Define dataloader
    dataloader <- torch::dataloader(
        dataset = dataset,
        num_workers = sits_env[["multicores"]],
        batch_size = 1L
    )
    # Predict!
    stats::predict(
        object = torch_model,
        newdata = dataloader,
        callbacks = list(callback),
        stack = FALSE
    )
}

#' @title Transform matrix to torch dataset
#' @name .torch_chunk_dataset
#' @keywords internal
#' @noRd
#' @description Transform input data to a torch dataset.
#'
#' @param x Raw feature matrix (n_samples x n_features)
#' @param stats Training data statistics used to normalize the features.
#'              When \code{NULL} no normalization is performed.
#' @param n_times Number of time steps. When both \code{n_times} and
#'                \code{n_bands} are informed, each batch is organized as a
#'                3D array (batch, n_times, n_bands).
#' @param n_bands  Number of bands.
#'
#' @return A torch dataset
#'
.torch_chunks_dataset <- torch::dataset(
    "dataset",
    initialize = function(chunks = NULL,
                          tile = NULL,
                          read_fn = NULL,
                          bands = NULL,
                          base_bands = NULL,
                          stats = NULL,
                          ml_features_name = NULL,
                          impute_fn = NULL) {
        self$chunks <- chunks
        self$tile <- tile
        self$read_fn <- read_fn
        self$bands <- bands
        self$base_bands <- base_bands
        self$ml_features_name <- ml_features_name
        self$impute_fn <- impute_fn
        self$n_bands <- length(.tile_bands(tile))
        self$n_times <- length(.tile_timeline(tile))

        # Pre-compute normalization tensors per batch
        if (!is.null(stats)) {
            # Compute min and max from stats
            min <- as.numeric(.stats_q02(stats))
            max <- as.numeric(.stats_q98(stats))
            # Create tensors for min and range
            self$min <- min
            self$range <- max - min
        } else {
            self$min <- NULL
            self$range <- NULL
        }
    },
    .normalize_data = function(values) {
        # Normalize on the batch tensor
        if (!is.null(self$min)) {
            values <- torch::torch_clamp(
                ((values - self$min) / self$range), min = 0.0001, max = 1.0
            )
        }
        values
    },
    .getbatch = function(i) {
        # Get chunk
        chunk <- self$chunks[i, ]
        # Retrieve block to be processed
        block <- .block(chunk)
        # Read and preprocess values from files.
        values <- self$read_fn(
            tile = self$tile,
            block = block,
            bands = self$bands,
            base_bands = self$base_bands,
            ml_features_name = self$ml_features_name,
            impute_fn = self$impute_fn
        )
        # Get mask of NA pixels
        na_mask <- C_mask_na(values)
        # Filter out NA pixels - only classify valid pixels
        values <- values[!na_mask, , drop = FALSE]
        # Transform into matrix
        values <- as.matrix(.pred_features(values))
        # Values as tensor
        values <- torch::torch_tensor(values)
        # Normalize values
        if (!is.null(self$min)) {
            values <- self$.normalize_data(values)
        }
        # Organize the features as a 3D array (batch, n_times, n_bands)
        if (!is.null(self$n_times) && !is.null(self$n_bands)) {
            # The reshape + permute reproduces the layout:
            # array(values, dim = c(batch, n_times, n_bands))
            values <- values$reshape(
                c(values$shape[[1L]], self$n_bands, self$n_times)
            )$permute(c(1L, 3L, 2L))$contiguous()
        }
        # Generate block as a vector
        block <- c(
            unlist(block),
            xmin = .xmin(chunk),
            xmax = .xmax(chunk),
            ymin = .ymin(chunk),
            ymax = .ymax(chunk)
        )
        # Return input for the model and auxiliary values
        # for the callback
        list(
            input = values,
            na_mask = na_mask,
            block = block
        )
    },
    .getitem = function(i) {
        self$.getbatch(i)
    },
    .length = function() {
        nrow(self$chunks)
    }
)

.callback_post_encode <- luz::luz_callback(
    name = ".callback_post_encode",
    initialize = function(output_dir, out_bands, out_files, band_conf, crs,
                          emb_dims, emb_names) {
        self$output_dir <- output_dir
        self$out_bands <- out_bands
        self$out_files <- out_files
        self$band_conf <- band_conf
        self$crs <- crs
        self$emb_dims <- emb_dims
        self$emb_names <- emb_names
    },
    on_predict_batch_end = function() {
        # Get number of valid pixels
        input_pixels <- dim(ctx$input)[[1L]]
        # Get prediction as a matrix with labels
        values <- torch::as_array(ctx$pred[[length(ctx$pred)]])
        # Get auxiliary values for the callback
        na_mask <- as.logical(as.array(ctx$batch[["na_mask"]]))
        block_vec <- as.numeric(as.array(ctx$batch[["block"]]))
        # Rebuild block tibble
        block <- tibble::tibble_row(
            # spatial metadata
            col = block_vec[[1L]],
            row = block_vec[[2L]],
            ncols = block_vec[[3L]],
            nrows = block_vec[[4L]],
            # geometry
            xmin = block_vec[[5L]],
            xmax = block_vec[[6L]],
            ymin = block_vec[[7L]],
            ymax = block_vec[[8L]],
            # crs
            crs = self$crs
        )
        # Log start of block
        .debug_log(
            event = "start_block_data_encode",
            key = "model",
            value = "torch_model"
        )
        if (length(input_pixels) > 0L) {
            # apply offset
            offset <- .offset(self$band_conf)
            if (.has(offset) && offset != 0.0) {
                values <- values - offset
            }
            # apply scale
            scale <- .scale(self$band_conf)
            max_value <- .max_value(self$band_conf)
            min_value <- .min_value(self$band_conf)
            if (.has(scale) && scale != 1.0) {
                values <- values / scale
            }
            values[values > max_value] <- max_value
            values[values < min_value] <- min_value
        }
        # Log end of block
        .debug_log(
            event = "end_block_data_encode",
            key = "model",
            value = "torch_model"
        )
        full_values <- matrix(
            NA_real_,
            nrow = length(na_mask),
            ncol = self$emb_dims,
            dimnames = list(NULL, self$emb_names)
        )
        if (input_pixels > 0L) {
            full_values[!na_mask, ] <- values
        }
        # Write block
        block_file <- .encode_write_block(
            values = full_values,
            chunk = block,
            output_dir = self$output_dir,
            out_files = self$out_files,
            out_bands = self$out_bands
        )
        # Replace accumulated tensor with the block file path
        ctx$pred[[length(ctx$pred)]] <- block_file
        ctx$pred
    }

)

.callback_post_classify <- luz::luz_callback(
    name = ".callback_post_classify",
    initialize = function(output_dir, out_file, out_band, band_conf,
                          ml_labels, crs) {
        self$output_dir <- output_dir
        self$out_file <- out_file
        self$out_band <- out_band
        self$band_conf <- band_conf
        self$ml_labels <- ml_labels
        self$crs <- crs
    },
    on_predict_batch_end = function() {
        # Get number of valid pixels
        input_pixels <- dim(ctx$input)[[1L]]
        # Get prediction as a matrix with labels
        values <- torch::as_array(ctx$pred[[length(ctx$pred)]])
        colnames(values) <- self$ml_labels
        # Get auxiliary values for the callback
        na_mask <- as.logical(as.array(ctx$batch[["na_mask"]]))
        block_vec <- as.numeric(as.array(ctx$batch[["block"]]))
        # Rebuild block tibble
        block <- tibble::tibble_row(
            # spatial metadata
            col = block_vec[[1L]],
            row = block_vec[[2L]],
            ncols = block_vec[[3L]],
            nrows = block_vec[[4L]],
            # geometry
            xmin = block_vec[[5L]],
            xmax = block_vec[[6L]],
            ymin = block_vec[[7L]],
            ymax = block_vec[[8L]],
            # crs
            crs = self$crs
        )
        # Log end of block
        .debug_log(
            event = "end_block_data_classification",
            key = "model",
            value = "torch_model"
        )
        # Apply scaling to classified values
        band_scale <- .scale(self$band_conf)
        # Reconstruct full output matrix with NA for masked pixels
        n_labels <- length(self$ml_labels)
        full_values <- matrix(
            NA_real_,
            nrow = length(na_mask),
            ncol = n_labels,
            dimnames = list(NULL, self$ml_labels)
        )
        if (input_pixels > 0L) {
            # Normalize values
            values <- .ml_normalize.torch_model(values, NULL)
            # Check processed values
            .check_processed_values(
                values = values,
                input_pixels = input_pixels
            )
            # Scale values
            full_values[!na_mask, ] <- values / band_scale
        }
        # Write block
        block_file <- .classify_write_block(
            values = full_values,
            block = block,
            output_dir = self$output_dir,
            out_file = self$out_file,
            out_band = self$out_band
        )
        # Replace accumulated tensor with the block file path
        ctx$pred[[length(ctx$pred)]] <- block_file
        ctx$pred
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
