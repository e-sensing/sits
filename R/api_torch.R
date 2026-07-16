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
                          out_band = NULL,
                          bands = NULL,
                          base_bands = NULL,
                          stats = NULL,
                          ml_features_name = NULL,
                          ml_labels = NULL,
                          impute_fn = NULL,
                          filter_fn = NULL,
                          output_dir = NULL,
                          out_file = NULL) {
        self$chunks <- chunks
        self$tile <- tile
        self$out_band <- out_band
        self$bands <- bands
        self$base_bands <- base_bands
        self$ml_features_name <- ml_features_name
        self$ml_labels <- ml_labels
        self$impute_fn <- impute_fn
        self$filter_fn <- filter_fn
        self$output_dir <- output_dir
        self$out_file <- out_file
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
        values <- .classify_data_read(
            tile = self$tile,
            block = block,
            bands = self$bands,
            base_bands = self$base_bands,
            ml_features_name = self$ml_features_name,
            impute_fn = self$impute_fn,
            filter_fn = self$filter_fn
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

.callback_post_process <- luz::luz_callback(
    name = ".callback_post_process",
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
