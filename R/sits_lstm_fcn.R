#' @title Train a Long Short Term Memory Fully Convolutional Network
#' @name sits_lstm_fcn
#'
#' @author Alexandre Assuncao, \email{alexcarssuncao@@gmail.com}
#'
#' @description Uses a branched neural network consisting of
#'  a lstm (long short term memory) branch and a three-layer fully
#'  convolutional branch (FCN)
#'  followed by concatenation to classify time series data.
#'
#' This function is based on the paper by Fazle Karim, Somshubra Majumdar,
#' and Houshang Darabi. If you use this method, please cite the original
#' LSTM with FCN paper.
#'
#' The original python code is available at the website
#' \url{https://github.com/titu1994/LSTM-FCN}. This code is licensed as GPL-3.
#'
#' @references F. Karim, S. Majumdar, H. Darabi and S. Chen,
#' "LSTM Fully Convolutional Networks for Time Series Classification,"
#'  in IEEE Access, vol. 6, pp. 1662-1669, 2018,
#'  \doi{10.1109/ACCESS.2017.2779939}.
#'
#' @param samples            Time series with the training samples.
#' @param samples_validation Time series with the validation samples. if the
#'                           \code{samples_validation} parameter is provided,
#'                           the \code{validation_split} parameter is ignored.
#' @param lstm_width         Number of neurons in the lstm hidden layer.
#' @param lstm_dropout       Dropout rate of the lstm layer.
#' @param cnn_layers         Number of 1D convolutional filters per layer
#' @param cnn_kernels        Size of the 1D convolutional kernels.
#' @param epochs             Number of iterations to train the model.
#' @param batch_size         Number of samples per gradient update.
#' @param validation_split   Fraction of training data to be used for
#'                           validation.
#' @param optimizer          Optimizer function to be used.
#' @param opt_hparams        Hyperparameters for optimizer:
#'                           lr : Learning rate of the optimizer
#'                           eps: Term added to the denominator
#'                                to improve numerical stability.
#'                           weight_decay:       L2 regularization
#' @param lr_decay_epochs    Number of epochs to reduce learning rate.
#' @param lr_decay_rate      Decay factor for reducing learning rate.
#' @param patience           Number of epochs without improvements until
#'                           training stops.
#' @param min_delta	         Minimum improvement in loss function
#'                           to reset the patience counter.
#' @param seed               Seed for random values.
#' @param verbose            Verbosity mode (TRUE/FALSE). Default is FALSE.
#'
#' @return A fitted model to be used for classification.
#'
#' @examples
#' if (sits_run_examples()) {
#'     # create an LSTM model
#'     torch_model <- sits_train(
#'         samples_modis_ndvi,
#'         sits_lstm_fcn(epochs = 20, verbose = TRUE)
#'     )
#'     # plot the model
#'     plot(torch_model)
#'     # create a data cube from local files
#'     data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#'     cube <- sits_cube(
#'         source = "BDC",
#'         collection = "MOD13Q1-6.1",
#'         data_dir = data_dir
#'     )
#'     # classify a data cube
#'     probs_cube <- sits_classify(
#'         data = cube, ml_model = torch_model, output_dir = tempdir()
#'     )
#'     # plot the probability cube
#'     plot(probs_cube)
#'     # smooth the probability cube using Bayesian statistics
#'     bayes_cube <- sits_smooth(probs_cube, output_dir = tempdir())
#'     # plot the smoothed cube
#'     plot(bayes_cube)
#'     # label the probability cube
#'     label_cube <- sits_label_classification(
#'         bayes_cube,
#'         output_dir = tempdir()
#'     )
#'     # plot the labelled cube
#'     plot(label_cube)
#' }
#' @export
sits_lstm_fcn <- function(samples = NULL,
                          samples_validation = NULL,
                          cnn_layers = c(128, 256, 128),
                          cnn_kernels = c(8, 5, 3),
                          lstm_width = 8,
                          lstm_dropout = 0.8,
                          epochs = 50,
                          batch_size = 64,
                          validation_split = 0.2,
                          optimizer = torch::optim_adamw,
                          opt_hparams = list(
                              lr = 5.0e-04,
                              eps = 1.0e-08,
                              weight_decay = 1.0e-06
                          ),
                          lr_decay_epochs = 1,
                          lr_decay_rate = 0.95,
                          patience = 20,
                          min_delta = 0.01,
                          seed = NULL,
                          verbose = FALSE) {
    # set caller for error msg
    .check_set_caller("sits_lstm_fcn")
    # Verifies if 'torch' and 'luz' packages is installed
    .check_require_packages(c("torch", "luz"))
    # Function that trains a torch model based on samples
    train_fun <- function(samples, embedding_dim = NULL) {
        # does not support working with DEM or other base data
        if (inherits(samples, "sits_base")) {
            stop(.conf("messages", "sits_train_base_data"), call. = FALSE)
        }
        # Avoid add a global variable for 'self'
        self <- NULL
        # Verifies if 'torch' and 'luz' packages is installed
        .check_require_packages(c("torch", "luz"))
        # Pre-conditions:
        .check_samples_train(samples)
        .check_int_parameter(cnn_layers, len_max = 2^31 - 1)
        .check_int_parameter(cnn_kernels,
            len_min = length(cnn_layers),
            len_max = length(cnn_layers)
        )
        .check_int_parameter(lstm_width, len_max = 2^31 - 1)
        .check_num_parameter(lstm_dropout, min = 0, max = 1)
        .check_int_parameter(epochs)
        .check_int_parameter(batch_size)
        # Check validation_split parameter if samples_validation is not passed
        if (is.null(samples_validation)) {
            .check_num_parameter(validation_split, exclusive_min = 0, max = 0.5)
        }
        # Check opt_hparams
        # Get parameters list
        optim_params_function <-  .torch_optim_params(optimizer, opt_hparams)
        # Other pre-conditions:
        .check_int_parameter(lr_decay_epochs)
        .check_num_parameter(lr_decay_rate, exclusive_min = 0, max = 1)
        .check_int_parameter(patience)
        .check_num_parameter(min_delta, min = 0)
        .check_lgl_parameter(verbose)
        .check_int_parameter(seed, allow_null = TRUE)

        # Extract sample metadata and normalization statistics
        info <- .torch_sample_info(samples)
        labels <- info[["labels"]]
        bands <- info[["bands"]]
        timeline <- info[["timeline"]]
        code_labels <- info[["code_labels"]]
        n_labels <- info[["n_labels"]]
        n_bands <- info[["n_bands"]]
        n_times <- info[["n_times"]]
        ml_stats <- info[["ml_stats"]]
        # Split into (shuffled) training and test predictors
        split <- .torch_split_train_test(
            samples = samples,
            samples_validation = samples_validation,
            validation_split = validation_split,
            ml_stats = ml_stats,
            info = info
        )
        # Build 3D arrays for model training
        arrays <- .torch_build_arrays(split, info, sequential = TRUE)
        # Set torch seed (kept in the model environment for reproducibility)
        torch_seed <- .torch_set_seed(seed)
        # The LSTM/FCN for time series:
        lstm_fcn_model <- torch::nn_module(
            classname = "model_lstm_fcn",
            initialize = function(n_bands,
                                  n_times,
                                  n_labels,
                                  kernel_sizes,
                                  hidden_dims,
                                  lstm_width,
                                  lstm_dropout) {
                # Upper branch: LSTM with dimension shift
                self$lstm <- torch::nn_lstm(
                    input_size = n_times,
                    hidden_size = lstm_width,
                    dropout = 0,
                    num_layers = 1,
                    batch_first = TRUE
                )
                # lstm dropout
                self$dropout <- torch::nn_dropout(p = lstm_dropout)
                # Lower branch: Fully Convolutional Layers and avg pooling
                self$conv_bn_relu1 <- .torch_conv1D_batch_norm_relu(
                    input_dim = n_bands,
                    output_dim = hidden_dims[[1]],
                    kernel_size = kernel_sizes[[1]],
                    padding = as.integer(kernel_sizes[[1]] %/% 2)
                )
                self$conv_bn_relu2 <- .torch_conv1D_batch_norm_relu(
                    input_dim = hidden_dims[[1]],
                    output_dim = hidden_dims[[2]],
                    kernel_size = kernel_sizes[[2]],
                    padding = as.integer(kernel_sizes[[2]] %/% 2)
                )
                self$conv_bn_relu3 <- .torch_conv1D_batch_norm_relu(
                    input_dim = hidden_dims[[2]],
                    output_dim = n_bands,
                    kernel_size = kernel_sizes[[3]],
                    padding = as.integer(kernel_sizes[[3]] %/% 2)
                )
                # Global average pooling
                self$pooling <- torch::nn_adaptive_avg_pool1d(output_size = lstm_width)
                # Flattening 3D tensor to run the dense layer
                self$flatten <- torch::nn_flatten()
                # Final module: dense layer outputting the number of labels
                if (!.has(embedding_dim)) {
                    self$dense <- torch::nn_linear(
                        in_features = n_bands * lstm_width * 2,
                        out_features = n_labels
                    )
                } else {
                    self$dense <- torch::nn_linear(
                        in_features = n_bands * lstm_width * 2,
                        out_features = embedding_dim
                    )
                }
            },
            forward = function(x) {
                # dimension shift and LSTM forward pass
                x_lstm <- x$permute(c(1, 3, 2)) |>
                    self$lstm()
                # FCN forward pass
                x_fcn <- x$permute(c(1, 3, 2)) |>
                    self$conv_bn_relu1() |>
                    self$conv_bn_relu2() |>
                    self$conv_bn_relu3() |>
                    self$pooling()
                # Concatenate upper and lower branches
                x_combined <- torch::torch_cat(list(x_lstm[[1]], x_fcn), dim = 2)
                x_flat <- self$flatten(x_combined)
                x_out <- x_flat |>
                    self$dense()
            }
        )
        # return encoder model
        if (.has(embedding_dim)) {
            return(lstm_fcn_model(
                n_bands = n_bands,
                n_times = n_times,
                n_labels = length(labels),
                kernel_sizes = cnn_kernels,
                hidden_dims = cnn_layers,
                lstm_width = lstm_width,
                lstm_dropout = lstm_dropout
            ))
        }
        # Train with CPU or GPU? LSTM-FCN is incompatible with Apple MPS, so
        # only CUDA is used for GPU training (MPS falls back to CPU).
        cpu_train <- !(.torch_cuda_enabled())
        # Train the model using luz (LSTM-FCN uses only early stopping)
        torch_model <- .torch_fit_model(
            module = lstm_fcn_model,
            optimizer = optimizer,
            optim_params = optim_params_function,
            hparams = list(
                n_bands = n_bands,
                n_times = n_times,
                n_labels = length(labels),
                kernel_sizes = cnn_kernels,
                hidden_dims = cnn_layers,
                lstm_width = lstm_width,
                lstm_dropout = lstm_dropout
            ),
            arrays = arrays,
            epochs = epochs,
            batch_size = batch_size,
            callbacks = .torch_callbacks(patience, min_delta),
            verbose = verbose,
            cpu_train = cpu_train
        )
        # Remove data used for training and free memory
        rm(split, arrays)
        gc()
        # Serialize model
        serialized_model <- .torch_serialize_model(torch_model$model)

        # Function that predicts labels of input values
        # NOTE: LSTM-FCN cannot run on Apple MPS, so prediction is forced onto
        # CPU there (see the CPU branch below).
        predict_fun <- function(values) {
            # Verifies if torch package is installed
            .check_require_packages("torch")

            # Unserialize model
            torch_model$model <- .torch_unserialize_model(
                model = torch_model$model,
                raw = serialized_model
            )
            # Transform input into a 3D tensor
            # Reshape the 2D matrix into a 3D array
            n_samples <- nrow(values)
            n_times <- .samples_ntimes(samples)
            n_bands <- length(bands)
            # Performs data normalization
            values <- .pred_normalize(pred = values, stats = ml_stats)
            values <- array(
                data = as.matrix(values), dim = c(n_samples, n_times, n_bands)
            )
            # CPU or GPU classification?
            # The MPS device does not yet support non-divisible input sizes.
            # Consequently, LSTM FCN is currently incompatible with MPS and is
            # therefore disabled.
            if (.torch_cuda_enabled()) {
                # Get batch size
                batch_size <- sits_env[["batch_size"]]
                # transform the input array to a dataset
                values <- .torch_as_dataset(values)
                # Transform data set to dataloader to use the batch size
                values <- torch::dataloader(values, batch_size = batch_size)
                # GPU classification
                values <- .try(
                    stats::predict(object = torch_model, values),
                    .msg_error = .conf("messages", ".check_gpu_memory_size")
                )
            } else {
                #  CPU classification (forced using luz)
                values <- stats::predict(
                    object = torch_model,
                    newdata = values,
                    accelerator = luz::accelerator(cpu = TRUE)
                )
            }
            # Convert from tensor to array
            values <- torch::as_array(values)
            # Update the columns names to labels
            colnames(values) <- labels
            values
        }
        # Set model class
        predict_fun <- .set_class(
            predict_fun, "torch_model", "sits_model", class(predict_fun)
        )
        predict_fun
    }
    # If samples is informed, train a model and return a predict function
    # Otherwise give back a train function to train model further
    .factory_function(samples, train_fun)
}
