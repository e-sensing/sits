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
#'
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
    train_fun <- function(samples) {
        # does not support working with DEM or other base data
        if (inherits(samples, "sits_base"))
            stop(.conf("messages", "sits_train_base_data"), call. = FALSE)
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
        # Get parameters list and remove the 'param' parameter
        optim_params_function <- formals(optimizer)[-1]
        if (!is.null(opt_hparams)) {
            .check_lst_parameter(opt_hparams,
                                 msg = .conf("messages", ".check_opt_hparams")
            )
            .check_chr_within(
                x = names(opt_hparams),
                within = names(optim_params_function),
                msg = .conf("messages", ".check_opt_hparams")
            )
            optim_params_function <- utils::modifyList(
                x = optim_params_function, val = opt_hparams
            )
        }
        # Other pre-conditions:
        .check_int_parameter(lr_decay_epochs)
        .check_num_parameter(lr_decay_rate, exclusive_min = 0, max = 1)
        .check_int_parameter(patience)
        .check_num_parameter(min_delta, min = 0)
        .check_lgl_parameter(verbose)
        .check_int_parameter(seed, allow_null = TRUE)

        # Samples labels
        labels <- .samples_labels(samples)
        # Samples bands
        bands <- .samples_bands(samples)
        # Samples timeline
        timeline <- .samples_timeline(samples)
        # Create numeric labels vector
        code_labels <- seq_along(labels)
        names(code_labels) <- labels
        # Number of labels, bands, and number of samples (used below)
        n_labels <- length(labels)
        n_bands <- length(bands)
        n_times <- .samples_ntimes(samples)
        # Data normalization
        ml_stats <- .samples_stats(samples)
        train_samples <- .predictors(samples)
        train_samples <- .pred_normalize(pred = train_samples, stats = ml_stats)
        # Post condition: is predictor data valid?
        .check_predictors(pred = train_samples, samples = samples)
        # Are there validation samples?
        if (!is.null(samples_validation)) {
            .check_samples_validation(
                samples_validation = samples_validation, labels = labels,
                timeline = timeline, bands = bands
            )
            # Test samples are extracted from validation data
            test_samples <- .predictors(samples_validation)
            test_samples <- .pred_normalize(
                pred = test_samples, stats = ml_stats
            )
        } else {
            # Split the data into training and validation data sets
            # Create partitions different splits of the input data
            test_samples <- .pred_sample(
                pred = train_samples, frac = validation_split
            )
            # Remove the lines used for validation
            sel <- !train_samples[["sample_id"]] %in%
                test_samples[["sample_id"]]
            train_samples <- train_samples[sel, ]
        }
        n_samples_train <- nrow(train_samples)
        n_samples_test <- nrow(test_samples)
        # Shuffle the data
        train_samples <- train_samples[sample(
            nrow(train_samples), nrow(train_samples)
        ), ]
        test_samples <- test_samples[sample(
            nrow(test_samples), nrow(test_samples)
        ), ]
        # Organize data for model training
        train_x <- array(
            data = as.matrix(.pred_features(train_samples)),
            dim = c(n_samples_train, n_times, n_bands)
        )
        train_y <- unname(code_labels[.pred_references(train_samples)])
        # Create the test data
        test_x <- array(
            data = as.matrix(.pred_features(test_samples)),
            dim = c(n_samples_test, n_times, n_bands)
        )
        test_y <- unname(code_labels[.pred_references(test_samples)])
        # Create a torch seed (we define a new variable to allow users
        # to access this seed number from the model environment)
        torch_seed <- .torch_seed(seed)
        # Set torch seed
        torch::torch_manual_seed(torch_seed)
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
                self$dense <- torch::nn_linear(
                    in_features = n_bands * lstm_width * 2,
                    out_features = n_labels
                )
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
        # train with CPU or GPU?
        if (torch::cuda_is_available())
            cpu_train <- FALSE
        else
            cpu_train <- TRUE
        # Train the model using luz
        torch_model <-
            luz::setup(
                module = lstm_fcn_model,
                loss = torch::nn_cross_entropy_loss(),
                metrics = list(luz::luz_metric_accuracy()),
                optimizer = optimizer
            ) |>
            luz::set_opt_hparams(
                !!!optim_params_function
            ) |>
            luz::set_hparams(
                n_bands = n_bands,
                n_times = n_times,
                n_labels = length(labels),
                kernel_sizes = cnn_kernels,
                hidden_dims = cnn_layers,
                lstm_width = lstm_width,
                lstm_dropout = lstm_dropout
            ) |>
            luz::fit(
                data = list(train_x, train_y),
                epochs = epochs,
                valid_data = list(test_x, test_y),
                callbacks = list(
                    luz::luz_callback_early_stopping(
                        patience = patience,
                        min_delta = min_delta
                    )
                ),
                accelerator = luz::accelerator(cpu = cpu_train),
                dataloader_options = list(batch_size = batch_size),
                verbose = verbose
            )
        # Serialize model
        serialized_model <- .torch_serialize_model(torch_model[["model"]])

        # Function that predicts labels of input values
        predict_fun <- function(values) {
            # Verifies if torch package is installed
            .check_require_packages("torch")
            # Set torch threads to 1
            suppressWarnings(torch::torch_set_num_threads(1L))
            # Unserialize model
            torch_model[["model"]] <- .torch_unserialize_model(serialized_model)
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
            if (
                .torch_gpu_classification() &&
                !torch::backends_mps_is_available()
            ) {
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
