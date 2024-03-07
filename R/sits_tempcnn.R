#' @title Train temporal convolutional neural network models
#' @name sits_tempcnn
#'
#' @author Charlotte Pelletier, \email{charlotte.pelletier@@univ-ubs.fr}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Felipe Souza, \email{lipecaso@@gmail.com}
#'
#' @description Use a TempCNN algorithm to classify data, which has
#' two stages: a 1D CNN and a  multi-layer perceptron.
#' Users can define the depth of the 1D network, as well as
#' the number of perceptron layers.
#'
#' This function is based on the paper by Charlotte Pelletier referenced below.
#' If you use this method, please cite the original tempCNN paper.
#'
#' The torch version is based on the code made available by the BreizhCrops
#' team: Marc Russwurm, Charlotte Pelletier, Marco Korner, Maximilian Zollner.
#' The original python code is available at the website
#' https://github.com/dl4sits/BreizhCrops. This code is licensed as GPL-3.
#'
#' @references Charlotte Pelletier, Geoffrey Webb and François Petitjean,
#' "Temporal Convolutional Neural Network for the Classification
#' of Satellite Image Time Series",
#' Remote Sensing, 11,523, 2019. DOI: 10.3390/rs11050523.
#'
#' @param samples            Time series with the training samples.
#' @param samples_validation Time series with the validation samples. if the
#'                           \code{samples_validation} parameter is provided,
#'                           the \code{validation_split} parameter is ignored.
#' @param cnn_layers         Number of 1D convolutional filters per layer
#' @param cnn_kernels        Size of the 1D convolutional kernels.
#' @param cnn_dropout_rates  Dropout rates for 1D convolutional filters.
#' @param dense_layer_nodes  Number of nodes in the dense layer.
#' @param dense_layer_dropout_rate  Dropout rate (0,1) for the dense layer.
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
#' @param verbose            Verbosity mode (TRUE/FALSE). Default is FALSE.
#'
#' @return A fitted model to be used for classification.
#'
#' @note
#' Please refer to the sits documentation available in
#' <https://e-sensing.github.io/sitsbook/> for detailed examples.
#' @examples
#' if (sits_run_examples()) {
#'     # create a TempCNN model
#'     torch_model <- sits_train(samples_modis_ndvi, sits_tempcnn())
#'     # plot the model
#'     plot(torch_model)
#'     # create a data cube from local files
#'     data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#'     cube <- sits_cube(
#'         source = "BDC",
#'         collection = "MOD13Q1-6",
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
sits_tempcnn <- function(samples = NULL,
                         samples_validation = NULL,
                         cnn_layers = c(256, 256, 256),
                         cnn_kernels = c(5, 5, 5),
                         cnn_dropout_rates = c(0.20, 0.20, 0.20),
                         dense_layer_nodes = 256,
                         dense_layer_dropout_rate = 0.50,
                         epochs = 150,
                         batch_size = 64,
                         validation_split = 0.2,
                         optimizer = torch::optim_adamw,
                         opt_hparams = list(
                             lr = 0.005,
                             eps = 1.0e-08,
                             weight_decay = 1.0e-06
                         ),
                         lr_decay_epochs = 1,
                         lr_decay_rate = 0.95,
                         patience = 20,
                         min_delta = 0.01,
                         verbose = FALSE) {
    # Function that trains a torch model based on samples
    train_fun <- function(samples) {
        # Avoid add a global variable for 'self'
        self <- NULL
        # Verifies if 'torch' and 'luz' packages is installed
        .check_require_packages(c("torch", "luz"))
        # Pre-conditions:
        .check_samples_train(samples)
        .check_int_parameter(param = cnn_layers, len_max = 2^31 - 1)
        .check_int_parameter(
            param = cnn_kernels, len_min = length(cnn_layers),
            len_max = length(cnn_layers)
        )
        .check_num_parameter(
            param = cnn_dropout_rates, min = 0, max = 1,
            len_min = length(cnn_layers), len_max = length(cnn_layers)
        )
        .check_int_parameter(param = dense_layer_nodes, len_max = 1)
        .check_num_parameter(
            param = dense_layer_dropout_rate, min = 0, max = 1, len_max = 1
        )
        .check_int_parameter(epochs)
        .check_int_parameter(batch_size)
        # Check validation_split parameter if samples_validation is not passed
        if (is.null(samples_validation)) {
            .check_num_parameter(
                param = validation_split, exclusive_min = 0, max = 0.5
            )
        }
        # Check opt_hparams
        # Get parameters list and remove the 'param' parameter
        optim_params_function <- formals(optimizer)[-1]
        if (!is.null(opt_hparams)) {
            .check_lst(opt_hparams, msg = "invalid 'opt_hparams' parameter")
            .check_chr_within(
                x = names(opt_hparams),
                within = names(optim_params_function),
                msg = "invalid hyperparameters provided in optimizer"
            )
            optim_params_function <- utils::modifyList(
                x = optim_params_function, val = opt_hparams
            )
        }
        # Other pre-conditions:
        .check_int_parameter(lr_decay_epochs)
        .check_num_parameter(param = lr_decay_rate, exclusive_min = 0, max = 1)
        .check_int_parameter(patience)
        .check_num_parameter(param = min_delta, min = 0)
        .check_lgl(verbose)
        # Samples labels
        labels <- .samples_labels(samples)
        # Samples bands
        bands <- .samples_bands(samples)
        # Samples timeline
        timeline <- sits_timeline(samples)
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
            sel <- !train_samples$sample_id %in% test_samples$sample_id
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
        # Set torch seed
        torch::torch_manual_seed(sample.int(10^5, 1))
        # Define the TempCNN architecture
        tcnn_model <- torch::nn_module(
            classname = "model_tcnn",
            initialize = function(n_bands,
                                  n_times,
                                  n_labels,
                                  kernel_sizes,
                                  hidden_dims,
                                  dropout_rates,
                                  dense_layer_nodes,
                                  dense_layer_dropout_rate) {
                self$hidden_dims <- hidden_dims
                # first module - transform input to hidden dims
                self$conv_bn_relu1 <- .torch_conv1D_batch_norm_relu_dropout(
                    input_dim    = n_bands,
                    output_dim   = hidden_dims[1],
                    kernel_size  = kernel_sizes[1],
                    padding      = as.integer(kernel_sizes[[1]] %/% 2),
                    dropout_rate = dropout_rates[1]
                )
                # second module - 1D CNN
                self$conv_bn_relu2 <- .torch_conv1D_batch_norm_relu_dropout(
                    input_dim    = hidden_dims[1],
                    output_dim   = hidden_dims[2],
                    kernel_size  = kernel_sizes[2],
                    padding      = as.integer(kernel_sizes[[2]] %/% 2),
                    dropout_rate = dropout_rates[2]
                )
                # third module - 1D CNN
                self$conv_bn_relu3 <- .torch_conv1D_batch_norm_relu_dropout(
                    input_dim    = hidden_dims[2],
                    output_dim   = hidden_dims[3],
                    kernel_size  = kernel_sizes[3],
                    padding      = as.integer(kernel_sizes[[3]] %/% 2),
                    dropout_rate = dropout_rates[3]
                )
                # flatten 3D tensor to 2D tensor
                self$flatten <- torch::nn_flatten()
                # create a dense tensor
                self$dense <- .torch_linear_batch_norm_relu_dropout(
                    input_dim    = hidden_dims[3] * n_times,
                    output_dim   = dense_layer_nodes,
                    dropout_rate = dense_layer_dropout_rate
                )
                # classification using softmax
                self$softmax <- torch::nn_sequential(
                    torch::nn_linear(dense_layer_nodes, n_labels),
                    torch::nn_softmax(dim = -1)
                )
            },
            forward = function(x) {
                # input is 3D n_samples x n_times x n_bands
                x <- x |>
                    torch::torch_transpose(2, 3) |>
                    self$conv_bn_relu1() |>
                    self$conv_bn_relu2() |>
                    self$conv_bn_relu3() |>
                    self$flatten() |>
                    self$dense() |>
                    self$softmax()
            }
        )
        # Train the model using luz
        torch_model <-
            luz::setup(
                module = tcnn_model,
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
                n_labels = n_labels,
                kernel_sizes = cnn_kernels,
                hidden_dims = cnn_layers,
                dropout_rates = cnn_dropout_rates,
                dense_layer_nodes = dense_layer_nodes,
                dense_layer_dropout_rate = dense_layer_dropout_rate
            ) |>
            luz::fit(
                data = list(train_x, train_y),
                epochs = epochs,
                valid_data = list(test_x, test_y),
                callbacks = list(
                    luz::luz_callback_early_stopping(
                        monitor = "valid_loss",
                        patience = patience,
                        min_delta = min_delta,
                        mode = "min"
                    ),
                    luz::luz_callback_lr_scheduler(
                        torch::lr_step,
                        step_size = lr_decay_epochs,
                        gamma = lr_decay_rate
                    )
                ),
                dataloader_options = list(batch_size = batch_size),
                verbose = verbose
            )
        # Serialize model
        serialized_model <- .torch_serialize_model(torch_model$model)

        # Function that predicts labels of input values
        predict_fun <- function(values) {
            # Verifies if torch package is installed
            .check_require_packages("torch")
            # Set torch threads to 1
            # Note: function does not work on MacOS
            suppressWarnings(torch::torch_set_num_threads(1))
            # Unserialize model
            torch_model$model <- .torch_unserialize_model(serialized_model)
            # Used to check values (below)
            input_pixels <- nrow(values)
            # Transform input into a 3D tensor
            # Reshape the 2D matrix into a 3D array
            n_samples <- nrow(values)
            n_times <- .samples_ntimes(samples)
            n_bands <- length(bands)
            # Performs data normalization
            values <- .pred_normalize(pred = values, stats = ml_stats)
            # Represent matrix values as array
            values <- array(
                data = as.matrix(values), dim = c(n_samples, n_times, n_bands)
            )
            # Transform to a torch dataset
            values <- .as_dataset(values)
            # We need to transform in a dataloader to use the batch size
            values <- torch::dataloader(
                values, batch_size = 2^15
            )
            # Do classification
            values <- stats::predict(object = torch_model, values)
            # Convert to tensor cpu to support GPU processing
            values <- torch::as_array(
                x = torch::torch_tensor(values, device = "cpu")
            )
            # Are the results consistent with the data input?
            .check_processed_values(
                values = values, input_pixels = input_pixels
            )
            # Update the columns names to labels
            colnames(values) <- labels
            return(values)
        }
        # Set model class
        predict_fun <- .set_class(
            predict_fun, "torch_model", "sits_model", class(predict_fun)
        )
        return(predict_fun)
    }
    # If samples is informed, train a model and return a predict function
    # Otherwise give back a train function to train model further
    result <- .factory_function(samples, train_fun)
    return(result)
}
