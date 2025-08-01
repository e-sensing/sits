#' @title Train temporal convolutional neural network models
#' @name sits_tempcnn
#'
#' @author Charlotte Pelletier, \email{charlotte.pelletier@@univ-ubs.fr}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @author Felipe Souza, \email{lipecaso@@gmail.com}
#'
#' @description Use a TempCNN algorithm to classify data, which has
#' two stages: a 1D CNN and a  multi-layer perceptron.
#' Users can define the depth of the 1D network, as well as
#' the number of perceptron layers.
#'
#' @note
#' \code{sits} provides a set of default values for all classification models.
#' These settings have been chosen based on testing by the authors.
#' Nevertheless, users can control all parameters for each model.
#' Novice users can rely on the default values,
#' while experienced ones can fine-tune deep learning models
#' using \code{\link[sits]{sits_tuning}}.
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
#'     torch_model <- sits_train(
#'         samples_modis_ndvi,
#'         sits_tempcnn(epochs = 20, verbose = TRUE)
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
sits_tempcnn <- function(samples = NULL,
                         samples_validation = NULL,
                         cnn_layers = c(64L, 64L, 64L),
                         cnn_kernels = c(5L, 5L, 5L),
                         cnn_dropout_rates = c(0.20, 0.20, 0.20),
                         dense_layer_nodes = 256L,
                         dense_layer_dropout_rate = 0.50,
                         epochs = 150L,
                         batch_size = 64L,
                         validation_split = 0.2,
                         optimizer = torch::optim_adamw,
                         opt_hparams = list(
                             lr = 5.0e-04,
                             eps = 1.0e-08,
                             weight_decay = 1.0e-06
                         ),
                         lr_decay_epochs = 1L,
                         lr_decay_rate = 0.95,
                         patience = 20L,
                         min_delta = 0.01,
                         verbose = FALSE) {
    # set caller for error msg
    .check_set_caller("sits_tempcnn")
    # Verifies if 'torch' and 'luz' packages is installed
    .check_require_packages(c("torch", "luz"))
    # documentation mode? verbose is FALSE
    verbose <- .message_verbose(verbose)
    # Function that trains a torch model based on samples
    train_fun <- function(samples) {
        # does not support working with DEM or other base data
        if (inherits(samples, "sits_base")) {
            stop(.conf("messages", "sits_train_base_data"), call. = FALSE)
        }
        # Avoid add a global variable for 'self'
        self <- NULL
        # Check validation_split parameter if samples_validation is not passed
        if (is.null(samples_validation)) {
            .check_num_parameter(validation_split, exclusive_min = 0.0, max = 0.5)
        }
        # Preconditions
        .check_pre_sits_tempcnn(
            samples = samples, cnn_layers = cnn_layers,
            cnn_kernels = cnn_kernels,
            cnn_dropout_rates = cnn_dropout_rates,
            dense_layer_nodes = dense_layer_nodes,
            dense_layer_dropout_rate = dense_layer_dropout_rate,
            epochs = epochs, batch_size = batch_size,
            lr_decay_epochs = lr_decay_epochs,
            lr_decay_rate = lr_decay_rate,
            patience = patience, min_delta = min_delta,
            verbose = verbose
        )
        # Check opt_hparams
        # Get parameters list and remove the 'param' parameter
        optim_params_function <- formals(optimizer)[-1L]
        .check_opt_hparams(opt_hparams, optim_params_function)
        optim_params_function <- utils::modifyList(
            x = optim_params_function,
            val = opt_hparams
        )
        # Sample labels
        sample_labels <- .samples_labels(samples)
        # Sample bands
        bands <- .samples_bands(samples)
        # Sample timeline
        timeline <- .samples_timeline(samples)
        # Create numeric labels vector
        code_labels <- seq_along(sample_labels)
        names(code_labels) <- sample_labels
        # Number of labels, bands, and number of samples (used below)
        n_labels <- length(sample_labels)
        n_bands <- length(bands)
        n_times <- .samples_ntimes(samples)
        # Data normalization
        ml_stats <- .samples_stats(samples)

        # Organize train and the test data
        train_test_data <- .torch_train_test_samples(
            samples = samples,
            samples_validation = samples_validation,
            ml_stats = ml_stats,
            labels = sample_labels,
            code_labels = code_labels,
            timeline = timeline,
            bands = bands,
            validation_split = validation_split
        )
        # Obtain the train and the test data
        train_samples <- train_test_data[["train_samples"]]
        test_samples <- train_test_data[["test_samples"]]

        # Organize data for model training
        n_samples_train <- nrow(train_samples)
        n_samples_test <- nrow(test_samples)
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
        torch::torch_manual_seed(sample.int(100000L, 1L))
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
                    output_dim   = hidden_dims[[1L]],
                    kernel_size  = kernel_sizes[[1L]],
                    padding      = as.integer(kernel_sizes[[1L]] %/% 2L),
                    dropout_rate = dropout_rates[[1L]]
                )
                # second module - 1D CNN
                self$conv_bn_relu2 <- .torch_conv1D_batch_norm_relu_dropout(
                    input_dim    = hidden_dims[[1L]],
                    output_dim   = hidden_dims[[2L]],
                    kernel_size  = kernel_sizes[[2L]],
                    padding      = as.integer(kernel_sizes[[2L]] %/% 2L),
                    dropout_rate = dropout_rates[[2L]]
                )
                # third module - 1D CNN
                self$conv_bn_relu3 <- .torch_conv1D_batch_norm_relu_dropout(
                    input_dim    = hidden_dims[[2L]],
                    output_dim   = hidden_dims[[3L]],
                    kernel_size  = kernel_sizes[[3L]],
                    padding      = as.integer(kernel_sizes[[3L]] %/% 2L),
                    dropout_rate = dropout_rates[[3L]]
                )
                # flatten 3D tensor to 2D tensor
                self$flatten <- torch::nn_flatten()
                # create a dense tensor
                self$dense <- .torch_linear_batch_norm_relu_dropout(
                    input_dim    = hidden_dims[[3L]] * n_times,
                    output_dim   = dense_layer_nodes,
                    dropout_rate = dense_layer_dropout_rate
                )
                # reduce to linear tensor with n_labels
                # softmax is done externally
                self$nn_linear <- torch::nn_sequential(
                    torch::nn_linear(dense_layer_nodes, n_labels)
                )
            },
            forward = function(x) {
                # input is 3D n_samples x n_times x n_bands
                x <- x |>
                    torch::torch_transpose(2L, 3L) |>
                    self$conv_bn_relu1() |>
                    self$conv_bn_relu2() |>
                    self$conv_bn_relu3() |>
                    self$flatten() |>
                    self$dense() |>
                    self$nn_linear()
            }
        )
        # train with CPU or GPU?
        cpu_train <- .torch_cpu_train()
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
            # Note: function does not work on MacOS
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
            # Represent matrix values as array
            values <- array(
                data = as.matrix(values), dim = c(n_samples, n_times, n_bands)
            )
            # GPU or CPU classification?
            if (.torch_gpu_classification()) {
                # Get batch size
                batch_size <- sits_env[["batch_size"]]
                # Transform the input array to a dataset
                values <- .torch_as_dataset(values)
                # Transform to dataloader to use the batch size
                values <- torch::dataloader(values, batch_size = batch_size)
                # Do GPU classification
                values <- .try(
                    stats::predict(object = torch_model, values),
                    .msg_error = .conf("messages", ".check_gpu_memory_size")
                )
            } else {
                # Do CPU classification
                values <- stats::predict(object = torch_model, values)
            }
            # Convert from tensor to array
            values <- torch::as_array(values)
            # Update the columns names to labels
            colnames(values) <- sample_labels
            values
        }
        # Set model class
        predict_fun <- .set_class(
            predict_fun, "torch_model", "sits_model", class(predict_fun)
        )
    }
    # If samples is informed, train a model and return a predict function
    # Otherwise give back a train function to train model further
    .factory_function(samples, train_fun)
}
