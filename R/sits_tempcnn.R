
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
#' @return A fitted model to be passed to \code{\link[sits]{sits_classify}}
#'
#' @examples
#' if (sits_active_tests()) {
#' # Retrieve the set of samples for the Mato Grosso (provided by EMBRAPA)
#'
#' # Build a machine learning model based on deep learning
#' tc_model <- sits_train(samples_modis_4bands, sits_tempcnn())
#' # Plot the model
#' plot(tc_model)
#'
#' # get a point and classify the point with the ml_model
#' point <- sits_select(point_mt_6bands, bands = c("NDVI", "EVI", "NIR", "MIR"))
#' class <- sits_classify(point, tc_model)
#'
#' plot(class, bands = c("NDVI", "EVI"))
#' }
#'
#' @export
sits_tempcnn <- function(samples = NULL,
                         samples_validation = NULL,
                         cnn_layers = c(64, 64, 64),
                         cnn_kernels = c(5, 5, 5),
                         cnn_dropout_rates = c(0.50, 0.50, 0.50),
                         dense_layer_nodes = 256,
                         dense_layer_dropout_rate = 0.50,
                         epochs = 150,
                         batch_size = 128,
                         validation_split = 0.2,
                         optimizer = torchopt::optim_adamw,
                         opt_hparams = list(lr = 0.005,
                                            eps = 1.0e-08,
                                            weight_decay = 1.0e-06),
                         lr_decay_epochs = 1,
                         lr_decay_rate = 0.95,
                         patience = 20,
                         min_delta = 0.01,
                         verbose = FALSE) {

    # set caller to show in errors
    .check_set_caller("sits_tempcnn")

    # function that returns torch model based on a sits sample data.table
    result_fun <- function(samples) {

        # verifies if torch and luz packages are installed
        .check_require_packages(c("torch", "luz"))

        # preconditions
        .check_length(
            x = cnn_layers,
            len_min = 3,
            len_max = 3,
            msg = "tempcnn uses three CNN layers"
        )
        .check_length(
            x = cnn_dropout_rates,
            len_min = 3,
            len_max = 3,
            msg = "tempcnn uses three dropout rates"
        )
        .check_that(
            x = length(dense_layer_nodes) == 1,
            msg = "There is only one dense layer"
        )
        .check_that(
            x = length(dense_layer_dropout_rate) == 1,
            msg = "dropout rates must be provided for the dense layer"
        )
        .check_num(
            x = lr_decay_epochs,
            is_integer = TRUE,
            len_max = 1,
            min = 1,
            msg = "invalid learning rate decay epochs"
        )
        .check_num(
            x = lr_decay_rate,
            len_max = 1,
            max = 1,
            min = 0,
            allow_zero = FALSE,
            msg = "invalid learning rate decay"
        )

        # get parameters list and remove the 'param' parameter
        optim_params_function <- formals(optimizer)[-1]
        if (!is.null(opt_hparams)) {
            .check_chr_within(
                x = names(opt_hparams),
                within = names(optim_params_function),
                msg = "Invalid hyperparameters provided in optimizer."
            )
            optim_params_function <- utils::modifyList(optim_params_function,
                                                       opt_hparams)
        }

        # get the timeline of the data
        timeline <- sits_timeline(samples)
        # get the bands of the data
        bands <- sits_bands(samples)
        # get the labels of the data
        labels <- sits_labels(samples)

        # create a named vector with integers match the class labels
        n_labels <- length(labels)
        int_labels <- c(1:n_labels)
        names(int_labels) <- labels

        # number of bands and number of samples
        n_bands <- length(sits_bands(samples))
        n_times <- nrow(sits_time_series(samples[1, ]))

        # data normalization
        stats <- .sits_ml_normalization_param(samples)
        train_samples <- .sits_distances(
            .sits_ml_normalize_data(samples, stats)
        )

        # is the training data correct?
        .check_chr_within(
            x = "reference",
            within = names(train_samples),
            discriminator = "any_of",
            msg = "input data does not contain distances"
        )

        if (!is.null(samples_validation)) {

            # check if the labels matches with train data
            .check_that(
                all(sits_labels(samples_validation) %in% labels) &&
                    all(labels %in% sits_labels(samples_validation))
            )
            # check if the timeline matches with train data
            .check_that(
                length(sits_timeline(samples_validation)) == length(timeline)
            )
            # check if the bands matches with train data
            .check_that(
                all(sits_bands(samples_validation) %in% bands) &&
                    all(bands %in% sits_bands(samples_validation))
            )

            test_samples <- .sits_distances(
                .sits_ml_normalize_data(samples_validation, stats)
            )
        } else {
            # split the data into training and validation data sets
            # create partitions different splits of the input data
            test_samples <- .sits_distances_sample(
                train_samples,
                frac = validation_split
            )

            # remove the lines used for validation
            train_samples <- train_samples[!test_samples, on = "original_row"]
        }
        n_samples_train <- nrow(train_samples)
        n_samples_test <- nrow(test_samples)

        # shuffle the data
        train_samples <- train_samples[sample(
            nrow(train_samples),
            nrow(train_samples)
        ), ]
        test_samples <- test_samples[sample(
            nrow(test_samples),
            nrow(test_samples)
        ), ]

        # transform training data into a 3D tensor
        # remove first two columns
        # reshape the 2D matrix into a 3D array
        train_x <- array(
            data = as.matrix(train_samples[, -2:0]),
            dim = c(n_samples_train, n_times, n_bands)
        )
        # transform training reference to an integer vector
        train_y <- unname(int_labels[as.vector(train_samples$reference)])

        # transform test data into a 3D tensor
        # remove first two columns
        # reshape the 2D matrix into a 3D array
        test_x <- array(
            data = as.matrix(test_samples[, -2:0]),
            dim = c(n_samples_test, n_times, n_bands)
        )
        # transform test reference to an integer vector
        test_y <- unname(int_labels[as.vector(test_samples$reference)])

        # set random seed for torch
        torch::torch_manual_seed(sample.int(10^5, 1))

        # define main torch tempcnn module
        tcnn_module <- torch::nn_module(
            classname = "tcnn_module",
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
                x <- x %>%
                    torch::torch_transpose(2, 3) %>%
                    self$conv_bn_relu1() %>%
                    self$conv_bn_relu2() %>%
                    self$conv_bn_relu3() %>%
                    self$flatten() %>%
                    self$dense() %>%
                    self$softmax()
            }
        )

        torch::torch_set_num_threads(1)

        # train the model using luz
        torch_model <-
            luz::setup(
                module = tcnn_module,
                loss = torch::nn_cross_entropy_loss(),
                metrics = list(luz::luz_metric_accuracy()),
                optimizer = optimizer
            ) %>%
            luz::set_opt_hparams(
                !!!optim_params_function
            ) %>%
            luz::set_hparams(
                n_bands = n_bands,
                n_times = n_times,
                n_labels = n_labels,
                kernel_sizes = cnn_kernels,
                hidden_dims = cnn_layers,
                dropout_rates = cnn_dropout_rates,
                dense_layer_nodes = dense_layer_nodes,
                dense_layer_dropout_rate = dense_layer_dropout_rate
            ) %>%
            luz::fit(
                data = list(train_x, train_y),
                epochs = epochs,
                valid_data = list(test_x, test_y),
                callbacks = list(
                    luz::luz_callback_early_stopping(
                        monitor = "valid_loss",
                        patience = patience,
                        min_delta = min_delta,
                        mode = "min"),
                    luz::luz_callback_lr_scheduler(
                        torch::lr_step,
                        step_size = lr_decay_epochs,
                        gamma = lr_decay_rate
                    )
                ),
                dataloader_options = list(batch_size = batch_size),
                verbose = verbose
            )

        model_to_raw <- function(model) {
            con <- rawConnection(raw(), open = "wr")
            torch::torch_save(model, con)
            on.exit(close(con), add = TRUE)
            r <- rawConnectionValue(con)
            return(r)
        }

        model_from_raw <- function(object) {
            con <- rawConnection(object)
            on.exit(close(con), add = TRUE)
            module <- torch::torch_load(con)
            return(module)
        }
        # serialize model
        serialized_model <- model_to_raw(torch_model$model)

        # construct model predict closure function and returns
        model_predict <- function(values) {

            # verifies if torch package is installed
            if (!requireNamespace("torch", quietly = TRUE)) {
                stop("Please install package torch", call. = FALSE)
            }
            .check_require_packages("torch")

            # set torch threads to 1
            # function does not work on MacOS
            suppressWarnings(torch::torch_set_num_threads(1))

            # restore model
            torch_model$model <- model_from_raw(serialized_model)

            # transform input (data.table) into a 3D tensor
            # remove first two columns
            # reshape the 2D matrix into a 3D array
            n_samples <- nrow(values)
            n_times <- nrow(sits_time_series(samples[1, ]))
            n_bands <- length(sits_bands(samples))
            values_x <- array(
                data = as.matrix(values[, -2:0]),
                dim = c(n_samples, n_times, n_bands)
            )
            # retrieve the prediction probabilities
            predicted <- data.table::as.data.table(
                torch::as_array(
                    stats::predict(torch_model, values_x)
                )
            )
            # adjust the names of the columns of the probs
            colnames(predicted) <- labels

            return(predicted)
        }

        class(model_predict) <- c(
            "torch_model", "sits_model",
            class(model_predict)
        )
        return(model_predict)
    }

    result <- .sits_factory_function(samples, result_fun)
    return(result)
}
