
#' @title Train temporal convolutional neural network models
#' @name sits_TempCNN
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
#' @param samples           Time series with the training samples.
#' @param cnn_layers        Number of 1D convolutional filters per layer
#' @param cnn_kernels       Size of the 1D convolutional kernels.
#' @param cnn_dropout_rates Dropout rates for 1D convolutional filters.
#' @param dense_layer_nodes Number of nodes in the dense layer.
#' @param dense_layer_dropout_rate  Dropout rate (0,1) for the dense layer.
#' @param epochs            Number of iterations to train the model.
#' @param batch_size        Number of samples per gradient update.
#' @param validation_split  Number between 0 and 1. Fraction of training data
#'                          to be used as validation data.
#'                          The model will set apart this fraction of the
#'                          training data, will not train on it,
#'                          and will evaluate the loss and any model metrics
#'                          on this data at the end of each epoch.
#'                          The validation data is selected from the last
#'                          samples in the x and y data provided,
#'                          before shuffling.
#' @param verbose           Verbosity mode (0 = silent, 1 = progress bar,
#'                          2 = one line per epoch).
#'
#' @return A fitted model to be passed to \code{\link[sits]{sits_classify}}
#'
#' @examples
#' \dontrun{
#' # Retrieve the set of samples for the Mato Grosso (provided by EMBRAPA)
#'
#' # Build a machine learning model based on deep learning
#' tc_model <- sits_train(samples_modis_4bands, sits_TempCNN(epochs = 75))
#' # Plot the model
#' plot(tc_model)
#'
#' # get a point and classify the point with the ml_model
#' point <- sits_select(point_mt_6bands, bands = c("NDVI", "EVI", "NIR", "MIR"))
#' class <- sits_classify(point, tc_model)
#'
#' plot(class, bands = c("NDVI", "EVI"))
#' }
#' @export
sits_TempCNN <- function(samples = NULL,
                         cnn_layers = c(64, 64, 64),
                         cnn_kernels = c(5, 5, 5),
                         cnn_dropout_rates = c(0.50, 0.50, 0.50),
                         dense_layer_nodes = 256,
                         dense_layer_dropout_rate = 0.50,
                         epochs = 60,
                         batch_size = 64,
                         validation_split = 0.2,
                         verbose = FALSE) {

    # set caller to show in errors
    .check_set_caller("sits_TempCNN")

    # function that returns keras model based on a sits sample data.table
    result_fun <- function(data) {

        # verifies if torch package is installed
        if (!requireNamespace("torch", quietly = TRUE)) {
            stop("Please install package torch", call. = FALSE)
        }
        # verifies if torch package is installed
        if (!requireNamespace("luz", quietly = TRUE)) {
            stop("Please install package luz", call. = FALSE)
        }
        # preconditions
        .check_length(
            x = cnn_layers,
            len_min = 3,
            len_max = 3,
            msg = "tempCNN uses three CNN layers"
        )
        .check_length(
            x = cnn_dropout_rates,
            len_min = 3,
            len_max = 3,
            msg = "tempCNN uses three dropout rates"
        )
        .check_that(
            x = length(dense_layer_nodes) == 1,
            msg = "There is only one dense layer"
        )
        .check_that(
            x = length(dense_layer_dropout_rate) == 1,
            msg = "dropout rates must be provided for the dense layer"
        )
        # get the labels of the data
        labels <- sits_labels(data)
        # create a named vector with integers match the class labels
        n_labels <- length(labels)
        int_labels <- c(1:n_labels)
        names(int_labels) <- labels

        # number of bands and number of samples
        n_bands <- length(sits_bands(data))
        n_times <- nrow(sits_time_series(data[1, ]))

        # data normalization
        stats <- .sits_ml_normalization_param(data)
        train_data <- .sits_distances(.sits_ml_normalize_data(data, stats))

        # split the data into training and validation data sets
        # create partitions different splits of the input data
        test_data <- .sits_distances_sample(train_data,
                                            frac = validation_split
        )
        # remove the lines used for validation
        train_data <- train_data[!test_data, on = "original_row"]

        n_samples_train <- nrow(train_data)
        n_samples_test <- nrow(test_data)

        # shuffle the data
        train_data <- train_data[sample(
            nrow(train_data),
            nrow(train_data)
        ), ]
        test_data <- test_data[sample(
            nrow(test_data),
            nrow(test_data)
        ), ]

        # transform training data into a 3D tensor
        # remove first two columns
        # reshape the 2D matrix into a 3D array
        train_x <- array(
            data = as.matrix(train_data[, -2:0]),
            dim = c(n_samples_train, n_times, n_bands)
        )
        # transform training reference to an integer vector
        train_y <- unname(int_labels[as.vector(train_data$reference)])

        # transform test data into a 3D tensor
        # remove first two columns
        # reshape the 2D matrix into a 3D array
        test_x <- array(
            data = as.matrix(test_data[, -2:0]),
            dim = c(n_samples_test, n_times, n_bands)
        )
        # transform test reference to an integer vector
        test_y <- unname(int_labels[as.vector(test_data$reference)])

        # set random seed for torch
        torch::torch_manual_seed(sample.int(10^5, 1))

        # module for 1D convolution with batch normalization and dropout
        conv1D_batch_norm_relu_dropout <- torch::nn_module(
            classname = "conv1D_batch_norm_relu_dropout",
            initialize = function(input_dim,
                                  hidden_dim,
                                  kernel_size,
                                  dropout_rate) {
                self$block <- torch::nn_sequential(
                    torch::nn_conv1d(input_dim,
                                     hidden_dim,
                                     kernel_size,
                                     padding = as.integer(kernel_size %/% 2)
                    ),
                    torch::nn_batch_norm1d(hidden_dim),
                    torch::nn_relu(),
                    torch::nn_dropout(dropout_rate)
                )
            },
            forward = function(x) {
                self$block(x)
            }
        )
        # module for linear transformation with batch normalization and dropout
        fc_batch_norm_relu_dropout <- torch::nn_module(
            classname = "fc_batch_norm_relu_dropout",
            initialize = function(input_dim,
                                  hidden_dims,
                                  dropout_rate) {
                self$block <- torch::nn_sequential(
                    torch::nn_linear(input_dim, hidden_dims),
                    torch::nn_batch_norm1d(hidden_dims),
                    torch::nn_relu(),
                    torch::nn_dropout(dropout_rate)
                )
            },
            forward = function(x) {
                self$block(x)
            }
        )
        # define main torch tempCNN module
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
                self$conv_bn_relu1 <- conv1D_batch_norm_relu_dropout(
                    input_dim = n_bands,
                    hidden_dim = hidden_dims[1],
                    kernel_size = kernel_sizes[1],
                    dropout_rate = dropout_rates[1]
                )
                # second module - 1D CNN
                self$conv_bn_relu2 <- conv1D_batch_norm_relu_dropout(
                    input_dim = hidden_dims[1],
                    hidden_dim = hidden_dims[2],
                    kernel_size = kernel_sizes[2],
                    dropout_rate = dropout_rates[2]
                )
                # third module - 1D CNN
                self$conv_bn_relu3 <- conv1D_batch_norm_relu_dropout(
                    input_dim    = hidden_dims[2],
                    hidden_dim   = hidden_dims[3],
                    kernel_size  = kernel_sizes[3],
                    dropout_rate = dropout_rates[3]
                )
                # flatten 3D tensor to 2D tensor
                self$flatten <- torch::nn_flatten()
                # create a dense tensor
                self$dense <- fc_batch_norm_relu_dropout(
                    input_dim    = hidden_dims[3] * n_times,
                    hidden_dim   = dense_layer_nodes,
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
        # train the model using luz
        torch_model <-
            luz::setup(
                module = tcnn_module,
                loss = torch::nn_cross_entropy_loss(),
                metrics = list(luz::luz_metric_accuracy()),
                optimizer = torch::optim_adam
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
                callbacks = list(luz::luz_callback_early_stopping(
                    patience = 10,
                    min_delta = 0.05
                )),
                verbose = verbose,
                dataloader_options = list(batch_size = batch_size)
            )

        model_to_raw <- function(model) {
            con <- rawConnection(raw(), open = "wr")
            torch::torch_save(model, con)
            on.exit(
                {
                    close(con)
                },
                add = TRUE
            )
            r <- rawConnectionValue(con)
            return(r)
        }

        model_from_raw <- function(object) {
            con <- rawConnection(object)
            on.exit(
                {
                    close(con)
                },
                add = TRUE
            )
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

            # restore model
            torch_model$model <- model_from_raw(serialized_model)

            # transform input (data.table) into a 3D tensor
            # remove first two columns
            # reshape the 2D matrix into a 3D array
            n_samples <- nrow(values)
            n_times <- nrow(sits_time_series(data[1, ]))
            n_bands <- length(sits_bands(data))
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
