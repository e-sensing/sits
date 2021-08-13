
#' @title Train a model using the Temporal Convolutional Neural Network
#' @name sits_TempCNN
#'
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Alexandre Ywata de Carvalho, \email{alexandre.ywata@@ipea.gov.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Use a TempCNN algorithm to classify data, which has
#' two stages: a 1D CNN and a  multi-layer perceptron.
#' Users can define the depth of the 1D network, as well as
#' the number of perceptron layers.
#'
#' This function is based on the paper by Charlotte Pelletier referenced below
#' and code available on github (https://github.com/charlotte-pel/temporalCNN).
#' If you use this method, please cite the original tempCNN paper.
#'
#' @references Charlotte Pelletier, Geoffrey Webb and François Petitjean,
#' "Temporal Convolutional Neural Network for the Classification
#' of Satellite Image Time Series",
#' Remote Sensing, 11,523, 2019. DOI: 10.3390/rs11050523.
#'
#' @param samples           Time series with the training samples.
#' @param cnn_layers        Number of 1D convolutional filters per layer
#' @param cnn_kernels       Size of the 1D convolutional kernels.
#' @param cnn_activation    Activation function for 1D convolution.
#'                          Valid values: {'relu', 'elu', 'selu', 'sigmoid'}.
#' @param cnn_L2_rate       Regularization rate for 1D convolution.
#' @param cnn_dropout_rates Dropout rates for 1D convolutional filters.
#' @param dense_layer_nodes Number of nodes in the dense layer.
#' @param dense_layer_activation    Activation functions for the dense layer.
#'                                  Valid values: {'relu', 'elu', 'selu', 'sigmoid'}.
#' @param dense_layer_dropout_rate  Dropout rate (0,1) for the dense layer.
#' @param optimizer         Function with a pointer to the optimizer function
#'                          (default is optimization_adam()).
#'                          Options: optimizer_adadelta(), optimizer_adagrad(),
#'                          optimizer_adam(), optimizer_adamax(),
#'                          optimizer_nadam(), optimizer_rmsprop(),
#'                          optimizer_sgd().
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
                         cnn_layers        = c(64, 64, 64),
                         cnn_kernels       = c(5, 5, 5),
                         cnn_activation    = "relu",
                         cnn_L2_rate       = 1e-06,
                         cnn_dropout_rates = c(0.50, 0.50, 0.50),
                         dense_layer_nodes = 256,
                         dense_layer_activation    = "relu",
                         dense_layer_dropout_rate  = 0.50,
                         optimizer = keras::optimizer_adam(learning_rate = 0.001),
                         epochs            = 150,
                         batch_size        = 128,
                         validation_split  = 0.2,
                         verbose = 0) {


    # function that returns keras model based on a sits sample data.table
    result_fun <- function(data) {

        # verifies if keras package is installed
        if (!requireNamespace("keras", quietly = TRUE)) {
            stop(paste("keras required for this function to work.",
                       "Please install it."), call. = FALSE)
        }

        # pre-conditions
        valid_activations <- c("relu", "elu", "selu", "sigmoid")

        assertthat::assert_that(
            length(cnn_layers) == length(cnn_kernels),
            msg = "sits_tempCNN: 1D layers must match 1D kernel sizes"
        )

        assertthat::assert_that(
            length(cnn_layers) == length(cnn_dropout_rates),
            msg = "sits_tempCNN: 1D layers must match 1D dropout rates"
        )

        assertthat::assert_that(
            length(dense_layer_nodes) == 1,
            msg = "sits_tempCNN: There is only one dense layer"
        )
        assertthat::assert_that(
            length(dense_layer_dropout_rate) == 1,
            msg = "sits_tempCNN: dropout rates must be provided for the dense layer"
        )

        assertthat::assert_that(
            cnn_activation %in% valid_activations,
            msg = "sits_tempCNN: invalid CNN activation method"
        )

        assertthat::assert_that(
            dense_layer_activation %in% valid_activations,
            msg = "sits_tempCNN: invalid activation method for dense layer"
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

        # organize data for model training
        train_x <- array(
            data = as.matrix(train_data[, 3:ncol(train_data)]),
            dim = c(n_samples_train, n_times, n_bands)
        )
        train_y <- unname(int_labels[as.vector(train_data$reference)]) - 1

        # create the test data for keras
        test_x <- array(
            data = as.matrix(test_data[, 3:ncol(test_data)]),
            dim = c(n_samples_test, n_times, n_bands)
        )
        test_y <- unname(int_labels[as.vector(test_data$reference)]) - 1

        # build the model step by step
        # create the input_tensor for 1D convolution
        input_tensor <- keras::layer_input(shape = c(n_times, n_bands))
        output_tensor <- input_tensor


        # build a set 1D convolution layers
        for (i in seq_len(length(cnn_layers))) {
            # Add a Convolution1D
            output_tensor <- keras::layer_conv_1d(
                output_tensor,
                filters = cnn_layers[[i]],
                kernel_size = cnn_kernels[[i]],
                kernel_initializer = "he_normal",
                kernel_regularizer = keras::regularizer_l2(l = cnn_L2_rate),
                padding = "same"
            )
            # batch normalization
            output_tensor <- keras::layer_batch_normalization(output_tensor)

            # Activation
            output_tensor <- keras::layer_activation(output_tensor,
                                                     activation = cnn_activation)
            # Apply layer dropout
            output_tensor <- keras::layer_dropout(output_tensor,
                                                  rate = cnn_dropout_rates[[i]])
        }

        # reshape a tensor into a 2D shape
        output_tensor <- keras::layer_flatten(output_tensor)

        # build the the dense layer
        output_tensor <- keras::layer_dense(
                output_tensor,
                units = dense_layer_nodes
        )

        # batch normalization
        output_tensor <- keras::layer_batch_normalization(output_tensor)
        # Activation
        output_tensor <- keras::layer_activation(output_tensor,
                                            activation = dense_layer_activation
        )
        # dropout
        output_tensor  <- keras::layer_dropout(output_tensor,
                                               rate = dense_layer_dropout_rate
        )


        # create the softmax layer
        model_loss <- "categorical_crossentropy"
        if (n_labels == 2) {
            output_tensor <- keras::layer_dense(
                output_tensor,
                units = 1,
                activation = "sigmoid"
            )
            model_loss <- "binary_crossentropy"
        }
        else {
            output_tensor <- keras::layer_dense(
                output_tensor,
                units = n_labels,
                activation = "softmax"
            )
            # keras requires categorical data to be put in a matrix
            train_y <- keras::to_categorical(train_y, n_labels)
            test_y <- keras::to_categorical(test_y, n_labels)
        }
        # create the model
        model_keras <- keras::keras_model(input_tensor, output_tensor)
        # compile the model
        model_keras %>% keras::compile(
            loss = model_loss,
            optimizer = optimizer,
            metrics = "accuracy"
        )

        # fit the model
        history <- model_keras %>% keras::fit(
            train_x, train_y,
            epochs = epochs, batch_size = batch_size,
            validation_data = list(test_x, test_y),
            verbose = verbose, view_metrics = "auto"
        )

        # import model to R
        R_model_keras <- keras::serialize_model(model_keras)

        # construct model predict closure function and returns
        model_predict <- function(values) {

            # verifies if keras package is installed
            if (!requireNamespace("keras", quietly = TRUE)) {
                stop(paste("keras required for this function to work.",
                           "Please install it."), call. = FALSE)
            }

            # restore model keras
            model_keras <- keras::unserialize_model(R_model_keras)

            # transform input (data.table) into a 3D tensor
            # (remove first two columns)
            n_samples <- nrow(values)
            n_timesteps <- nrow(sits_time_series(data[1, ]))
            n_bands <- length(sits_bands(data))
            values_x <- array(
                data = as.matrix(values[, 3:ncol(values)]),
                dim = c(n_samples, n_timesteps, n_bands)
            )
            # retrieve the prediction probabilities
            prediction <- data.table::as.data.table(stats::predict(
                model_keras,
                values_x
            ))
            # If binary classification,
            # adjust the prediction values to match binary classification
            if (n_labels == 2) {
                prediction <- .sits_keras_binary_class(prediction)
            }

            # adjust the names of the columns of the probs
            colnames(prediction) <- labels

            return(prediction)
        }

        class(model_predict) <- c("keras_model", "sits_model",
                                  class(model_predict))
        return(model_predict)
    }

    result <- .sits_factory_function(samples, result_fun)
    return(result)
}
