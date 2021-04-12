#' @title Train a model using the a full Convolutional Neural Network
#' @name sits_FCN
#'
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Alexandre Ywata de Carvalho, \email{alexandre.ywata@@ipea.gov.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Use the a full 1D CNN algorithm to classify data.
#' Users can define the number of convolutional layers,
#' the size of the convolutional kernels, and the activation functions.
#'
#' The FCN has been proposed for time series classification by Wang et al.
#' The SITS implementation of FCN is based on the work of Hassan Fawaz and
#' collaborators. Fawaz provides a reference Keras implementation of FCN
#' in https://github.com/hfawaz/dl-4-tsc.
#' If you use this function, please cite the references.
#'
#' @references Hassan Fawaz, Germain Forestier, Jonathan Weber,
#' Lhassane Idoumghar,  and Pierre-Alain Muller,
#' "Deep learning for time series classification: a review",
#' Data Mining and Knowledge Discovery, 33(4): 917--963, 2019.
#'
#' Zhiguang Wang, Weizhong Yan, and Tim Oates,
#' "Time series classification from scratch with deep neural networks:
#' A strong baseline",
#' 2017 international joint conference on neural networks (IJCNN).
#'
#' @param samples           Time series with the training samples.
#' @param layers            Vector with size of the 1D convolutional filters
#'                          for each layer.
#' @param kernels           Vector with size of the 1D convolutional kernels.
#' @param activation        Activation function for 1D convolution.
#'                          Valid values: {'relu', 'elu', 'selu', 'sigmoid'}.
#' @param L2_rate           Regularization rate for 1D convolution.
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
#' @return A model fitted to input data to be passed
#'         to \code{\link[sits]{sits_classify}}
#'
#' @examples
#' \dontrun{
#' # Retrieve the set of samples for the Mato Grosso region
#'
#' # Build a machine learning model based on deep learning
#' fcn_model <- sits_train(samples_mt_4bands, sits_FCN())
#' # Plot the model
#' plot(fcn_model)
#'
#' # get a point and classify the point with the ml_model
#' point <- sits_select(point_mt_6bands,
#'     bands = c("NDVI", "EVI", "NIR", "MIR")
#' )
#' point_class <- sits_classify(point.tb, cnn_model)
#' plot(point_class, bands = c("NDVI", "EVI"))
#' }
#' @export
sits_FCN <- function(samples = NULL,
                     layers = c(128, 256, 128),
                     kernels = c(8, 5, 3),
                     activation = "relu",
                     L2_rate = 1e-06,
                     optimizer = keras::optimizer_adam(lr = 0.001),
                     epochs = 150,
                     batch_size = 128,
                     validation_split = 0.2,
                     verbose = 1) {
    # backward compatibility
    samples <- .sits_tibble_rename(samples)

    # function that returns keras model based on a sits sample data.table
    result_fun <- function(data) {
        # pre-conditions
        assertthat::assert_that(length(layers) == length(kernels),
                                msg = "sits_FCN: 1D layers must match 1D kernels"
        )

        valid_activations <- c("relu", "elu", "selu", "sigmoid")
        assertthat::assert_that(all(activation %in% valid_activations),
                                msg = "sits_FCN: invalid CNN activation method"
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
        stats <- .sits_normalization_param(data)
        train_data <- .sits_distances(.sits_normalize_data(data, stats))

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

        n_layers <- length(layers)
        # build the 1D nodes
        for (i in seq_len(n_layers)) {
            # Add a Convolution1D layer
            output_tensor <- keras::layer_conv_1d(output_tensor,
                                       filters = layers[[i]],
                                       kernel_size = kernels[[i]],
                                       kernel_regularizer = keras::regularizer_l2(l = L2_rate)
            )
            # Batch normalization
            output_tensor <- keras::layer_batch_normalization(output_tensor)
            # activation
            output_tensor <- keras::layer_activation(output_tensor,
                                                     activation = activation)
        }

        # Apply max pooling?
        output_tensor <- keras::layer_global_average_pooling_1d(output_tensor)

        # create the final tensor
        model_loss <- "categorical_crossentropy"
        if (n_labels == 2) {
            output_tensor <- keras::layer_dense(output_tensor,
                                                units = 1,
                                                activation = "sigmoid"
            )
            model_loss <- "binary_crossentropy"
        }
        else {
            output_tensor <- keras::layer_dense(output_tensor,
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

        # show training evolution
        graphics::plot(history)

        # construct model predict closure function and returns
        model_predict <- function(values) {
            # transform input (data.table) into a 3D tensor
            # get the dimensions of the 3D tensor
            n_samples <- nrow(values)
            n_times <- nrow(sits_time_series(data[1, ]))
            n_bands <- length(sits_bands(data))

            # reorganize the data as a 3D matrix
            values <- array(
                data = as.matrix(values[, 3:ncol(values)]),
                dim = c(n_samples, n_times, n_bands)
            )
            # retrieve the prediction probabilities
            predicted <- data.table::as.data.table(stats::predict(
                model_keras,
                values
            ))

            # If binary classification,
            # adjust the prediction values to match binary classification
            if (n_labels == 2) {
                predicted <- .sits_keras_binary_class(predicted)
            }

            # adjust the names of the columns of the probs
            colnames(predicted) <- labels

            return(predicted)
        }
        class(model_predict) <- c("keras_model", class(model_predict))
        return(model_predict)
    }

    result <- .sits_factory_function(samples, result_fun)
    return(result)
}
