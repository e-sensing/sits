#' @title Train a model using the ResNet model
#' @name sits_ResNet
#'
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Alexandre Ywata de Carvalho, \email{alexandre.ywata@@ipea.gov.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Use a ResNet architecture for classifiying image time series.
#' The ResNet (or deep residual network) was proposed by a team
#' in Microsoft Research for 2D image classification.
#' ResNet tries to address the degradation of accuracy
#' in a deep network. The idea is to replace a deep network
#' with a combination of shallow ones.
#' In the paper by Fawaz et al. (2019), ResNet was considered the best method
#' for time series classification, using the UCR dataset.
#' Please refer to the paper for more details.
#'
#' The SITS implementation of RestNet is based on the work of Hassan Fawaz and
#' collaborators, and also inspired by the paper of Wang et al (see below).
#' Fawaz provides a reference code  in https://github.com/hfawaz/dl-4-tsc.
#' If you use this function, please cite the references.
#'
#' @references Hassan Fawaz, Germain Forestier, Jonathan Weber,
#' Lhassane Idoumghar,  and Pierre-Alain Muller,
#' "Deep learning for time series classification: a review",
#' Data Mining and Knowledge Discovery, 33(4): 917--963, 2019.
#'
#' Zhiguang Wang, Weizhong Yan, and Tim Oates,
#' "Time series classification from scratch with deep neural networks:
#'  A strong baseline",
#'  2017 international joint conference on neural networks (IJCNN).
#'
#' @param samples           Time series with the training samples.
#' @param blocks            Number of 1D convolutional filters for
#'                          each block of three layers.
#' @param kernels           Size of the 1D convolutional kernels
#'                          for each layer of each block.
#' @param activation        Activation function for 1D convolution.
#'                          Valid values: {'relu', 'elu', 'selu', 'sigmoid'}.
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
#' rn_model <- sits_train(samples_modis_4bands, sits_ResNet(epochs = 75))
#' # Plot the model
#' plot(rn_model)
#'
#' # get a point and classify the point with the ml_model
#' point <- sits_select(point_mt_6bands,
#'     bands = c("NDVI", "EVI", "NIR", "MIR")
#' )
#' class <- sits_classify(point.tb, rn_model)
#' plot(class, bands = c("NDVI", "EVI"))
#' }
#' @export
sits_ResNet <- function(samples = NULL,
                        blocks = c(64, 128, 128),
                        kernels = c(8, 5, 3),
                        activation = "relu",
                        optimizer = keras::optimizer_adam(lr = 0.001),
                        epochs = 300,
                        batch_size = 64,
                        validation_split = 0.2,
                        verbose = 0) {

    # function that returns keras model based on a sits sample data.table
    result_fun <- function(data) {
        # verifies if keras package is installed
        if (!requireNamespace("keras", quietly = TRUE)) {
            stop(paste("keras required for this function to work.",
                       "Please install it."), call. = FALSE)
        }

        valid_activations <- c("relu", "elu", "selu", "sigmoid")
        # pre-conditions
        assertthat::assert_that(
            activation %in% valid_activations,
            msg = "sits_ResNet: invalid CNN activation method"
        )

        assertthat::assert_that(
            length(kernels) == 3,
            msg = "sits_ResNet: should inform size of three kernels"
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

        # initial assignment
        output_tensor <- input_tensor
        shortcut <- input_tensor

        n_blocks <- length(blocks)
        for (i in seq_len(n_blocks)) {
            # Add a Convolution1D
            output_tensor_x <- keras::layer_conv_1d(output_tensor,
                                                    filters = blocks[[i]],
                                                    kernel_size = kernels[1],
                                                    padding = "same"
            )
            # normalization
            output_tensor_x <- keras::layer_batch_normalization(output_tensor_x)

            # activation
            output_tensor_x <- keras::layer_activation(output_tensor_x,
                                                       activation = activation
            )

            # Add a new convolution
            output_tensor_y <- keras::layer_conv_1d(output_tensor_x,
                                                    filters = blocks[[i]],
                                                    kernel_size = kernels[2],
                                                    padding = "same"
            )
            # normalization
            output_tensor_y <- keras::layer_batch_normalization(output_tensor_y)

            # activation
            output_tensor_y <- keras::layer_activation(output_tensor_y,
                                                       activation = activation
            )

            # Add a third convolution
            output_tensor_z <- keras::layer_conv_1d(output_tensor_y,
                                                    filters = blocks[[i]],
                                                    kernel_size = kernels[3],
                                                    padding = "same"
            )
            output_tensor_z <- keras::layer_batch_normalization(output_tensor_z)

            # include the shortcut
            shortcut <- keras::layer_conv_1d(shortcut,
                                             filters = blocks[[i]],
                                             kernel_size = 1,
                                             padding = "same"
            )
            shortcut <- keras::layer_batch_normalization(shortcut)

            # get the output tensor
            output_tensor <- keras::layer_add(list(shortcut, output_tensor_z))
            output_tensor <- keras::layer_activation(output_tensor,
                                                     activation = activation
            )
            shortcut <- output_tensor
        }

        # reshape a tensor into a 2D shape
        output_tensor <- keras::layer_global_average_pooling_1d(output_tensor)
        # reshape a tensor into a 2D shape
        output_tensor <- keras::layer_flatten(output_tensor)

        # create the final tensor
        model_loss <- ""
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
            model_loss <- "categorical_crossentropy"
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
            # adjust the prediction values for binary classification
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
