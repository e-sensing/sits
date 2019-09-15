#' @title Provides access to diagnostic information about a Keras deep learning model
#' @name sits_keras_diagnostics
#'
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description After the Keras deeplearning model is compiled and fit, this
#'              function provides access to the history plot and the evaluation results.
#'
#' @param dl_model  A valid keras model.
#'
#' @return This function returns NULL. It only prints the model diagnostics.
#'
#' @examples
#' \donttest{
#' # Retrieve the set of samples for the Mato Grosso region (provided by EMBRAPA)
#' data(cerrado_2classes)
#'  # obtain a DL model
#' dl_model <- sits_train(cerrado_2classes,
#'      sits_deeplearning(units = c(512, 512), dropout_rates = c(0.45, 0.25), epochs = 100))
#' # run the keras diagnostics
#' sits_keras_diagnostics(dl_model)
#' }
#' @export
sits_keras_diagnostics <- function(dl_model) {
    if (purrr::is_null(environment(dl_model)$model.keras)) {
        message("Please configure a keras model before running this function")
        return(FALSE)
    }

    message("Plotting history of the model fit")
    graphics::plot(environment(dl_model)$history)

    test_eval <- keras::evaluate(environment(dl_model)$model.keras, environment(dl_model)$test.x, environment(dl_model)$test.y, verbose = 0)
    message("Estimated loss and accuracy based on test data")
    message(paste0("Estimated accuracy: ", round(test_eval$acc, digits = 3),
                   " estimated loss: ", round(test_eval$loss, digits = 3)))
    return(TRUE)
}

#' @title Train a classification model using a multi-layer perceptron.
#' @name sits_deeplearning
#'
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Alexandre Xavier Ywata de Carvalho, \email{alexandre.ywata@@ipea.gov.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Use a multi-layer perceptron to classify data. USers can define
#' the number and size of the hidden layers, and the dropout rates and activation
#' functions for each layer.
#' This function is a front-end to the "keras" method R package.
#' Please refer to the documentation in that package for more details.
#'
#' @param data              Time series with the training samples.
#' @param layers            Vector with the number of hidden nodes in each hidden layer.
#' @param activation        Vector with the names of activation functions. Valid values are {'relu', 'elu', 'selu', 'sigmoid'}.
#' @param dropout_rates     Vector with the dropout rates (0,1) for each layer to the next layer.
#' @param optimizer         Function with a pointer to the optimizer function (default is optimization_adam()).
#'                          Options are optimizer_adadelta(), optimizer_adagrad(), optimizer_adam(),
#'                          optimizer_adamax(), optimizer_nadam(), optimizer_rmsprop(), optimizer_sgd()
#' @param epochs            Number of iterations to train the model.
#' @param batch_size        Number of samples per gradient update.
#' @param validation_split  Number between 0 and 1. Fraction of the training data to be used as validation data.
#'                          The model will set apart this fraction of the training data, will not train on it,
#'                          and will evaluate the loss and any model metrics on this data at the end of each epoch.
#'                          The validation data is selected from the last samples in the x and y data provided,
#'                          before shuffling.
#' @param verbose           Verbosity mode (0 = silent, 1 = progress bar, 2 = one line per epoch).
#' @param binary_classification A lenght-one logical indicating if this is a binary classification. If it is so,
#'                          the number of unique labels in the training data must be two as well.
#'
#' @return A model fitted to input data to be passed to \code{\link[sits]{sits_classify}}
#'
#' @examples
#' \donttest{
#' # Retrieve the set of samples for the Mato Grosso region (provided by EMBRAPA)
#' samples_2bands <- sits_select_bands(samples_mt_6bands, ndvi, evi)
#'
#' # Build a machine learning model based on deep learning
#' dl_model <- sits_train (samples_2bands,
#'                         sits_deeplearning(layers = c(512, 512, 512),
#'                                           dropout_rates = c(0.50, 0.40, 0.35),
#'                                           epochs = 100))
#'
#' # get a point and classify the point with the ml_model
#' point.tb <- sits_select_bands(point_mt_6bands, ndvi, evi)
#' class.tb <- sits_classify(point.tb, dl_model)
#' sits_plot(class.tb)
#' }
#' @export
sits_deeplearning <- function(data          = NULL,
                              layers           = c(512, 512, 512, 512, 512),
                              activation       = 'elu',
                              dropout_rates    = c(0.50, 0.40, 0.35, 0.30, 0.20),
                              optimizer        = keras::optimizer_adam(lr = 0.001),
                              epochs           = 500,
                              batch_size       = 128,
                              validation_split = 0.2,
                              verbose          = 1,
                              binary_classification = FALSE) {
    # backward compatibility
    if ("coverage" %in% names(data))
        data <- .sits_tibble_rename(data)

    # function that returns keras model based on a sits sample data.table
    result_fun <- function(data){
        # pre-conditions
        ensurer::ensure_that(layers, length(.) == length(dropout_rates),
                             err_desc = "sits_deeplearning: number of layers must match number of dropout rates")

        valid_activations <- c("relu", "elu", "selu", "sigmoid")
        ensurer::ensure_that(activation, (.) %in% valid_activations,
                             err_desc = "sits_deeplearning: invalid node activation method")

        # data normalization
        stats <- .sits_normalization_param(data)
        train_data_DT <- .sits_distances(.sits_normalize_data(data, stats))

        # is the train data correct?
        ensurer::ensure_that(train_data_DT, "reference" %in% names(.),
                             err_desc = "sits_deeplearning: input data does not contain distances")


        # get the labels of the data
        labels <- sits_labels(data)$label

        # create a named vector with integers match the class labels
        n_labels <- length(labels)
        int_labels <- c(1:n_labels)
        names(int_labels) <- labels

        # split the data into training and validation data sets
        # create partitions different splits of the input data
        test_data_DT <- .sits_sample_distances(train_data_DT, frac = validation_split)

        # remove the lines used for validation
        train_data_DT <- train_data_DT[!test_data_DT, on = "original_row"]

        # shuffle the data
        train_data_DT <- train_data_DT[sample(nrow(train_data_DT), nrow(train_data_DT)),]
        test_data_DT  <- test_data_DT[sample(nrow(test_data_DT), nrow(test_data_DT)),]

        # organize data for model training
        train.x <- data.matrix(train_data_DT[, -(1:2)])
        train.y <- unname(int_labels[as.vector(train_data_DT$reference)]) - 1

        # create the test data for keras
        test.x <- data.matrix(test_data_DT[, -(1:2)])
        test.y <- unname(int_labels[as.vector(test_data_DT$reference)]) - 1

        # build the model step by step
        # create the input_tensor
        input_tensor  <- keras::layer_input(shape = c(NCOL(train.x)))
        output_tensor <-  input_tensor

        # build the nodes
        for (i in 1:length(layers)) {
            output_tensor <- keras::layer_dense(output_tensor, units = layers[i], activation = activation)
            output_tensor <- keras::layer_dropout(output_tensor, rate = dropout_rates[i])
            output_tensor <- keras::layer_batch_normalization(output_tensor)
        }
        # create the final tensor
        model_loss <- ""
        if (binary_classification && n_labels == 2) {
            output_tensor <- keras::layer_dense(output_tensor, units = 1, activation = "sigmoid")
            model_loss <- "binary_crossentropy"
        }
        else {
            output_tensor <- keras::layer_dense(output_tensor, units = n_labels, activation = "softmax")
            model_loss <- "categorical_crossentropy"
            # keras requires categorical data to be put in a matrix
            train.y <- keras::to_categorical(train.y, n_labels)
            test.y  <- keras::to_categorical(test.y, n_labels)
        }
        # create the model
        model.keras <- keras::keras_model(input_tensor, output_tensor)
        # compile the model
        model.keras %>% keras::compile(
            loss = model_loss,
            optimizer = optimizer,
            metrics = "accuracy"
        )

        # fit the model
        history <- model.keras %>% keras::fit(
            train.x, train.y,
            epochs = epochs, batch_size = batch_size,
            validation_data = list(test.x, test.y),
            verbose = verbose, view_metrics = "auto"
        )

        # show training evolution
        graphics::plot(history)

        # construct model predict closure function and returns
        model_predict <- function(values_DT){
            # transform input (data.table) into a matrix (remove first two columns)
            values.x         <- data.matrix(values_DT[, -(1:2)])
            # retrieve the prediction probabilities
            prediction_DT <- data.table::as.data.table(stats::predict(model.keras, values.x))
            # adjust the names of the columns of the probs
            colnames(prediction_DT) <- labels

            return(prediction_DT)
        }

        return(model_predict)
    }

    result <- .sits_factory_function(data, result_fun)
    return(result)
}

#' @title Train a model using the a full Convolutional Neural Network
#' @name sits_FCN
#'
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Alexandre Xavier Ywata de Carvalho, \email{alexandre.ywata@@ipea.gov.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Use the a full 1D CNN algorithm to classify data. Users can define the number of
#' convolutional layers], the size of the convolutional
#' kernels, and the activation functions.
#'
#' The FCN has been proposed for time series classification by the paper of Wang et al.
#' The SITS implementation of FCN is based on the work of Hassan Fawaz and
#' collaborators. Fawaz provides a reference Keras implementation of FCN
#' in https://github.com/hfawaz/dl-4-tsc.
#' If you use this function, please cite the references.
#'
#' @references Hassan Fawaz, Germain Forestier, Jonathan Weber, Lhassane Idoumghar,  and Pierre-Alain Muller,
#' "Deep learning for time series classification: a review",
#' Data Mining and Knowledge Discovery, 33(4): 917--963, 2019.
#'
#' Zhiguang Wang, Weizhong Yan, and Tim Oates,
#' "Time series classification from scratch with deep neural networks: A strong baseline",
#'  2017 international joint conference on neural networks (IJCNN).
#'
#'
#'
#' @param data              Time series with the training samples.
#' @param layers            Vector with the size of the 1D convolutional filters for each layer.
#' @param kernels           Vector with the size of the 1D convolutional kernels.
#' @param activation        Activation function for 1D convolution.
#'                          Valid values are {'relu', 'elu', 'selu', 'sigmoid'}.
#' @param L2_rate           Regularization rate for 1D convolution.
#'                          Valid values are {'relu', 'elu', 'selu', 'sigmoid'}.
#' @param optimizer         Function with a pointer to the optimizer function (default is optimization_adam()).
#'                          Options are optimizer_adadelta(), optimizer_adagrad(), optimizer_adam(),
#'                          optimizer_adamax(), optimizer_nadam(), optimizer_rmsprop(), optimizer_sgd()
#' @param epochs            Number of iterations to train the model.
#' @param batch_size        Number of samples per gradient update.
#' @param validation_split  Number between 0 and 1. Fraction of the training data to be used as validation data.
#'                          The model will set apart this fraction of the training data, will not train on it,
#'                          and will evaluate the loss and any model metrics on this data at the end of each epoch.
#'                          The validation data is selected from the last samples in the x and y data provided,
#'                          before shuffling.
#' @param verbose           Verbosity mode (0 = silent, 1 = progress bar, 2 = one line per epoch).
#' @param binary_classification A lenght-one logical indicating if this is a binary classification. If it is so,
#'                          the number of unique labels in the training data must be two as well.
#'
#' @return A model fitted to input data to be passed to \code{\link[sits]{sits_classify}}
#'
#' @examples
#' \donttest{
#' # Retrieve the set of samples for the Mato Grosso region (provided by EMBRAPA)
#' samples_2bands <- sits_select_bands(samples_mt_6bands, ndvi, evi)
#'
#' # Build a machine learning model based on deep learning
#' cnn_model <- sits_train (samples_2bands, sits_FCN())
#'
#' # get a point and classify the point with the ml_model
#' point.tb <- sits_select_bands(point_mt_6bands, ndvi, evi)
#' class.tb <- sits_classify(point.tb, cnn_model)
#' sits_plot(class.tb)
#' }
#' @export
sits_FCN <- function(data         = NULL,
                     layers           = c(128, 256, 128),
                     kernels          = c(9, 7, 5),
                     activation       = 'relu',
                     L2_rate          = 1e-06,
                     optimizer        = keras::optimizer_adam(lr = 0.001),
                     epochs           = 150,
                     batch_size       = 128,
                     validation_split = 0.2,
                     verbose          = 1,
                     binary_classification = FALSE) {
    # backward compatibility
    if ("coverage" %in% names(data))
        data <- .sits_tibble_rename(data)

    # function that returns keras model based on a sits sample data.table
    result_fun <- function(data){
        # pre-conditions
        ensurer::ensure_that(layers, length(.) == length(kernels),
                             err_desc = "sits_FCN: number of 1D layers must match number of 1D kernels")
        valid_activations <- c("relu", "elu", "selu", "sigmoid")
        ensurer::ensure_that(activation, all(. %in% valid_activations),
                             err_desc = "sits_FCN: invalid CNN activation method")

        # data normalization
        stats <- .sits_normalization_param(data)
        # obtain the distances used for training and test
        train_data_DT <- .sits_distances(.sits_normalize_data(data, stats))

        # is the input data consistent?
        ensurer::ensure_that(train_data_DT, "reference" %in% names(.),
                             err_desc = "sits_CNN: input data does not contain distances")

        # get the labels of the data
        labels <- sits_labels(data)$label

        # create a named vector with integers match the class labels
        n_labels <- length(labels)
        int_labels <- c(1:n_labels)
        names(int_labels) <- labels

        # number of bands and number of samples
        n_bands <- length(sits_bands(data))
        n_timesteps <- nrow(sits_time_series(data[1,]))

        # split the data into training and validation data sets
        # create partitions different splits of the input data
        test_data_DT <- .sits_sample_distances(train_data_DT, frac = validation_split)

        # remove the lines used for validation
        train_data_DT <- train_data_DT[!test_data_DT, on = "original_row"]

        # shuffle the data
        train_data_DT <- train_data_DT[sample(nrow(train_data_DT), nrow(train_data_DT)),]
        test_data_DT  <- test_data_DT[sample(nrow(test_data_DT), nrow(test_data_DT)),]

        # organize training and test data
        # tranform the training data into a 3D array
        n_samples_train <- nrow(train_data_DT)
        train.x <- array(data = as.matrix(train_data_DT[,3:ncol(train_data_DT)]),
                         dim = c(n_samples_train, n_timesteps, n_bands))
        # the training labels are stored as a 1D array
        train.y <- unname(int_labels[as.vector(train_data_DT$reference)]) - 1

        # tranform the test data into a 3D array
        n_samples_test <- nrow(test_data_DT)
        test.x <- array(data = as.matrix(test_data_DT[,3:ncol(test_data_DT)]),
                        dim = c(n_samples_test, n_timesteps, n_bands))
        # the test labels are stored as a 1D array
        test.y <- unname(int_labels[as.vector(test_data_DT$reference)]) - 1

        # build the model step by step
        # create the input_tensor for 1D convolution
        input_tensor  <- keras::layer_input(shape = c(n_timesteps, n_bands))
        output_tensor <- input_tensor

        # build the 1D nodes
        for (i in 1:length(layers)) {
            # Add a Convolution1D layer
            output_tensor <- keras::layer_conv_1d(output_tensor, filters = layers[i],
                                                  kernel_size = kernels[i],
                                                  kernel_regularizer = keras::regularizer_l2(l = L2_rate))
            # Batch normalization
            output_tensor <- keras::layer_batch_normalization(output_tensor)
            # activation
            output_tensor <- keras::layer_activation(output_tensor, activation = activation)
        }
        # Apply max pooling?
        output_tensor <- keras::layer_global_average_pooling_1d(output_tensor)

        # reshape a tensor into a 2D shape
        output_tensor <- keras::layer_flatten(output_tensor)

        # create the final tensor
        model_loss <- ""
        if (binary_classification && n_labels == 2) {
            output_tensor <- keras::layer_dense(output_tensor, units = 1, activation = "sigmoid")
            model_loss <- "binary_crossentropy"
        }
        else {
            output_tensor <- keras::layer_dense(output_tensor, units = n_labels, activation = "softmax")
            model_loss <- "categorical_crossentropy"
            # keras requires categorical data to be put in a matrix
            train.y <- keras::to_categorical(train.y, n_labels)
            test.y  <- keras::to_categorical(test.y, n_labels)
        }
        # create the model
        model.keras <- keras::keras_model(input_tensor, output_tensor)
        # compile the model
        model.keras %>% keras::compile(
            loss = model_loss,
            optimizer = optimizer,
            metrics = "accuracy"
        )

        # fit the model
        history <- model.keras %>% keras::fit(
            train.x, train.y,
            epochs = epochs, batch_size = batch_size,
            validation_data = list(test.x, test.y),
            verbose = verbose, view_metrics = "auto"
        )

        # show training evolution
        graphics::plot(history)

        # construct model predict closure function and returns
        model_predict <- function(values_DT){
            # transform input (data.table) into a 3D tensor (remove first two columns)
            n_samples <- nrow(values_DT)
            n_timesteps <- nrow(sits_time_series(data[1,]))
            n_bands <- length(sits_bands(data))
            values.x <- array(data = as.matrix(values_DT[,3:ncol(values_DT)]),
                              dim = c(n_samples, n_timesteps, n_bands))
            # retrieve the prediction probabilities
            prediction_DT <- data.table::as.data.table(stats::predict(model.keras, values.x))
            # adjust the names of the columns of the probs
            colnames(prediction_DT) <- labels

            return(prediction_DT)
        }

        return(model_predict)
    }

    result <- .sits_factory_function(data, result_fun)
    return(result)
}


#' @title Train a model using the ResNet model
#' @name sits_ResNet
#'
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Alexandre Xavier Ywata de Carvalho, \email{alexandre.ywata@@ipea.gov.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Use a ResNet architecture for classifiying satellite image time series.
#' The ResNet (or deep residual network) was proposed by a team in Microsoft Research
#' for 2D image classification. ResNet tries to address the degradation of accuracy
#' in a deep network. The idea is to replace a deep network with a combination of shallow
#' ones. In the paper by Fawaz et al. (2019), ResNet was considered the best method for
#' time series classification, using the UCR dataset. Please refer to the paper for
#' more details.
#'
#' The SITS implementation of RestNet is based on the work of Hassan Fawaz and
#' collaborators, and also inspired by the paper of Wang et al (see below). Fawaz provides
#' a reference Keras implementation of ResNet in https://github.com/hfawaz/dl-4-tsc.
#' If you use this function, please cite the references.
#'
#' @references Hassan Fawaz, Germain Forestier, Jonathan Weber, Lhassane Idoumghar,  and Pierre-Alain Muller,
#' "Deep learning for time series classification: a review",
#' Data Mining and Knowledge Discovery, 33(4): 917--963, 2019.
#'
#' Zhiguang Wang, Weizhong Yan, and Tim Oates,
#' "Time series classification from scratch with deep neural networks: A strong baseline",
#'  2017 international joint conference on neural networks (IJCNN).
#'
#' @param data              Time series with the training samples.
#' @param blocks            Vector with number of 1D convolutional filters of each block of three layers.
#' @param kernels           Vector with the size of the 1D convolutional kernels for each layer of each block.
#' @param activation        Activation function for 1D convolution.
#'                          Valid values are {'relu', 'elu', 'selu', 'sigmoid'}.
#' @param optimizer         Function with a pointer to the optimizer function (default is optimization_adam()).
#'                          Options are optimizer_adadelta(), optimizer_adagrad(), optimizer_adam(),
#'                          optimizer_adamax(), optimizer_nadam(), optimizer_rmsprop(), optimizer_sgd()
#' @param epochs            Number of iterations to train the model.
#' @param batch_size        Number of samples per gradient update.
#' @param validation_split  Number between 0 and 1. Fraction of the training data to be used as validation data.
#'                          The model will set apart this fraction of the training data, will not train on it,
#'                          and will evaluate the loss and any model metrics on this data at the end of each epoch.
#'                          The validation data is selected from the last samples in the x and y data provided,
#'                          before shuffling.
#' @param verbose           Verbosity mode (0 = silent, 1 = progress bar, 2 = one line per epoch).
#' @param binary_classification A lenght-one logical indicating if this is a binary classification. If it is so,
#'                          the number of unique labels in the training data must be two as well.
#'
#' @return A model fitted to input data to be passed to \code{\link[sits]{sits_classify}}
#'
#' @examples
#' \donttest{
#' # Retrieve the set of samples for the Mato Grosso region (provided by EMBRAPA)
#' samples_2bands <- sits_select_bands(samples_mt_6bands, ndvi, evi)
#'
#' # Build a machine learning model based on deep learning
#' dl_model <- sits_train (samples_2bands, sits_ResNet())
#'
#' # get a point and classify the point with the ml_model
#' point.tb <- sits_select_bands(point_mt_6bands, ndvi, evi)
#' class.tb <- sits_classify(point.tb, dl_model)
#' sits_plot(class.tb)
#' }
#' @export
sits_ResNet <- function(data              = NULL,
                        blocks           = c(64, 128, 128),
                        kernels          = c(8, 5, 3),
                        activation       = 'relu',
                        optimizer        = keras::optimizer_adam(lr = 0.001),
                        epochs           = 150,
                        batch_size       = 128,
                        validation_split = 0.2,
                        verbose          = 1,
                        binary_classification = FALSE) {
    # backward compatibility
    if ("coverage" %in% names(data))
        data <- .sits_tibble_rename(data)

    # function that returns keras model based on a sits sample data.table
    result_fun <- function(data){
        valid_activations <- c("relu", "elu", "selu", "sigmoid")
        # pre-conditions
        ensurer::ensure_that(activation, (.) %in% valid_activations,
                             err_desc = "sits_ResNet: invalid CNN activation method")

        ensurer::ensure_that(kernels, length(.) == 3,
                             err_desc = "sits_ResNet: should inform size of three kernels")

        # data normalization
        stats <- .sits_normalization_param(data)
        # obtain the distances used for training and test
        train_data_DT <- .sits_distances(.sits_normalize_data(data, stats))

        # is the input data consistent?
        ensurer::ensure_that(train_data_DT, "reference" %in% names(.),
                             err_desc = "sits_ResNet: input data does not contain distances")

        # get the labels of the data
        labels <- sits_labels(data)$label

        # create a named vector with integers match the class labels
        n_labels <- length(labels)
        int_labels <- c(1:n_labels)
        names(int_labels) <- labels

        # number of bands and number of samples
        n_bands <- length(sits_bands(data))
        n_timesteps <- nrow(sits_time_series(data[1,]))

        # split the data into training and validation data sets
        # create partitions different splits of the input data
        test_data_DT <- .sits_sample_distances(train_data_DT, frac = validation_split)

        # remove the lines used for validation
        train_data_DT <- train_data_DT[!test_data_DT, on = "original_row"]

        # shuffle the data
        train_data_DT <- train_data_DT[sample(nrow(train_data_DT), nrow(train_data_DT)),]
        test_data_DT  <- test_data_DT[sample(nrow(test_data_DT), nrow(test_data_DT)),]

        # organize training and test data
        # tranform the training data into a 3D array
        n_samples_train <- nrow(train_data_DT)
        train.x <- array(data = as.matrix(train_data_DT[,3:ncol(train_data_DT)]),
                         dim = c(n_samples_train, n_timesteps, n_bands))
        # the training labels are stored as a 1D array
        train.y <- unname(int_labels[as.vector(train_data_DT$reference)]) - 1

        # tranform the test data into a 3D array
        n_samples_test <- nrow(test_data_DT)
        test.x <- array(data = as.matrix(test_data_DT[,3:ncol(test_data_DT)]),
                        dim = c(n_samples_test, n_timesteps, n_bands))
        # the test labels are stored as a 1D array
        test.y <- unname(int_labels[as.vector(test_data_DT$reference)]) - 1

        # build the model step by step
        # create the input_tensor for 1D convolution
        input_tensor  <- keras::layer_input(shape = c(n_timesteps, n_bands))

        # initial assignment
        output_tensor <- input_tensor
        shortcut <- input_tensor

        for (i in 1:length(blocks)) {
            # Add a Convolution1D
            output_tensor_x <- keras::layer_conv_1d(output_tensor, filters = blocks[i],
                                                    kernel_size = kernels[1],
                                                    padding = "same")
            # normalization
            output_tensor_x <- keras::layer_batch_normalization(output_tensor_x)

            # activation
            output_tensor_x <- keras::layer_activation(output_tensor_x, activation = activation)

            # Add a new convolution
            output_tensor_y <- keras::layer_conv_1d(output_tensor_x, filters = blocks[i],
                                                    kernel_size = kernels[2],
                                                    padding = "same")
            # normalization
            output_tensor_y <- keras::layer_batch_normalization(output_tensor_y)

            # activation
            output_tensor_y <- keras::layer_activation(output_tensor_y, activation = activation)

            # Add a third convolution
            output_tensor_z <- keras::layer_conv_1d(output_tensor_y, filters = blocks[i],
                                                    kernel_size = kernels[3],
                                                    padding = "same")
            output_tensor_z <- keras::layer_batch_normalization(output_tensor_z)

            # include the shortcut
            shortcut <- keras::layer_conv_1d(shortcut, filters = blocks[i],
                                             kernel_size = 1,
                                             padding = "same")
            shortcut <- keras::layer_batch_normalization(shortcut)

            # get the output tensor
            output_tensor <- keras::layer_add(list(shortcut, output_tensor_z))
            output_tensor <- keras::layer_activation(output_tensor, activation = activation)
            shortcut <- output_tensor
        }

        # reshape a tensor into a 2D shape
        output_tensor <- keras::layer_global_average_pooling_1d(output_tensor)

        # create the final tensor
        model_loss <- ""
        if (binary_classification && n_labels == 2) {
            output_tensor <- keras::layer_dense(output_tensor, units = 1, activation = "sigmoid")
            model_loss <- "binary_crossentropy"
        }
        else {
            output_tensor <- keras::layer_dense(output_tensor, units = n_labels, activation = "softmax")
            model_loss <- "categorical_crossentropy"
            # keras requires categorical data to be put in a matrix
            train.y <- keras::to_categorical(train.y, n_labels)
            test.y  <- keras::to_categorical(test.y, n_labels)
        }
        # create the model
        model.keras <- keras::keras_model(input_tensor, output_tensor)
        # compile the model
        model.keras %>% keras::compile(
            loss = model_loss,
            optimizer = optimizer,
            metrics = "accuracy"
        )

        # fit the model
        history <- model.keras %>% keras::fit(
            train.x, train.y,
            epochs = epochs, batch_size = batch_size,
            validation_data = list(test.x, test.y),
            verbose = verbose, view_metrics = "auto"
        )
        # show training evolution
        graphics::plot(history)

        # construct model predict closure function and returns
        model_predict <- function(values_DT){
            # transform input (data.table) into a 3D tensor (remove first two columns)
            n_samples <- nrow(values_DT)
            n_timesteps <- nrow(sits_time_series(data[1,]))
            n_bands <- length(sits_bands(data))
            values.x <- array(data = as.matrix(values_DT[,3:ncol(values_DT)]),
                              dim = c(n_samples, n_timesteps, n_bands))
            # retrieve the prediction probabilities
            prediction_DT <- data.table::as.data.table(stats::predict(model.keras, values.x))
            # adjust the names of the columns of the probs
            colnames(prediction_DT) <- labels

            return(prediction_DT)
        }

        return(model_predict)
    }

    result <- .sits_factory_function(data, result_fun)
    return(result)
}
#' @title Train a model using the Temporal Convolutional Neural Network
#' @name sits_TempCNN
#'
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Alexandre Xavier Ywata de Carvalho, \email{alexandre.ywata@@ipea.gov.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Use a TempCNN algorithm to classify data. The tempCNN algorithm has
#' two stages: a 1D CNN is applied to the input time series data is combined with a
#' multi-layer perceptron. Users can define the depth of the 1D network, as well as
#' the number of perceptron layers.
#'
#' This function is based on the paper by Charlotte Pelletier referenced below
#' and the code made available on github (https://github.com/charlotte-pel/temporalCNN)
#' If you use this method, please cite the original tempCNN paper.
#'
#' @references Charlotte Pelletier, Geoffrey Webb and François Petitjean,
#' "Temporal Convolutional Neural Network for the Classification of Satellite Image Time Series",
#' Remote Sensing, 11,523, 2019. DOI: 10.3390/rs11050523.
#'
#' @param data              Time series with the training samples.
#' @param cnn_layers        Vector with the number of 1D convolutional filters per layer
#' @param cnn_kernels       Vector with the size of the 1D convolutional kernels.
#' @param cnn_activation    Activation function for 1D convolution.
#'                          Valid values are {'relu', 'elu', 'selu', 'sigmoid'}.
#' @param cnn_L2_rate       Regularization rate for 1D convolution.
#'                          Valid values are {'relu', 'elu', 'selu', 'sigmoid'}.
#' @param cnn_dropout_rates Vector with dropout rates for 1D convolutional filters.
#' @param mlp_layers        Vector with the number of hidden nodes in the MLP (multi-layer-perceptron).
#' @param mlp_activation    Names of 2D activation functions for the MLP. Valid values are {'relu', 'elu', 'selu', 'sigmoid'}.
#' @param mlp_dropout_rates Vector with the dropout rates (0,1) for each layer in the MLP.
#' @param optimizer         Function with a pointer to the optimizer function (default is optimization_adam()).
#'                          Options are optimizer_adadelta(), optimizer_adagrad(), optimizer_adam(),
#'                          optimizer_adamax(), optimizer_nadam(), optimizer_rmsprop(), optimizer_sgd()
#' @param epochs            Number of iterations to train the model.
#' @param batch_size        Number of samples per gradient update.
#' @param validation_split  Number between 0 and 1. Fraction of the training data to be used as validation data.
#'                          The model will set apart this fraction of the training data, will not train on it,
#'                          and will evaluate the loss and any model metrics on this data at the end of each epoch.
#'                          The validation data is selected from the last samples in the x and y data provided,
#'                          before shuffling.
#' @param verbose           Verbosity mode (0 = silent, 1 = progress bar, 2 = one line per epoch).
#' @param binary_classification A lenght-one logical indicating if this is a binary classification. If it is so,
#'
#' @return A model fitted to input data to be passed to \code{\link[sits]{sits_classify}}
#'
#' @examples
#' \donttest{
#' # Retrieve the set of samples for the Mato Grosso region (provided by EMBRAPA)
#' samples_2bands <- sits_select_bands(samples_mt_6bands, ndvi, evi)
#'
#' # Build a machine learning model based on deep learning
#' dl_model <- sits_train (samples_2bands, sits_TempCNN())
#'
#' # get a point and classify the point with the ml_model
#' point.tb <- sits_select_bands(point_mt_6bands, ndvi, evi)
#' class.tb <- sits_classify(point.tb, dl_model)
#' sits_plot(class.tb)
#' }
#' @export
sits_TempCNN <- function(data                 = NULL,
                         cnn_layers           = c(64, 64, 64),
                         cnn_kernels          = c(5, 5, 5),
                         cnn_activation       = 'relu',
                         cnn_L2_rate          = 1e-06,
                         cnn_dropout_rates    = c(0.50, 0.50, 0.50),
                         mlp_layers           = c(256),
                         mlp_activation       = 'relu',
                         mlp_dropout_rates    = c(0.50),
                         optimizer            = keras::optimizer_adam(lr = 0.001),
                         epochs               = 150,
                         batch_size           = 128,
                         validation_split     = 0.2,
                         verbose              = 1,
                         binary_classification = FALSE) {
    # backward compatibility
    if ("coverage" %in% names(data))
        data <- .sits_tibble_rename(data)

    # function that returns keras model based on a sits sample data.table
    result_fun <- function(data){
        # data normalization
        stats <- .sits_normalization_param(data)
        # obtain the distances used for training and test
        train_data_DT <- .sits_distances(.sits_normalize_data(data, stats))

        valid_activations <- c("relu", "elu", "selu", "sigmoid")

        # is the input data consistent?
        ensurer::ensure_that(train_data_DT, "reference" %in% names(.),
                             err_desc = "sits_tempCNN: input data does not contain distances")

        ensurer::ensure_that(cnn_layers, length(.) == length(cnn_kernels),
                             err_desc = "sits_tempCNN: number of 1D layers must match number of 1D kernel sizes")
        ensurer::ensure_that(cnn_layers, length(.) == length(cnn_dropout_rates),
                             err_desc = "sits_tempCNN: number of 1D layers must match number of 1D dropout rates")
        ensurer::ensure_that(mlp_layers, length(.) == length(mlp_dropout_rates),
                             err_desc = "sits_tempCNN: number of 2D units must match number of 2D dropout rates")

        ensurer::ensure_that(cnn_activation, (.) %in% valid_activations,
                             err_desc = "sits_tempCNN: invalid CNN activation method")

        ensurer::ensure_that(mlp_activation, (.) %in% valid_activations,
                             err_desc = "sits_tempCNN: invalid node activation method")

        # get the labels of the data
        labels <- sits_labels(data)$label

        # create a named vector with integers match the class labels
        n_labels <- length(labels)
        int_labels <- c(1:n_labels)
        names(int_labels) <- labels

        # number of bands and number of samples
        n_bands <- length(sits_bands(data))
        n_timesteps <- nrow(sits_time_series(data[1,]))

        # split the data into training and validation data sets
        # create partitions different splits of the input data
        test_data_DT <- .sits_sample_distances(train_data_DT, frac = validation_split)

        # remove the lines used for validation
        train_data_DT <- train_data_DT[!test_data_DT, on = "original_row"]

        # shuffle the data
        train_data_DT <- train_data_DT[sample(nrow(train_data_DT), nrow(train_data_DT)),]
        test_data_DT  <- test_data_DT[sample(nrow(test_data_DT), nrow(test_data_DT)),]

        # organize training and test data
        # tranform the training data into a 3D array
        n_samples_train <- nrow(train_data_DT)
        train.x <- array(data = as.matrix(train_data_DT[,3:ncol(train_data_DT)]),
                         dim = c(n_samples_train, n_timesteps, n_bands))
        # the training labels are stored as a 1D array
        train.y <- unname(int_labels[as.vector(train_data_DT$reference)]) - 1

        # tranform the test data into a 3D array
        n_samples_test <- nrow(test_data_DT)
        test.x <- array(data = as.matrix(test_data_DT[,3:ncol(test_data_DT)]),
                        dim = c(n_samples_test, n_timesteps, n_bands))
        # the test labels are stored as a 1D array
        test.y <- unname(int_labels[as.vector(test_data_DT$reference)]) - 1

        # build the model step by step
        # create the input_tensor for 1D convolution
        input_tensor  <- keras::layer_input(shape = c(n_timesteps, n_bands))
        output_tensor <- input_tensor

        # build the 1D nodes
        for (i in 1:length(cnn_layers)) {
            # Add a Convolution1D
            output_tensor <- keras::layer_conv_1d(output_tensor, filters = cnn_layers[i],
                                                  kernel_size = cnn_kernels[i],
                                                  kernel_regularizer = keras::regularizer_l2(l = cnn_L2_rate))
            # Apply layer dropout
            output_tensor <- keras::layer_dropout(output_tensor, rate = cnn_dropout_rates[i])
            # Activation
            output_tensor <- keras::layer_activation(output_tensor, activation = cnn_activation)
        }

        # reshape a tensor into a 2D shape
        output_tensor <- keras::layer_flatten(output_tensor)

        # build the 2D nodes
        for (i in 1:length(mlp_layers)) {
            output_tensor <- keras::layer_dense(output_tensor, units = mlp_layers[i], activation = mlp_activation)
            output_tensor <- keras::layer_dropout(output_tensor, rate = mlp_dropout_rates[i])
            output_tensor <- keras::layer_batch_normalization(output_tensor)
        }

        # create the final tensor
        model_loss <- ""
        if (binary_classification && n_labels == 2) {
            output_tensor <- keras::layer_dense(output_tensor, units = 1, activation = "sigmoid")
            model_loss <- "binary_crossentropy"
        }
        else {
            output_tensor <- keras::layer_dense(output_tensor, units = n_labels, activation = "softmax")
            model_loss <- "categorical_crossentropy"
            # keras requires categorical data to be put in a matrix
            train.y <- keras::to_categorical(train.y, n_labels)
            test.y  <- keras::to_categorical(test.y, n_labels)
        }
        # create the model
        model.keras <- keras::keras_model(input_tensor, output_tensor)
        # compile the model
        model.keras %>% keras::compile(
            loss = model_loss,
            optimizer = optimizer,
            metrics = "accuracy"
        )

        # fit the model
        history <- model.keras %>% keras::fit(
            train.x, train.y,
            epochs = epochs, batch_size = batch_size,
            validation_data = list(test.x, test.y),
            verbose = verbose, view_metrics = "auto"
        )

        # show training evolution
        graphics::plot(history)

        # construct model predict closure function and returns
        model_predict <- function(values_DT){
            # transform input (data.table) into a 3D tensor (remove first two columns)
            n_samples <- nrow(values_DT)
            n_timesteps <- nrow(sits_time_series(data[1,]))
            n_bands <- length(sits_bands(data))
            values.x <- array(data = as.matrix(values_DT[,3:ncol(values_DT)]),
                              dim = c(n_samples, n_timesteps, n_bands))
            # retrieve the prediction probabilities
            prediction_DT <- data.table::as.data.table(stats::predict(model.keras, values.x))
            # adjust the names of the columns of the probs
            colnames(prediction_DT) <- labels

            return(prediction_DT)
        }

        return(model_predict)
    }

    result <- .sits_factory_function(data, result_fun)
    return(result)
}

#' @title Train a model using the a combination of LSTM and CNN
#' @name sits_LSTM_FCN
#'
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Alexandre Xavier Ywata de Carvalho, \email{alexandre.ywata@@ipea.gov.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Use a combination of an LSTM (Long Short Term Memory) and a
#' cascade of 1D-CNN newtorks to classify data. Users can define the number of
#' convolutional layers, the size of the convolutional
#' kernels, and the activation functions.
#'
#' #' This function is based on the paper by Karim et al. referenced below
#' and the code made available on github (https://github.com/titu1994/LSTM-FCN)
#' If you use this method, please cite the original paper.
#'
#' @references Fazle Karim, Somshubra Majumdar, Houshang Darabi, Sun Chen,
#' "LSTM fully convolutional networks for time series classification", IEEE Access, 6(1662-1669), 2018.
#'
#'
#' @param data              Time series with the training samples.
#' @param lstm_units        Number of cells in the each LSTM layer
#' @param lstm_dropout      Dropout rate of the LSTM module
#' @param cnn_layers        Vector with the number of filters for each 1D CNN layer.
#' @param cnn_kernels       Vector with the size of the 1D convolutional kernels.
#' @param activation        Activation function for 1D convolution.
#'                          Valid values are {'relu', 'elu', 'selu', 'sigmoid'}.
#' @param optimizer         Function with a pointer to the optimizer function (default is optimization_adam()).
#'                          Options are optimizer_adadelta(), optimizer_adagrad(), optimizer_adam(),
#'                          optimizer_adamax(), optimizer_nadam(), optimizer_rmsprop(), optimizer_sgd()
#' @param epochs            Number of iterations to train the model.
#' @param batch_size        Number of samples per gradient update.
#' @param validation_split  Number between 0 and 1. Fraction of the training data to be used as validation data.
#'                          The model will set apart this fraction of the training data, will not train on it,
#'                          and will evaluate the loss and any model metrics on this data at the end of each epoch.
#'                          The validation data is selected from the last samples in the x and y data provided,
#'                          before shuffling.
#' @param verbose           Verbosity mode (0 = silent, 1 = progress bar, 2 = one line per epoch).
#' @param binary_classification A lenght-one logical indicating if this is a binary classification. If it is so,
#'                          the number of unique labels in the training data must be two as well.
#'
#' @return A model fitted to input data to be passed to \code{\link[sits]{sits_classify}}
#'
#' @examples
#' \donttest{
#' # Retrieve the set of samples for the Mato Grosso region (provided by EMBRAPA)
#' samples_2bands <- sits_select_bands(samples_mt_6bands, ndvi, evi)
#'
#' # Build a machine learning model based on deep learning
#' lstm_cnn_model <- sits_train (samples_2bands, sits_LSTM_FCN())
#'
#' # get a point and classify the point with the ml_model
#' point.tb <- sits_select_bands(point_mt_6bands, ndvi, evi)
#' class.tb <- sits_classify(point.tb, lstm_cnn_model)
#' sits_plot(class.tb)
#' }
#' @export
sits_LSTM_FCN <- function(data                =  NULL,
                          lstm_units          = 8,
                          lstm_dropout        = 0.80,
                          cnn_layers          = c(128, 256, 128),
                          cnn_kernels         = c(8, 5, 3),
                          activation          = 'relu',
                          optimizer           = keras::optimizer_adam(lr = 0.001),
                          epochs              = 150,
                          batch_size          = 128,
                          validation_split    = 0.2,
                          verbose             = 1,
                          binary_classification = FALSE) {
    # backward compatibility
    if ("coverage" %in% names(data))
        data <- .sits_tibble_rename(data)

    # function that returns keras model based on a sits sample data.table
    result_fun <- function(data){
        # data normalization
        stats <- .sits_normalization_param(data)
        # obtain the distances used for training and test
        train_data_DT <- .sits_distances(.sits_normalize_data(data, stats))

        valid_activations <- c("relu", "elu", "selu", "sigmoid")

        # is the input data consistent?
        ensurer::ensure_that(train_data_DT, "reference" %in% names(.),
                             err_desc = "sits_LSTM_FCN: input data does not contain distances")

        ensurer::ensure_that(cnn_layers, length(.) == length(cnn_kernels),
                             err_desc = "sits_LSTM_FCN: number of 1D CNN layers must match number of 1D kernels")

        ensurer::ensure_that(activation, all(. %in% valid_activations),
                             err_desc = "sits_LSTM_FCN: invalid CNN activation method")

        # get the labels of the data
        labels <- sits_labels(data)$label

        # create a named vector with integers match the class labels
        n_labels <- length(labels)
        int_labels <- c(1:n_labels)
        names(int_labels) <- labels

        # number of bands and number of samples
        n_bands <- length(sits_bands(data))
        n_timesteps <- nrow(sits_time_series(data[1,]))

        # split the data into training and validation data sets
        # create partitions different splits of the input data
        test_data_DT <- .sits_sample_distances(train_data_DT, frac = validation_split)

        # remove the lines used for validation
        train_data_DT <- train_data_DT[!test_data_DT, on = "original_row"]

        # shuffle the data
        train_data_DT <- train_data_DT[sample(nrow(train_data_DT), nrow(train_data_DT)),]
        test_data_DT  <- test_data_DT[sample(nrow(test_data_DT), nrow(test_data_DT)),]

        # organize training and test data
        # tranform the training data into a 3D array
        n_samples_train <- nrow(train_data_DT)
        train.x <- array(data = as.matrix(train_data_DT[,3:ncol(train_data_DT)]),
                         dim = c(n_samples_train, n_timesteps, n_bands))
        # the training labels are stored as a 1D array
        train.y <- unname(int_labels[as.vector(train_data_DT$reference)]) - 1

        # tranform the test data into a 3D array
        n_samples_test <- nrow(test_data_DT)
        test.x <- array(data = as.matrix(test_data_DT[,3:ncol(test_data_DT)]),
                        dim = c(n_samples_test, n_timesteps, n_bands))
        # the test labels are stored as a 1D array
        test.y <- unname(int_labels[as.vector(test_data_DT$reference)]) - 1

        # build the model step by step
        # create the input_tensor for 1D convolution
        input_tensor  <- keras::layer_input(shape = c(n_timesteps, n_bands))
        output_tensor <- input_tensor

        # build the LSTM node
        lstm_layer <- keras::layer_permute(input_tensor, dims = c(2,1))
        lstm_layer <- keras::layer_lstm(input_tensor, units = lstm_units, dropout = lstm_dropout)

        # build the 1D nodes
        for (i in 1:length(cnn_layers)) {
            # Add a 1D CNN layer
            output_tensor <- keras::layer_conv_1d(output_tensor, filters = cnn_layers[i],
                                                  kernel_size = cnn_kernels[i])

            # batch normalisation
            output_tensor <- keras::layer_batch_normalization(output_tensor)
            # Layer activation
            output_tensor <- keras::layer_activation(output_tensor, activation = activation)
        }

        # Apply average pooling
        output_tensor <- keras::layer_global_average_pooling_1d(output_tensor)

        # Concatenate LSTM and CNN
        output_tensor <- keras::layer_concatenate(list(lstm_layer, output_tensor))

        # reshape a tensor into a 2D shape
        output_tensor <- keras::layer_flatten(output_tensor)

        # create the final tensor
        model_loss <- ""
        if (binary_classification && n_labels == 2) {
            output_tensor <- keras::layer_dense(output_tensor, units = 1, activation = "sigmoid")
            model_loss <- "binary_crossentropy"
        }
        else {
            output_tensor <- keras::layer_dense(output_tensor, units = n_labels, activation = "softmax")
            model_loss <- "categorical_crossentropy"
            # keras requires categorical data to be put in a matrix
            train.y <- keras::to_categorical(train.y, n_labels)
            test.y  <- keras::to_categorical(test.y, n_labels)
        }
        # create the model
        model.keras <- keras::keras_model(input_tensor, output_tensor)
        # compile the model
        model.keras %>% keras::compile(
            loss = model_loss,
            optimizer = optimizer,
            metrics = "accuracy"
        )

        # fit the model
        history <- model.keras %>% keras::fit(
            train.x, train.y,
            epochs = epochs, batch_size = batch_size,
            validation_data = list(test.x, test.y),
            verbose = verbose, view_metrics = "auto"
        )

        # show training evolution
        graphics::plot(history)

        # construct model predict closure function and returns
        model_predict <- function(values_DT){
            # transform input (data.table) into a 3D tensor (remove first two columns)
            n_samples <- nrow(values_DT)
            n_timesteps <- nrow(sits_time_series(data[1,]))
            n_bands <- length(sits_bands(data))
            values.x <- array(data = as.matrix(values_DT[,3:ncol(values_DT)]),
                              dim = c(n_samples, n_timesteps, n_bands))
            # retrieve the prediction probabilities
            prediction_DT <- data.table::as.data.table(stats::predict(model.keras, values.x))
            # adjust the names of the columns of the probs
            colnames(prediction_DT) <- labels

            return(prediction_DT)
        }

        return(model_predict)
    }

    result <- .sits_factory_function(data, result_fun)
    return(result)
}
