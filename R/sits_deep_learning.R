#' @title Train a  deep learning model using multi-layer perceptron
#' @name sits_deeplearning
#'
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Alexandre Ywata de Carvalho, \email{alexandre.ywata@@ipea.gov.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Use a multi-layer perceptron algorithm to classify data.
#' This function is a front-end to the "keras" method R package.
#' Please refer to the documentation in that package for more details.
#'
#' @param samples           Time series with the training samples.
#' @param layers            Vector with number of hidden nodes in each layer.
#' @param activation        Vector with the names of activation functions.
#'                          Valid values are {'relu', 'elu', 'selu', 'sigmoid'}.
#' @param dropout_rates     Vector with the dropout rates (0,1)
#'                          for each layer.
#' @param optimizer         Function with a pointer to the optimizer function
#'                          (default is optimization_adam()).
#'                          Options are optimizer_adadelta(),
#'                          optimizer_adagrad(), optimizer_adam(),
#'                          optimizer_adamax(), optimizer_nadam(),
#'                          optimizer_rmsprop(), optimizer_sgd()
#' @param epochs            Number of iterations to train the model.
#' @param batch_size        Number of samples per gradient update.
#' @param validation_split  Number between 0 and 1.
#'                          Fraction of the training data for validation.
#'                          The model will set apart this fraction
#'                          and will evaluate the loss and any model metrics
#'                          on this data at the end of each epoch.
#' @param verbose           Verbosity mode (0 = silent,
#'                          1 = progress bar, 2 = one line per epoch).
#' @return                  Either a model to be passed in sits_predict
#'                          or a function prepared to be called further.
#'
#' @note
#' The parameters for the MLP have been chosen based on the work by Wang et al. 2017
#' that takes multilayer perceptrons as the baseline for time series classifications:
#' (a) Four layers with 512 neurons each, specified by the parameter `layers`;
#' (b) Using the 'elu' activation function;
#' (c) dropout rates of 10%, 20%, 20%, and 30% for the layers;
#' (d) the "optimizer_adam" as optimizer (default value);
#' (e) a number of training steps (`epochs`) of 150;
#' (f) a `batch_size` of 64, which indicates how many time series
#' are used for input at a given steps;
#' (g) a validation percentage of 20%, which means 20% of the samples
#' will be randomly set side for validation.
#'
#' @references
#' Hassan Fawaz, Germain Forestier, Jonathan Weber,
#' Lhassane Idoumghar,  and Pierre-Alain Muller,
#' "Deep learning for time series classification: a review",
#' Data Mining and Knowledge Discovery, 33(4): 917--963, 2019.
#'
#' Zhiguang Wang, Weizhong Yan, and Tim Oates,
#' "Time series classification from scratch with deep neural networks:
#' A strong baseline",
#' 2017 international joint conference on neural networks (IJCNN).
#'
#' Implementation based on the python keras implementation provided in
#' https://github.com/hfawaz/dl-4-tsc.
#'
#' @examples
#' \dontrun{
#' # Retrieve the set of samples for the Mato Grosso region
#' data(samples_modis_4bands)
#' samples_mt_ndvi <- sits_select(samples_modis_4bands, bands = "NDVI")
#' # Build a machine learning model based on deep learning
#' dl_model <- sits_train(
#'     samples_mt_ndvi,
#'     sits_deeplearning(
#'         layers = c(64, 64),
#'         dropout_rates = c(0.50, 0.40),
#'         epochs = 50
#'     )
#' )
#' # get a point with a 16 year time series
#' point_ndvi <- sits_select(point_mt_6bands, bands = "NDVI")
#' # classify the point
#' point_class <- sits_classify(point_ndvi, dl_model)
#' # plot the classified point
#' plot(point_class)
#' }
#' @export
#'
sits_deeplearning <- function(samples = NULL,
                              layers = c(512, 512, 512, 512),
                              activation = "elu",
                              dropout_rates = c(0.10, 0.20, 0.20, 0.30),
                              optimizer = keras::optimizer_adam(lr = 0.001),
                              epochs = 200,
                              batch_size = 64,
                              validation_split = 0.2,
                              verbose = 0) {

    # function that returns a keras model based on samples
    result_fun <- function(data) {

        # verifies if keras package is installed
        if (!requireNamespace("keras", quietly = TRUE)) {
            stop(paste("keras required for this function to work.",
                       "Please install it."), call. = FALSE)
        }

        # pre-conditions
        assertthat::assert_that(
            length(layers) == length(dropout_rates),
            msg = "sits_deeplearning: number of layers does not match
                        number of dropout rates"
        )
        assertthat::assert_that(
            length(activation) == 1,
            msg = "sits_deeplearning: use only one activation function"
        )
        valid_activations <- c("relu", "elu", "selu", "sigmoid")
        assertthat::assert_that(
            activation %in% valid_activations,
            msg = "sits_deeplearning: invalid node activation method"
        )
        # data normalization
        stats <- .sits_normalization_param(data)
        train_data <- .sits_distances(.sits_normalize_data(data, stats))

        # is the training data correct?
        assertthat::assert_that(
            "reference" %in% names(train_data),
            msg = "sits_deeplearning:
                   input data does not contain distances"
        )

        # get the labels of the data
        labels <- sits_labels(data)

        # create a named vector with integers match the class labels
        n_labels <- length(labels)
        int_labels <- c(1:n_labels)
        names(int_labels) <- labels

        # split the data into training and validation data sets
        # create partitions different splits of the input data
        test_data <- .sits_distances_sample(train_data,
            frac = validation_split
        )

        # remove the lines used for validation
        train_data <- train_data[!test_data, on = "original_row"]

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
        train_x <- data.matrix(train_data[, -2:0])
        train_y <- unname(int_labels[as.vector(train_data$reference)]) - 1

        # create the test data for keras
        test_x <- data.matrix(test_data[, -2:0])
        test_y <- unname(int_labels[as.vector(test_data$reference)]) - 1

        # build the model step by step
        # create the input_tensor
        input_tensor <- keras::layer_input(shape = c(NCOL(train_x)))
        output_tensor <- input_tensor

        # build the nodes
        n_layers <- length(layers)
        for (i in seq_len(n_layers)) {
            output_tensor <- keras::layer_dense(output_tensor,
                units = layers[[i]],
                activation = activation
            )
            output_tensor <- keras::layer_dropout(output_tensor,
                rate = dropout_rates[[i]]
            )
            output_tensor <- keras::layer_batch_normalization(output_tensor)
        }
        # create the final tensor
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
            model_loss <- "categorical_crossentropy"
        }
        # create the model
        model_keras <- keras::keras_model(input_tensor, output_tensor)
        # compile the model
        model_keras %>% keras::compile(
            loss = model_loss,
            optimizer = optimizer,
            metrics = "accuracy"
        )

        options(keras.fit_verbose = verbose)

        # fit the model
        history <- model_keras %>% keras::fit(
            train_x, train_y,
            epochs = epochs, batch_size = batch_size,
            validation_data = list(test_x, test_y),
            verbose = verbose, view_metrics = "auto"
        )

        # import model to R
        R_model_keras <- keras::serialize_model(model_keras)

        # build predict closure function
        model_predict <- function(values) {

            # verifies if keras package is installed
            if (!requireNamespace("keras", quietly = TRUE)) {
                stop(paste("keras required for this function to work.",
                           "Please install it."), call. = FALSE)
            }

            # restore model keras
            model_keras <- keras::unserialize_model(R_model_keras)

            # transform input (data.table) into a matrix
            # (remove first two columns)
            values <- data.matrix(values[, -2:0])

            # retrieve the prediction probabilities
            predicted <- data.table::as.data.table(
                stats::predict(model_keras, values)
            )

            # binary classification case
            # adjust prediction values to match binary classification
            if (n_labels == 2) {
                predicted <- .sits_keras_binary_class(predicted)
            }

            # add the class labels as the column names
            colnames(predicted) <- labels

            return(predicted)
        }
        class(model_predict) <- c("keras_model", "sits_model",
                                  class(model_predict))
        return(model_predict)
    }

    result <- .sits_factory_function(samples, result_fun)
    return(result)
}
