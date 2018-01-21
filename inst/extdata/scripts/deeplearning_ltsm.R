#' @title Train a SITS classifiction model using the keras deep learning with LSTM
#' @name sits_deeplearning_lstm
#'
#' @author Alexandre Xavier Ywata de Carvalho, \email{alexandre.ywata@@ipea.gov.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Use a deeplearning algorithm to classify data using a LSTM (long-term short memory architecture)
#' This function is a front-end to the "keras" method R package.
#' Please refer to the documentation in that package for more details.
#'
#' @param distances.tb      a data.table object with a set of distance measures for each training sample
#' @param units             a vector containing the number of hidden nodes in each hidden layer
#' @param activation        a vector containing the names of activation functions. Valid values are {'relu', 'elu', 'selu', 'sigmoid'}
#' @param dropout_rates     a vector number in containing the dropout rates (0,1) from each layer to the next layer
#' @param optimizer         Function with a pointer to the optimizer function (default is optimization_adam()).
#'                          Options are optimizer_adadelta(), optimizer_adagrad(), optimizer_adam(),
#'                          optimizer_adamax(), optimizer_nadam(), optimizer_rmsprop(), optimizer_sgd()
#' @param epochs            Number of iterations to train the model.
#' @param batch_size        Number of samples per gradient update.
#' @param validation_split	Float between 0 and 1. Fraction of the training data to be used as validation data.
#'                          The model will set apart this fraction of the training data, will not train on it,
#'                          and will evaluate the loss and any model metrics on this data at the end of each epoch.
#'                          The validation data is selected from the last samples in the x and y data provided,
#'                          before shuffling.
#' @return result          either an model function to be passed in sits_predict or an function prepared that can be called further to compute multinom training model
#' @examples
#' \donttest{
#' # Retrieve the set of samples for the Mato Grosso region (provided by EMBRAPA)
#' data(samples_MT_ndvi)
#' # get a point with a 16 year time series
#' data(point_ndvi)
#' # classify the point
#' class.tb <- sits_classify (point_ndvi, samples_MT_ndvi, sits_deeplearning(),
#'                            adj_fun = function (x) {BBmisc::normalize(x, method = "range")})
#' }
#' @export
#'
sits_deeplearning_ltsm <- function(distances.tb     = NULL,
                                   units            = c(400,200,100),
                                   activation       = 'tanh',
                                   dropout_rates    = c(0.4, 0.3, 0.2),
                                   optimizer        = keras::optimizer_adam(lr = 0.001),
                                   epochs           = 50,
                                   batch_size       = 128,
                                   validation_split = 0.2) {

    # library(keras)

    # function that returns keras model based on a sits sample tibble
    result_fun <- function(train_data.tb){

        # is the input data the result of a TWDTW matching function?
        ensurer::ensure_that(train_data.tb, "reference" %in% names(.),
                             err_desc = "sits_deeplearning_ltsm: input data does not contain distance")

        ensurer::ensure_that(units, length(.) == length(dropout_rates),
                             err_desc = "sits_deeplearning_ltsm: number of units must match number of dropout rates")

        ensurer::ensure_that(activation, length(.) == length(dropout_rates) || length(.) == 1,
                             err_desc = "sits_deeplearning: activation vectors should be one string or a
                             set of strings that match the number of units")

        # get the labels of the data
        labels <- as.vector(unique(train_data.tb$reference))

        # create a named vector with integers match the class labels
        int_labels <- c(1:length(labels))
        names(int_labels) <- labels
        n_labels <- length(labels)

        # split the data into training and validation
        # create partitions different splits of the input data
        test_data.tb <- .sits_sample_distances(train_data.tb, frac = validation_split)

        # remove the lines used for validation
        train_data.tb <- dplyr::anti_join(train_data.tb, test_data.tb)

        # shuflle the data
        train_data.tb <- dplyr::sample_frac(train_data.tb, 1.0)

        # organize data for model training
        train.x <- data.matrix(train_data.tb[, -(1:2)])
        train.y <- unname(int_labels[as.vector(train_data.tb[, 2])]) - 1

        # keras requires categorical data to be put in a matrix
        train.y <- keras::to_categorical(train.y, n_labels)

        # create the test data for keras
        test.x <- data.matrix(test_data.tb[, -(1:2)])
        test.y <- unname(int_labels[as.vector(test_data.tb[, 2])]) - 1

        # keras requires categorical data to be put in a matrix
        test.y <- keras::to_categorical(test.y, n_labels)

        # set the activation vector
        act_vec <- vector()
        #
        for (i in 1:length(units)) {
            if (length(activation) == 1)
                act_vec[i] <- activation
            else
                act_vec <- activation
        }
        n_vectors <- NCOL(train.x)
        n_samples <- NROW(train.x)
        # build the model step by step
        model.keras <- keras::keras_model_sequential()
        # create the input_tensor
        keras::layer_lstm(model.keras, units = units[1], activation = act_vec[1],
                          input_shape = c(n_samples, n_vectors), batch_input_shape = c(batch_size, n_vectors, 1),
                          return_sequences = TRUE, stateful = TRUE,
                          dropout = dropout_rates[1], recurrent_dropout = dropout_rates[1])
        # build the nodes
        for (i in 2:length(units))
            keras::layer_lstm(model.keras, units = units[i], activation = act_vec[i],
                              return_sequences = TRUE, stateful = TRUE,
                              dropout = dropout_rates[i], recurrent_dropout = dropout_rates[i])
        # create the final tensor
        keras::layer_dense(model.keras, units = n_labels, activation = "softmax")

        # compile the model
        model.keras %>% keras::compile(
            loss = "categorical_crossentropy",
            optimizer = optimizer,
            metrics = c("accuracy")
        )
        # fit the model
        history <- model.keras %>% keras::fit(
            train.x, train.y,
            epochs = epochs, batch_size = batch_size,
            validation_data = list(test.x, test.y)
        )
        # evaluate the model
        sits.env$config$keras_history <- history

        # evaluate the model
        sits.env$config$keras_eval <- keras::evaluate(model.keras, test.x, test.y, verbose = 0)

        # save the model as a global variable
        sits.env$config$keras_model <- model.keras

        # construct model predict enclosure function and returns
        model_predict <- function(values.tb){
            values.x    <- data.matrix(values.tb[, -(1:2)])
            preds       <- stats::predict(model.keras, values.x)
            pred.labels <- names(int_labels[max.col(preds)])
            return(pred.labels)
        }
        return(model_predict)
    }

    result <- .sits_factory_function(distances.tb, result_fun)
    return(result)
}
