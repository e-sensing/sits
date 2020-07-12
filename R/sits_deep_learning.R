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
#' @examples
#' \donttest{
#' # Retrieve the set of samples for the Mato Grosso region
#' data(samples_mt_4bands)
#' samples_mt_ndvi <- sits_select_bands(samples_mt_4bands, ndvi)
#' # Build a machine learning model based on deep learning
#' dl_model <- sits_train (samples_mt_ndvi,
#'                         sits_deeplearning(layers = c(64, 64, 64),
#'                                           dropout_rates = c(0.50, 0.40, 0.35),
#'                                           epochs = 50))
#' # get a point with a 16 year time series
#' data(point_ndvi)
#' # classify the point
#' class.tb <- sits_classify (point_ndvi, dl_model)
#' # plot the classified point
#' sits_plot(class.tb)
#' }
#' @export
sits_deeplearning <- function(samples          = NULL,
                        layers           = c(512, 512, 512, 512, 512),
                        activation       = 'elu',
                        dropout_rates    = c(0.50, 0.40, 0.35, 0.30, 0.20),
                        optimizer        = keras::optimizer_adam(lr = 0.001),
                        epochs           = 500,
                        batch_size       = 128,
                        validation_split = 0.2,
                        verbose          = 1) {
    # backward compatibility
    samples <- .sits_tibble_rename(samples)

    # function that returns keras model based on a sits sample data.table
    result_fun <- function(data){

        # pre-conditions
        assertthat::assert_that(length(layers) == length(dropout_rates),
                msg = "sits_deeplearning: number of layers does not match
                        number of dropout rates")

        valid_activations <- c("relu", "elu", "selu", "sigmoid")
        assertthat::assert_that(activation %in% valid_activations,
                msg = "sits_deeplearning: invalid node activation method")

        assertthat::assert_that(length(activation) == length(dropout_rates) ||
                                length(activation) == 1,
                       msg = "sits_deeplearning:
                       activation vectors should be one string or a
                       set of strings that match the number of units")

        # data normalization
        stats <- .sits_normalization_param(data)
        train_data_DT <- .sits_distances(.sits_normalize_data(data, stats))

        # is the train data correct?
        assertthat::assert_that("reference" %in% names(train_data_DT),
            msg = "sits_deeplearning:
                   input data does not contain distances")

        # get the labels of the data
        labels <- sits_labels(data)$label

        # create a named vector with integers match the class labels
        n_labels <- length(labels)
        int_labels <- c(1:n_labels)
        names(int_labels) <- labels

        # split the data into training and validation data sets
        # create partitions different splits of the input data
        test_data_DT <- .sits_sample_distances(train_data_DT,
                                               frac = validation_split)

        # remove the lines used for validation
        train_data_DT <- train_data_DT[!test_data_DT, on = "original_row"]

        # shuffle the data
        train_data_DT <- train_data_DT[sample(nrow(train_data_DT),
                                              nrow(train_data_DT)),]
        test_data_DT  <- test_data_DT[sample(nrow(test_data_DT),
                                             nrow(test_data_DT)),]

        # organize data for model training
        train.x <- data.matrix(train_data_DT[, -(1:2)])
        train.y <- unname(int_labels[as.vector(train_data_DT$reference)]) - 1

        # create the test data for keras
        test.x <- data.matrix(test_data_DT[, -(1:2)])
        test.y <- unname(int_labels[as.vector(test_data_DT$reference)]) - 1

        # set the activation vector
        act_vec <- vector()

        n_layers <- length(layers)
        for (i in 1:n_layers) {
            if (length(activation) == 1)
                act_vec[i] <- activation
            else
                act_vec <- activation
        }

        # build the model step by step
        # create the input_tensor
        input_tensor  <- keras::layer_input(shape = c(NCOL(train.x)))
        output_tensor <- input_tensor

        # build the nodes
        for (i in 1:n_layers) {
            output_tensor <- keras::layer_dense(output_tensor,
                                                units = layers[i],
                                                activation = act_vec[i])
            output_tensor <- keras::layer_dropout(output_tensor,
                                                  rate = dropout_rates[i])
            output_tensor <- keras::layer_batch_normalization(output_tensor)
        }
        # create the final tensor
        model_loss <- "categorical_crossentropy"
        if (n_labels == 2) {
            output_tensor <- keras::layer_dense(output_tensor,
                                                units = 1,
                                                activation = "sigmoid")
            model_loss <- "binary_crossentropy"
        }
        else {
            output_tensor <- keras::layer_dense(output_tensor,
                                                units = n_labels,
                                                activation = "softmax")
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

        prev.fit_verbose <- getOption("keras.fit_verbose")
        options(keras.fit_verbose = verbose)

        # fit the model
        history <- model.keras %>% keras::fit(
            train.x, train.y,
            epochs = epochs, batch_size = batch_size,
            validation_data = list(test.x, test.y),
            verbose = verbose, view_metrics = "auto"
        )

        graphics::plot(history)

        # construct model predict closure function and returns
        model_predict <- function(values_DT){
            # transform input (data.table) into a matrix
            # (remove first two columns)
            values.x         <- data.matrix(values_DT[, -(1:2)])
            # retrieve the prediction probabilities
            predict_DT <- data.table::as.data.table(
                                   stats::predict(model.keras, values.x))

            # for the binary classification case
            # adjust the prediction values
            # to match the multi-class classification
            if (n_labels == 2)
                predict_DT <- .sits_keras_binary_class(predict_DT)

            # add the class labels as the column names
            colnames(predict_DT) <- labels

            return(predict_DT)
        }
        class(model_predict) <- append(class(model_predict),
                                       "keras_model",
                                       after = 0)
        return(model_predict)
    }

    result <- .sits_factory_function(samples, result_fun)
    return(result)
}


