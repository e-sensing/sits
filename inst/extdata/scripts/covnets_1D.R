#' @title Train a SITS classifiction model using the Convolutional Neural Networks
#' @name sits_convnets
#'
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Alexandre Xavier Ywata de Carvalho, \email{alexandre.ywata@@ipea.gov.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}

#'
#' @description Use a deeplearning algorithm to classify data.
#' This function is a front-end to the "keras" method R package.
#' Please refer to the documentation in that package for more details.
#'
#' @param distances.tb      a data.table object with a set of distance measures for each training sample
#' @param embedding_dim     dimension of the first layer embeddings
#' @param filter            a vector containing the number of filters in each hidden layer
#' @param activation        a vector containing the names of activation functions. Valid values are {'relu', 'elu', 'selu', 'sigmoid'}
#' @param kernel            a vector number in containing the size of 1D convolution kernels
#' @param pool_size         size of max pooling windows
#' @param optimizer         Function with a pointer to the optimizer function (default is optimization_adam()).
#'                          Options are optimizer_adadelta(), optimizer_adagrad(), optimizer_adam(),
#'                          optimizer_adamax(), optimizer_nadam(), optimizer_rmsprop(), optimizer_sgd()
#' @param epochs            Number of iterations to train the model.
#' @param batch_size        Number of samples per gradient update.
#' @param int_conv_factor   Factor to convert from real to integer values.
#' @param validation_split	Float between 0 and 1. Fraction of the training data to be used as validation data.
#'                          The model will set apart this fraction of the training data, will not train on it,
#'                          and will evaluate the loss and any model metrics on this data at the end of each epoch.
#'                          The validation data is selected from the last samples in the x and y data provided,
#'                          before shuffling.
#' @return result          either an model function to be passed in sits_predict or an function prepared that can be called further to compute multinom training model
#' @export
#'
library(keras)



embedding_dim    = 128
filters          = c(32,32)
activation       = 'relu'
kernels          = c(7, 7)
pool_size        = 5
optimizer        = keras::optimizer_adam(lr = 0.001)
epochs           = 50
batch_size       = 128
int_conv_factor  = 10000
validation_split = 0.2

# Retrieve the set of samples for the Cerrado
samples <- paste0("https://www.dropbox.com/s/addv5lxbpjm85jr/cerrado_13classes_modis_col6.rda?raw=1")

download.file(samples, destfile = "./cerrado_13classes_modis_col6.rda")
load(file = "./cerrado_13classes_modis_col6.rda")

# select the bands "ndvi", "evi", "nir", and "mir"
samples.tb <- sits_select(samples.tb, bands = c("ndvi","evi", "nir", "mir"))

# find the distance from the data
train_data.tb <- sits_distances(samples.tb, adj_fun  = function(x) {identity(x)})


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
        train_data.tb <- dplyr::anti_join(train_data.tb, test_data.tb, by = names(train_data.tb))

        # shuflle the data
        train_data.tb <- dplyr::sample_frac(train_data.tb, 1.0)

        # organize data for model training
        train.x <- data.matrix(train_data.tb[, -(1:2)])
        train.y <- unname(int_labels[as.vector(train_data.tb[, 2])]) - 1

        # 1D CNN requires recoding inputs as integers
        train.x <- as.integer(int_conv_factor * train.x)

        # keras requires categorical data to be put in a matrix
        train.y <- keras::to_categorical(train.y, n_labels)

        # create the test data for keras
        test.x <- data.matrix(test_data.tb[, -(1:2)])
        test.y <- unname(int_labels[as.vector(test_data.tb[, 2])]) - 1

        # keras requires categorical data to be put in a matrix
        test.y <- keras::to_categorical(test.y, n_labels)

        # 1D CNN requires recoding inputs as integers
        test.x <- as.integer(int_conv_factor * test.x)

        # set the activation vector
        act_vec <- vector()
        #
        for (i in 1:length(filters)) {
            if (length(activation) == 1)
                act_vec[i] <- activation
            else
                act_vec <- activation
        }

        # build the model step by step
        # create the input_tensor
        model.keras <- keras::keras_model_sequential() %>%
            keras::layer_embedding(input_dim    = (int_conv_factor + 1),
                                   output_dim   = embedding_dim,
                                   input_length = NCOL(train.x),
                                   batch_size   = batch_size)

        # build the nodes
        for (i in 1:length(filters)) {
            model.keras <- keras::layer_conv_1d(model.keras, filters = filters[i],
                                                kernel_size = kernels[i], activation = act_vec[i])
            if (i < length(filters))
                model.keras <- keras::layer_max_pooling_1d(model.keras, pool_size = pool_size)
            else
                model.keras <- keras::layer_global_max_pooling_1d(model.keras)

        }
        # create the final tensor
        model.keras <- keras::layer_dense(model.keras, units = n_labels, activation = "softmax")

        # display the model
        summary(model.keras)

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
        plot(history)
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
