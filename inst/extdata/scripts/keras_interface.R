
library(keras)
#install_keras()

data("cerrado_13classes_modis_col6")

distances <- sits_distances(cerrado_13classes_modis_col6, adj_fun = function(x){BBmisc::normalize(x, method = "range")})

train_data.tb <-  distances

# get the labels of the data
labels <- as.vector(unique(train_data.tb$reference))

# create a named vector with integers match the class labels
int_labels <- c(1:length(labels))
names(int_labels) <- labels
num_labels <- length(labels)

# shuflle the data
train_data.tb <- dplyr::sample_frac(train_data.tb, 1.0)

train.x <- data.matrix(train_data.tb[, -(1:2)])
train.y <- unname(int_labels[as.vector(train_data.tb[, 2])]) - 1

# prepare data for training
train.y <- keras::to_categorical(train.y, num_labels)

n_input <- NCOL(train.x)

model <- keras::keras_model_sequential()
model %>%
    keras::layer_dense(units = 400, activation = "relu", input_shape = c(n_input)) %>%
    keras::layer_dropout(rate = 0.4) %>%
    keras::layer_dense(units = 200, activation = "relu") %>%
    keras::layer_dropout(rate = 0.3) %>%
    keras::layer_dense(units = 100, activation = "relu") %>%
    keras::layer_dropout(rate = 0.3) %>%
    keras::layer_dense(units = num_labels, activation = "softmax")

model %>% compile(
    loss = "categorical_crossentropy",
    optimizer = optimizer_adam(),
    metrics = c("accuracy")
)

history <- model %>% fit(
    train.x, train.y,
    epochs = 30, batch_size = 128,
    validation_split = 0.2
)

#' @title Train a SITS classifiction model using the keras deep learning
#' @name sits_deeplearning
#'
#' @author Alexandre Xavier Ywata de Carvalho, \email{alexandre.ywata@@ipea.gov.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Use a deeplearning algorithm to classify data.
#' This function is a front-end to the "keras" method R package.
#' Please refer to the documentation in that package for more details.
#'
#' @param distances        a time series with a set of distance measures for each training sample
#' @param hidden_nodes     a vector containing the number of hidden nodes in each hidden layer
#' @param activation       a vector containing the names of activation functions. Valid values are {'relu', 'elu', 'selu', 'sigmoid'}
#' @param dropout          a vector number in containing the dropout ratios (0,1) from each layer to the next layer
#' @param out_activation    Name of the output activation function. Valid values are {'rmse', 'softmax', 'logistic'}
#' @param optimizer         Function with a pointer to the optimizer function (default is optimization_adam()).
#'                          Options are optimizer_adadelta(), optimizer_adagrad(), optimizer_adam(),
#'                          optimizer_adamax(), optimizer_nadam(), optimizer_rmsprop(), optimizer_sgd()
#' @param epochs            Number of iterations to train the model.
#' @param batch.size        Number of samples per gradient update.
#' @param validation_split	Float between 0 and 1. Fraction of the training data to be used as validation data.
#'                          The model will set apart this fraction of the training data, will not train on it,
#'                          and will evaluate the loss and any model metrics on this data at the end of each epoch.
#'                          The validation data is selected from the last samples in the x and y data provided,
#'                          before shuffling.
#' @param validation_data	Data on which to evaluate the loss and any model metrics at the end of each epoch.
#'                          The model will not be trained on this data. This could be a list (x_val, y_val)
#'                          or a list (x_val, y_val, val_sample_weights).
#'                          validation_data will override validation_split.
#' @param device           whether to train on mx.cpu (default) or mx.gpu
#' @param multicores       number of cores to use for training (default = 1)
#' @return result          either an model function to be passed in sits_predict or an function prepared that can be called further to compute multinom training model
#' @examples
#' \donttest{
#' # Retrieve the set of samples for the Mato Grosso region (provided by EMBRAPA)
#' data(samples_MT_ndvi)
#' # get a point with a 16 year time series
#' data(point_ndvi)
#' # classify the point
#' class.tb <- sits_classify (point_ndvi, samples_MT_ndvi, sits_deeplearning(), adj_fun = identity)
#' }
#' @export
#'
sits_deeplearning <- function(distances        = NULL,
                              hidden_node      = c(400,200,100),
                              activation       = c('relu', 'relu', 'relu'),
                              dropout          = c(0.4, 0.3, 0.2),
                              out_activation   = "softmax",
                              optimizer        = keras::optimization_adam(lr = 0.001),
                              epochs           = 50,
                              batch.size       = 32,
                              validation.split = 0.2,
                              validation.data  = NULL) {

    # function that returns keras model based on a sits sample tibble
    result_fun <- function(train_data.tb){

        # is the input data the result of a TWDTW matching function?
        ensurer::ensure_that(train_data.tb, "reference" %in% names(.),
                             err_desc = "sits_mlp: input data does not contain distance")

        # get the labels of the data
        labels <- as.vector(unique(train_data.tb$reference))

        # create a named vector with integers match the class labels
        int_labels <- c(1:length(labels))
        names(int_labels) <- labels
        num_labels <- length(labels)

        # shuflle the data
        train_data.tb <- dplyr::sample_frac(train_data.tb, 1.0)

        train.x <- data.matrix(train_data.tb[, -(1:2)])
        train.y <- unname(int_labels[as.vector(train_data.tb[, 2])]) - 1

        # prepare data for training
        train.y <- keras::to_categorical(train.y, num_labels)


        # construct model predict enclosure function and returns
        model_predict <- function(values.tb){
            values.x <- data.matrix(values.tb[, -(1:2)])
            preds <- stats::predict(model.mlp, values.x)
            pred.labels <- names(int_labels[max.col(t(preds))])
            return(pred.labels)
        }
        return(model_predict)
    }

    result <- .sits_factory_function(distances.tb, result_fun)
    return(result)
}
