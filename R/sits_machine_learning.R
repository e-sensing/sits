#'
#' @title Train SITS classification models
#' @name sits_train
#'
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Alexandre Xavier Ywata de Carvalho, \email{alexandre.ywata@@ipea.gov.br}
#'
#' @description Given a tibble with a set of distance measures,
#'    returns trained models. Currenly, sits supports the following models:
#' 'svm' (see \code{\link[sits]{sits_svm}}), 'random forest' (see \code{\link[sits]{sits_rfor}}),
#' 'boosting' (see \code{\link[sits]{sits_gbm}}), 'lda' (see \code{\link[sits]{sits_lda}}),
#' 'qda' (see \code{\link[sits]{sits_qda}}), multinomial logit' (see \code{\link[sits]{sits_mlr}}),
#' 'lasso' (see \code{\link[sits]{sits_mlr}}), 'ridge' (see \code{\link[sits]{sits_mlr}}),
#' and "deep learning" (see \code{\link[sits]{sits_deeplearning}})
#' The sits_train function is called inside \code{\link[sits]{sits_classify}}
#' and \code{\link[sits]{sits_classify_raster}}, so the user does not need
#' to explicitly use this function. Please see the above-mention classification functions.
#'
#' @param  data.tb          a time series with the training samples
#' @param  ml_method        the machine learning method that returns a model for prediction
#' @param  adj_fun          Adjustment function to be applied to the data
#' @return result           a model fitted into input data given by train_method parameter
#'
#' @examples
#'
#'\donttest{
#' # Retrieve the set of samples for the Mato Grosso region (provided by EMBRAPA)
#' data(samples_MT_ndvi)
#' # find a training model based on the distances and default values (SVM model)
#' ml_model <- sits_train (samples_MT_ndvi)
#' # get a point
#' data(ts_2000_2016)
#' point.tb <- sits_select (ts_2000_2016, bands = c("ndvi"))
#' # classify the point with the ml_model
#' class.tb <- sits_classify_model (point.tb, samples_MT_ndvi, ml_model)
#' }
#' @export
#'
sits_train <- function(data.tb, ml_method = sits_svm(), adj_fun = sits_adjust()) {

    # is the input data a valid SITS tibble?
    ensurer::ensure_that(data.tb, "label" %in% names(.),
                         err_desc = "sits_train: input data does not contain a valid SITS tibble")

    # is the train method a function?
    ensurer::ensure_that(ml_method, class(.) == "function",
                         err_desc = "sits_train: ml_method is not a valid function")

    #is the distance method a function ?
    ensurer::ensure_that(adj_fun, class(.) == "function",
                         err_desc = "sits_train: adj_fun is not a valid function")

    # compute the distances
    distances.tb <- sits_distances(data.tb, adj_fun)

    # compute the training method by the given data
    result <- ml_method(distances.tb)

    # return a valid machine learning method
    return(result)
}
#' @title Train a SITS classifiction model using the keras deep learning
#' @name sits_deeplearning
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
sits_deeplearning <- function(distances.tb        = NULL,
                              units            = c(400,200,100),
                              activation       = 'relu',
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
                             err_desc = "sits_deeplearning: input data does not contain distance")

        ensurer::ensure_that(units, length(.) == length(dropout_rates),
                             err_desc = "sits_deeplearning: number of units must match number of dropout rates")

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
        train_data.tb <- dplyr::anti_join(train_data.tb, test_data.tb, by = names(train_data.tb))

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

        # build the model step by step
        # create the input_tensor
        input_tensor <- keras::layer_input(shape = c(NCOL(train.x)))
        output_tensor <-  input_tensor

        # build the nodes
        for (i in 1:length(units)) {
            output_tensor <- keras::layer_dense(output_tensor, units = units[i], activation = act_vec[i])
            output_tensor <- keras::layer_dropout(output_tensor, rate = dropout_rates[i])
        }
        # create the final tensor
        output_tensor <- keras::layer_dense(output_tensor, units = n_labels, activation = "softmax")
        # create the model
        model.keras <- keras::keras_model(input_tensor, output_tensor)

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
        graphics::plot(history)
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
#' @title Train a SITS classification model with a gradient boosting machine
#' @name sits_gbm
#'
#' @author Alexandre Xavier Ywata de Carvalho, \email{alexandre.ywata@@ipea.gov.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function implements the generalized boosted modeling framework.
#' Boosting is the process of iteratively adding basis functions in a greedy fashion
#' so that each additional basis function further reduces the selected loss function.
#' This function is a front-end to the "gbm" method in the "gbm" package.
#' Please refer to the documentation in that package for more details.
#'
#' @param distances.tb     a time series with a set of distance measures for each training sample
#' @param formula          a symbolic description of the model to be fit. SITS offers a set of such formulas (default: sits_formula_logref)
#' @param distribution     The name of the distribution - use "multinomial" for classification
#' @param n.trees          Number of trees to fit. This should not be set to too small a number,
#'                         to ensure that every input row gets predicted at least a few times. (default: 5000)
#' @param interaction.depth  The maximum depth of variable interactions. 1 implies an additive model, 2 implies a model with up to 2-way interactions.
#' @param shrinkage        a shrinkage parameter applied to each tree in the expansion.
#'                         Also known as the learning rate or step-size reduction.
#' @param cv.folds         number of cross-validations to run
#' @param n.cores          number of cores to run
#' @param ...              other parameters to be passed to `gbm::gbm` function
#' @return result          a model function to be passed in sits_predict or an function prepared that can be called further to compute multinom training model
#'
#' @examples
#' \donttest{
#' # Retrieve the set of samples for the Mato Grosso region (provided by EMBRAPA)
#' data(samples_MT_ndvi)
#' # get a point with a 16 year time series
#' data(point_ndvi)
#' # classify the point
#' class.tb <- sits_classify (point_ndvi, samples_MT_ndvi, sits_gbm())
#' }

#' @export
#'
sits_gbm <- function(distances.tb = NULL, formula = sits_formula_logref(), distribution = "multinomial",
                     n.trees = 500, interaction.depth = 2, shrinkage = 0.001, cv.folds = 5, n.cores = 1, ...) {

    # function that returns glmnet::multinom model based on a sits sample tibble
    result_fun <- function(train_data.tb){

        # is the input data the result of a TWDTW matching function?
        ensurer::ensure_that(train_data.tb, "reference" %in% names(.),
                             err_desc = "sits_gbm: input data does not contain distance")

        # if parameter formula is a function call it passing as argument the input data sample. The function must return a valid formula.
        if (class(formula) == "function")
            formula <- formula(train_data.tb)

        # call gbm::gbm method and return the trained multinom model
        df <- data.frame (train_data.tb[-1:0])
        result_gbm <- gbm::gbm(formula = formula, data = df,
                               distribution = distribution, n.trees = n.trees, interaction.depth = interaction.depth,
                               shrinkage = shrinkage, cv.folds = cv.folds, n.cores = n.cores,...)

        # check performance using 5-fold cross-validation
        best.iter <- gbm::gbm.perf(result_gbm, method="cv")

        # construct model predict enclosure function and returns
        # construct model predict enclosure function and returns
        model_predict <- function(values.tb){
            result <- stats::predict(result_gbm, newdata = values.tb, best.iter)
            #return (result)
            return(colnames(result)[max.col(data.frame(result))])
        }
        return(model_predict)
    }

    result <- .sits_factory_function(distances.tb, result_fun)
    return(result)
}
#' @title Train a SITS classification model using linear discriminant analysis
#' @name sits_lda
#'
#' @author Alexandre Xavier Ywata de Carvalho, \email{alexandre.ywata@@ipea.gov.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function receives a tibble with a set of attributes X for each observation Y
#' These attributes are distance metrics between patterns and observations, obtained by a distance
#' function in SITS (see \code{\link[sits]{sits_distances}}).
#' The method performs a linear discriminant analysis (lda) to obtain a predictive model.
#' This function is a front-end to the "lda" method in the "MASS" package.
#' Please refer to the documentation in that package for more details.
#'
#' @param distances.tb     a time series with a set of distance measures for each training sample
#' @param formula          a symbolic description of the model to be fit. SITS offers a set of such formulas (default: sits_formula_logref)
#' @param ...              other parameters to be passed to MASS::lda function
#' @return result          a model function to be passed in sits_predict
#'
#' @examples
#' \donttest{
#' # Retrieve the set of samples for the Mato Grosso region (provided by EMBRAPA)
#' data(samples_MT_ndvi)
#' # get a point with a 16 year time series
#' data(point_ndvi)
#' # classify the point
#' class.tb <- sits_classify (point_ndvi, samples_MT_ndvi, ml_method = sits_lda())
#' }
#' @export
#'
sits_lda <- function(distances.tb = NULL, formula = sits_formula_logref(), ...) {

    # function that returns MASS::lda model based on a sits sample tibble
    result_fun <- function(train_data.tb){

        # is the input data the result of a TWDTW matching function?
        ensurer::ensure_that(train_data.tb, "reference" %in% names(.),
                             err_desc = "sits_lda: input data does not contain distance")

        # if parameter formula is a function call it passing as argument the input data sample. The function must return a valid formula.
        if (class(formula) == "function")
            formula <- formula(train_data.tb)

        # call MASS::lda method and return the trained lda model
        result_lda <- MASS::lda(formula = formula, data = train_data.tb, ..., na.action = stats::na.fail)

        # construct model predict enclosure function and returns
        model_predict <- function(values.tb){
            return(stats::predict(result_lda, newdata = values.tb)$class)
        }
        return(model_predict)
    }

    result <- .sits_factory_function(distances.tb, result_fun)
    return(result)
}

#' @title Train a SITS classification model using quadratic discriminant analysis
#' @name sits_qda
#'
#' @author Alexandre Xavier Ywata de Carvalho, \email{alexandre.ywata@@ipea.gov.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function receives a tibble with a set of attributes X for each observation Y
#' These attributes are distance metrics between patterns and observations, obtained by a distance
#' function in SITS (see \code{\link[sits]{sits_distances}}).
#' The function performs a quadratic discriminant analysis (qda) to obtain a predictive model.
#' This function is a front-end to the "qda" method in the "MASS" package.
#' Please refer to the documentation in that package for more details.
#'
#' @param distances.tb     a time series with a set of distance measures for each training sample
#' @param formula          a symbolic description of the model to be fit. SITS offers a set of such formulas (default: sits_formula_logref)
#' @param ...              other parameters to be passed to MASS::lda function
#' @return result          a model function to be passed in sits_predict
#'
#' @examples
#' \donttest{
#' # Retrieve the set of samples for the Mato Grosso region (provided by EMBRAPA)
#' data(samples_MT_ndvi)
#' # get a point with a 16 year time series
#' data(point_ndvi)
#' # classify the point
#' class.tb <- sits_classify (point_ndvi, samples_MT_ndvi, ml_method = sits_qda())
#' }
#' @export
#'
sits_qda <- function(distances.tb = NULL, formula = sits_formula_logref(), ...) {

    # function that returns MASS::lda model based on a sits sample tibble
    result_fun <- function(train_data.tb){

        # is the input data the result of a matching function?
        ensurer::ensure_that(train_data.tb, "reference" %in% names(.),
                             err_desc = "sits_qda: input data does not contain distance")

        # if parameter formula is a function call it passing as argument the input data sample. The function must return a valid formula.
        if (class(formula) == "function")
            formula <- formula(train_data.tb)

        # call MASS::qda method and return the trained lda model
        result_qda <- MASS::qda(formula = formula, data = train_data.tb, ..., na.action = stats::na.fail)

        # construct model predict enclosure function and returns
        model_predict <- function(values.tb){
            return(stats::predict(result_qda, newdata = values.tb)$class)
        }
        return(model_predict)
    }

    result <- .sits_factory_function(distances.tb, result_fun)
    return(result)
}

#' @title Train a SITS classifiaction model using multinomial log-linear regions via neural networks
#' @name sits_mlr
#'
#' @author Alexandre Xavier Ywata de Carvalho, \email{alexandre.ywata@@ipea.gov.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Use multinomial log-linear (mlr) fitting model via neural networks to classify data.
#' These attributes are distance metrics between patterns and observations, obtained by a distance
#' function in SITS (see \code{\link[sits]{sits_distances}}).
#' This function is a front-end to the "multinom" method in the "nnet" package.
#' Please refer to the documentation in that package for more details.
#'
#' @param distances.tb     a time series with a set of distance measures for each training sample
#' @param formula          a symbolic description of the model to be fit. SITS offers a set of such formulas (default: sits_formula_logref)
#' @param n_weights        maximum number of weights (should be proportional to size of input data)
#' @param maxit            maximum number of iterations (default 300)
#' @param ...              other parameters to be passed to nnet::multinom function
#' @return result          a model function to be passed in sits_predict
#'
#' @examples
#' \donttest{
#' # Retrieve the set of samples for the Mato Grosso region (provided by EMBRAPA)
#' data(samples_MT_ndvi)
#' # get a point with a 16 year time series
#' data(point_ndvi)
#' # classify the point
#' class.tb <- sits_classify (point_ndvi, samples_MT_ndvi, ml_method = sits_mlr())
#' }
#' @export
#'
sits_mlr <- function(distances.tb = NULL, formula = sits_formula_logref(),
                     n_weights = 5000, maxit = 250, ...) {

    # function that returns nnet::multinom model based on a sits sample tibble
    result_fun <- function(train_data.tb){


        # is the input data the result of a TWDTW matching function?
        ensurer::ensure_that(train_data.tb, "reference" %in% names(.),
                             err_desc = "sits_mlr: input data does not contain distance")

        # if parameter formula is a function call it passing as argument the input data sample. The function must return a valid formula.
        if (class(formula) == "function")
            formula <- formula(train_data.tb)

        # call nnet::multinom method and return the trained multinom model
        result_mlr <- nnet::multinom(formula = formula,
                                     data = train_data.tb,
                                     maxit = maxit,
                                     MaxNWts = n_weights, ..., na.action = stats::na.fail)

        # construct model predict enclosure function and returns
        model_predict <- function(values.tb){
            result <- stats::predict(result_mlr, newdata = values.tb)
            return(result)
        }
        return(model_predict)
    }

    result <- .sits_factory_function(distances.tb, result_fun)
    return(result)
}


#' @title Train a SITS classifiction model using random forest algorithm
#' @name sits_rfor
#'
#' @author Alexandre Xavier Ywata de Carvalho, \email{alexandre.ywata@@ipea.gov.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Use Random Forest algorithm to classify data.
#' This function is a front-end to the "randomForest" method in the "randomForest" package.
#' Please refer to the documentation in that package for more details.
#'
#' @param distances.tb     a time series with a set of distance measures for each training sample
#' @param ntree            Number of trees to grow. This should not be set to too small a number,
#'                         to ensure that every input row gets predicted at least a few times. (default: 2000)
#' @param nodesize         Minimum size of terminal nodes (default 1 for classification)
#' @param ...              other parameters to be passed to `randomForest::randomForest` function
#' @return result          either an model function to be passed in sits_predict or an function prepared that can be called further to compute multinom training model
#' @examples
#' \donttest{
#' # Retrieve the set of samples for the Mato Grosso region (provided by EMBRAPA)
#' data(samples_MT_ndvi)
#' # get a point with a 16 year time series
#' data(point_ndvi)
#' # classify the point
#' class.tb <- sits_classify (point_ndvi, samples_MT_ndvi, sits_rfor())
#' }
#' @export
#'
sits_rfor <- function(distances.tb = NULL, ntree = 2000, nodesize = 1, ...) {

    # function that returns `randomForest::randomForest` model based on a sits sample tibble
    result_fun <- function(train_data.tb){

        # is the input data the result of a TWDTW matching function?
        ensurer::ensure_that(train_data.tb, "reference" %in% names (.),
                             err_desc = "sits_rfor: input data does not contain distance")

        # call `randomForest::randomForest` method and return the trained multinom model
        df <- data.frame(train_data.tb[-1:0])
        result_rfor <- randomForest::randomForest(x = df[-1:0],
                                                  y = as.factor(df$reference),
                                                  data = NULL, ntree = ntree, nodesize = 1,
                                                  norm.votes = FALSE, ..., na.action = stats::na.fail)

        # construct model predict enclosure function and returns
        model_predict <- function(values.tb){
            return(stats::predict(result_rfor, newdata = values.tb, type = "response"))
        }
        return(model_predict)
    }

    result <- .sits_factory_function(distances.tb, result_fun)
    return(result)
}
#' @title Train a SITS classification model using a support vector machine
#' @name sits_svm
#'
#' @author Alexandre Xavier Ywata de Carvalho, \email{alexandre.ywata@@ipea.gov.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function receives a tibble with a set of attributes X for each observation Y
#' These attributes are distance metrics between patterns and observations, obtained by a distance
#' function in SITS (see \code{\link[sits]{sits_distances}}).
#' The SVM algorithm is used for multiclass-classification.
#' For this purpose, it uses the "one-against-one" approach, in which k(k-1)/2 binary
#' classifiers are trained; the appropriate class is found by a voting scheme.
#' This function is a front-end to the "svm" method in the "e1071" package.
#' Please refer to the documentation in that package for more details.
#'
#' @param distances.tb     a time series with a set of distance measures for each training sample
#' @param formula          a symbolic description of the model to be fit. SITS offers a set of such formulas (default: sits_svm)
#' @param kernel           the kernel used in training and predicting (options = linear, polynomial, radial basis, sigmoid)
#' @param degree           exponential of polynomial type kernel
#' @param coef0	           parameter needed for kernels of type polynomial and sigmoid (default: 0)
#' @param cost             cost of constraints violation
#' @param tolerance	       tolerance of termination criterion (default: 0.001)
#' @param epsilon	       epsilon in the insensitive-loss function (default: 0.1)
#' @param cross            the number of cross validation folds applied on the training data to assess the quality of the model,
#' @param ...              other parameters to be passed to e1071::svm function
#' @return result          a fitted model function to be passed to sits_predict
#'
#' @examples
#' # Retrieve the set of samples for the Mato Grosso region (provided by EMBRAPA)
#' data(samples_MT_ndvi)
#' # get a point
#' data(point_ndvi)
#' # classify the point
#' class.tb <- sits_classify (point_ndvi, samples_MT_ndvi,
#'        ml_method = sits_svm(kernel = "radial", cost = 10))
#'
#' @export
#'
sits_svm <- function(distances.tb = NULL, formula = sits_formula_logref(), kernel = "radial",
                     degree = 3, coef0 = 0, cost = 10, tolerance = 0.001, epsilon = 0.1, cross = 4, ...) {

    # function that returns e1071::svm model based on a sits sample tibble
    result_fun <- function(train_data.tb){

        # is the input data the result of a matching function?
        ensurer::ensure_that(train_data.tb, "reference" %in% names (.),
                             err_desc = "sits_svm: input data does not contain references information")

        # if parameter formula is a function call it passing as argument the input data sample. The function must return a valid formula.
        if (class(formula) == "function")
            formula <- formula(train_data.tb)

        # call e1071::svm method and return the trained svm model
        result_svm <- e1071::svm(formula = formula, data = train_data.tb, kernel = kernel,
                                 degree = degree, cost = cost, coef0 = coef0,
                                 tolerance = tolerance, epsilon = epsilon, cross = cross, ..., na.action = stats::na.fail)

        # construct model predict enclosure function and returns
        model_predict <- function(values.tb){
            return(stats::predict(result_svm, newdata = values.tb))
        }
        return(model_predict)
    }

    result <- .sits_factory_function(distances.tb, result_fun)
}


#' @title Provides access to diagnostic information about a Keras deep learning model
#' @name sits_keras_diagnostics
#'
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description After the Keras deeplearning model is compiled and fit, this
#'              function provides access to the history plot and the evaluation results
#'
#' @param test.x  Test data to be run with the model (X variables)
#' @param test.y  Test data to be run with the model (Y variables)
#'
#' @return NULL   Prints the model diagnostics
#'
#' @examples
#' \donttest{
#' # Retrieve the set of samples for the Mato Grosso region (provided by EMBRAPA)
#' data(cerrado2classes)
#' # create a vector of distances
#' distances <- sits_distances(cerrado2classes, adj_fun  = function(x) {identity(x)})
#' # obtain a DL model
#' ml_model = sits_deeplearning(distances.tb)
#' # classify the point
#' sits_keras_diagnostics()
#' }
#' @export
sits_keras_diagnostics <- function(test.x = NULL, test.y = NULL) {
    if (purrr::is_null(sits.env$config$keras_model))
        message("Please configure a keras model before running this function")
    else {
        message("Plotting history and evaluation of the model fit")
        if (purrr::is_null(test.x) || purrr::is_null(test.y)) {
            print(sits.env$config$keras_eval)
        }
        else
            keras::evaluate(sits.env$config$keras_model, test.x, test.y, verbose = 0)
    }
    return(NULL)
}


#'
#' @title Define a loglinear formula for classification models
#' @name sits_formula_logref
#'
#' @author Alexandre Xavier Ywata de Carvalho, \email{alexandre.ywata@@ipea.gov.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description A function to be used as a symbolic description of some fitting models such as svm, lda, qda, and gbm.
#' This function instructs the model to do a logarithm transformation of the input values.
#' The `predictors_index` parameter informs the positions of `tb` fields corresponding to formula independent variables.
#' If no value is given, the default is NULL, a value indicating that all fields will be used as predictors.
#'
#' @param predictors_index  the index of the valid columns whose names are used to compose formula (default: NULL)
#' @return result_fun       a function that computes a valid formula
#'
#' @export
sits_formula_logref <- function(predictors_index = -2:0){

    # this function returns a formula like 'factor(reference~log(f1)+log(f2)+...+log(fn)' where f1, f2, ..., fn are,
    # respectivelly, the reference and predictors fields of tibble in `tb` parameter.
    result_fun <- function(tb){

        # if no predictors_index are given, assume that all tb's fields are used
        if (is.null(predictors_index))
            predictors_index <- 1:NROW(tb)

        # get predictors names
        categories <- names(tb)[c(predictors_index)]

        # compute formula result
        result_for <- stats::as.formula(paste0("factor(reference)~", paste0(paste0('log(`', categories, '`)'), collapse = "+")))
        return(result_for)
    }
    return(result_fun)
}

#' @title Define a linear formula for classification models
#' @name sits_formula_linear
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Alexandre Xavier Ywata de Carvalho, \email{alexandre.ywata@@ipea.gov.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description A function to be used as a symbolic description of some fitting models such as svm.
#' This function instructs the model to do a linear transformation of the input values.
#' The `predictors_index` parameter informs the positions of `tb` fields corresponding to formula independent variables.
#' If no value is given, the default is NULL, a value indicating that all fields will be used as predictors.
#'
#' @param predictors_index  the index of the valid columns whose names are used to compose formula (default: NULL)
#' @return result_fun       a function that computes a valid formula
#'
#' @export
sits_formula_linear <- function(predictors_index = -2:0){

    # this function returns a formula like 'factor(reference~log(f1)+log(f2)+...+log(fn)' where f1, f2, ..., fn are,
    # respectivelly, the reference and predictors fields of tibble in `tb` parameter.
    result_fun <- function(tb){

        # if no predictors_index are given, assume that all tb's fields are used
        if (is.null(predictors_index))
            predictors_index <- 1:NROW(tb)

        # get predictors names
        categories <- names(tb)[c(predictors_index)]

        # compute formula result
        result_for <- stats::as.formula(paste0("factor(reference)~", paste0(paste0(categories, collapse = "+"))))
        return(result_for)
    }
    return(result_fun)
}

#' @title Define a smoothing formula for classification models
#' @name sits_formula_smooth
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description A function to be used as a symbolic description of some fitting models such as gam.
#' This function instructs the model to do a smoothing transformation (via splines) of the input values.
#' `predictors_index` parameter informs the positions of `tb` fields corresponding to formula independent variables.
#' If no value is given, the default is NULL, a value indicating that all fields will be used as predictors.
#'
#' @param predictors_index  the index of the valid columns whose names are used to compose formula (default: NULL)
#' @return result_fun       a function that computes a valid formula
#'
#' @export
sits_formula_smooth <- function(predictors_index = -2:0){

    # this function returns a formula like 'factor(reference~log(f1)+log(f2)+...+log(fn)' where f1, f2, ..., fn are,
    # respectivelly, the reference and predictors fields of tibble in `tb` parameter.
    result_fun <- function(tb){

        # if no predictors_index are given, assume that all tb's fields are used
        if (is.null(predictors_index))
            predictors_index <- 1:NROW(tb)

        # get predictors names
        categories <- names(tb)[c(predictors_index)]

        # compute formula result
        result_for <- stats::as.formula(paste0("factor(reference)~", paste0(paste0('s(`', categories, '`)'), collapse = "+")))
        return(result_for)
    }
    return(result_fun)
}

#' @title Predict class based on the trained models
#' @name .sits_predict
#'
#' @author Alexandre Xavier Ywata de Carvalho, \email{alexandre.ywata@@ipea.gov.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Given a SITS tibble time series and a model trained by \code{\link[sits]{sits_train}},
#'   returns a predicted label. Note that the \code{\link[sits]{.sits_predict}} function is
#'   called inside \code{\link[sits]{sits_classify}}, \code{\link[sits]{sits_classify_model}}
#'   and \code{\link[sits]{sits_classify_raster}}, so the user does not need
#'   to explicitly use it. Please see the above-mentioned classification functions.
#'
#' @param distances.tb  a tibble with a set of distance metrics to each of the classes
#' @param ml_model      a model trained by \code{\link[sits]{sits_train}}
#' @param ...           other parameters to be passed to the model function
#' @return predicted    the predicted labels (vector)
#'
.sits_predict <- function(distances.tb = NULL, ml_model, ...){

    # is the input data the result of a TWDTW matching function?
    ensurer::ensure_that(distances.tb, "reference" %in% names(.),
                         err_desc = "sits_predict: input data does not contain TWDTW matches")

    # is the input model a model function?
    ensurer::ensure_that(ml_model, class(.) == "function",
                         err_desc = "sits_predict: model parameter is not a function model returned by sits_train.")

    predicted <- as.character(ml_model(distances.tb))

    return(predicted)
}
