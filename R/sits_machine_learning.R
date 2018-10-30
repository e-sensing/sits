#' @title Train sits classification models
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
#' and 'deep learning' (see \code{\link[sits]{sits_deeplearning}}).
#' The sits_train function is called inside \code{\link[sits]{sits_classify}}
#' and \code{\link[sits]{sits_classify_raster}}, so the user does not need
#' to explicitly use this function. Please see the above-mention classification functions.
#'
#' @param  data.tb          Time series with the training samples.
#' @param  ml_method        Machine learning method that returns a model for prediction.
#' @return A model fitted to input data given by train_method parameter.
#'
#' @examples
#'\donttest{
#' # Retrieve the set of samples for the Mato Grosso region (provided by EMBRAPA)
#' # find a training model based on the distances and default values (SVM model)
#' samples.tb <- sits_select_bands(samples_MT_9classes, ndvi, evi, nir, mir)
#' ml_model <- sits_train (samples.tb, sits_rfor())
#' # get a point and classify the point with the ml_model
#' point.tb <- sits_select_bands(point_MT_6bands, ndvi, evi, nir, mir)
#' class.tb <- sits_classify(point_MT_6bands, ml_model)
#' sits_plot(class.tb)
#' }
#' @export
sits_train <- function(data.tb, ml_method = sits_svm()) {
    # is the input data a valid sits tibble?
    ensurer::ensure_that(data.tb, "label" %in% names(.),
                         err_desc = "sits_train: input data does not contain a valid sits tibble")

    # is the train method a function?
    ensurer::ensure_that(ml_method, class(.) == "function",
                         err_desc = "sits_train: ml_method is not a valid function")

    # compute the training method by the given data
    result <- ml_method(data.tb)

    # return a valid machine learning method
    return(result)
}

#' @title Train a sits classifiction model using the keras deep learning
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
#' @param data.tb           Time series with the training samples.
#' @param units             Vector with the number of hidden nodes in each hidden layer.
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
#' @return Either an model function to be passed in sits_predict or an function prepared that can be called further to compute multinom training model.
#' @examples
#' \donttest{
#' # Retrieve the set of samples for the Mato Grosso region (provided by EMBRAPA)
#' data(samples_MT_ndvi)
#' # Build a machine learning model based on deep learning
#' dl_model <- sits_train (samples_MT_ndvi,
#'                         sits_deeplearning(units = c(512, 512, 512),
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
sits_deeplearning <- function(data.tb          = NULL,
                              units            = c(512, 512, 512, 512, 512),
                              activation       = 'elu',
                              dropout_rates    = c(0.50, 0.40, 0.35, 0.30, 0.20),
                              optimizer        = keras::optimizer_adam(lr = 0.001),
                              epochs           = 500,
                              batch_size       = 128,
                              validation_split = 0.2,
                              verbose          = 1,
                              binary_classification = FALSE) {
    # function that returns keras model based on a sits sample data.table
    result_fun <- function(data.tb){
        # data normalization
        stats.tb <- .sits_normalization_param(data.tb)
        train_data_DT <- sits_distances(sits_normalize_data(data.tb, stats.tb))

        # is the train data correct?
        ensurer::ensure_that(train_data_DT, "reference" %in% names(.),
                             err_desc = "sits_deeplearning: input data does not contain distances")

        ensurer::ensure_that(units, length(.) == length(dropout_rates),
                             err_desc = "sits_deeplearning: number of units must match number of dropout rates")

        ensurer::ensure_that(activation, length(.) == length(dropout_rates) || length(.) == 1,
                             err_desc = "sits_deeplearning: activation vectors should be one string or a
                             set of strings that match the number of units")

        # get the labels of the data
        labels <- sits_labels(data.tb)$label

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

        # set the activation vector
        act_vec <- vector()

        for (i in 1:length(units)) {
            if (length(activation) == 1)
                act_vec[i] <- activation
            else
                act_vec <- activation
        }

        # build the model step by step
        # create the input_tensor
        input_tensor  <- keras::layer_input(shape = c(NCOL(train.x)))
        output_tensor <-  input_tensor

        # build the nodes
        for (i in 1:length(units)) {
            output_tensor <- keras::layer_dense(output_tensor, units = units[i], activation = act_vec[i])
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

        prev.fit_verbose <- getOption("keras.fit_verbose")
        options(keras.fit_verbose = verbose)

        # fit the model
        history <- model.keras %>% keras::fit(
            train.x, train.y,
            epochs = epochs, batch_size = batch_size,
            validation_data = list(test.x, test.y)
        )

        options(keras.fit_verbose = prev.fit_verbose)

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

    result <- .sits_factory_function(data.tb, result_fun)
    return(result)
}

#' @title Train a sits classification model with a gradient boosting machine
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
#' @param data.tb          Time series with the training samples.
#' @param formula          A symbolic description of the model to be fit. Package sits offers a set of such formulas (default: sits_formula_logref).
#' @param distribution     Name of the distribution - use "multinomial" for classification.
#' @param n.trees          Number of trees to fit. This should not be set to too small a number,
#'                         to ensure that every input row gets predicted at least a few times. (default: 5000).
#' @param interaction.depth  The maximum depth of variable interactions. 1 implies an additive model, 2 implies a model with up to 2-way interactions.
#' @param shrinkage        A shrinkage parameter applied to each tree in the expansion.
#'                         Also known as the learning rate or step-size reduction.
#' @param cv.folds         Number of cross-validations to run.
#' @param ...              Other parameters to be passed to `gbm::gbm` function.
#' @return A model function to be passed in sits_predict or an function prepared that can be called further to compute multinom training model
#'
#' @examples
#' \donttest{
#' # Retrieve the set of samples for the Mato Grosso region (provided by EMBRAPA)
#' data(samples_MT_ndvi)
#' # Build a GBM model
#' gbm_model <- sits_train(samples_MT_ndvi, sits_gbm(n.trees = 200, cv.folds = 2))
#' # get a point with a 16 year time series
#' data(point_ndvi)
#' # classify the point
#' class.tb <- sits_classify (point_ndvi, gbm_model)
#' # plot the classification
#' sits_plot(class.tb)
#' }
#' @export
sits_gbm <- function(data.tb = NULL, formula = sits_formula_logref(), distribution = "multinomial",
                     n.trees = 500, interaction.depth = 2, shrinkage = 0.001, cv.folds = 5, ...) {
    # function that returns glmnet::multinom model based on a sits sample tibble
    result_fun <- function(data.tb){

        # data normalization
        stats.tb <- .sits_normalization_param(data.tb)
        train_data_DT <- sits_distances(sits_normalize_data(data.tb, stats.tb))

        # get the labels of the data
        labels <- sits_labels(data.tb)$label

        # create a named vector with integers match the class labels
        int_labels <- c(1:length(labels))
        names(int_labels) <- labels

        # if parameter formula is a function call it passing as argument the input data sample. The function must return a valid formula.
        if (class(formula) == "function")
            formula <- formula(train_data_DT)

        # find the number of cores
        multicores <- max(parallel::detectCores(logical = FALSE) - 1, 1)

        # call gbm::gbm method and return the trained multinom model
        result_gbm <- gbm::gbm(formula = formula, data = train_data_DT[, 2:length(train_data_DT)],
                               distribution = distribution, n.trees = n.trees, interaction.depth = interaction.depth,
                               shrinkage = shrinkage, cv.folds = cv.folds, n.cores = multicores,...)

        # check performance using n-fold cross-validation
        best.iter <- gbm::gbm.perf(result_gbm, method = "cv")

        # construct model predict closure function and returns
        model_predict <- function(values_DT){
            # retrieve the prediction results
            preds      <- stats::predict(result_gbm, newdata = values_DT, best.iter)
            # get the prediction probabilties
            # 2-do: normalize the probabilities vectors
            prediction_DT <- data.table::as.data.table(preds[,,1])
            return(prediction_DT)
        }
        return(model_predict)
    }

    result <- .sits_factory_function(data.tb, result_fun)
    return(result)
}

#' @title Train a sits classification model using linear discriminant analysis
#' @name sits_lda
#'
#' @author Alexandre Xavier Ywata de Carvalho, \email{alexandre.ywata@@ipea.gov.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function receives a tibble with a set of attributes X for each observation Y
#' These attributes are distance metrics between patterns and observations, obtained by a distance
#' function in sits (see \code{\link[sits]{sits_distances}}).
#' The method performs a linear discriminant analysis (lda) to obtain a predictive model.
#' This function is a front-end to the "lda" method in the "MASS" package.
#' Please refer to the documentation in that package for more details.
#'
#' @param data.tb          Time series with the training samples.
#' @param formula          A symbolic description of the model to be fit. Package sits offers a set of such formulas (default: sits_formula_logref).
#' @param ...              Other parameters to be passed to MASS::lda function.
#' @return A model function to be passed in sits_predict.
#'
#' @examples
#' \donttest{
#' # Retrieve the set of samples for the Mato Grosso region (provided by EMBRAPA)
#' data(samples_MT_ndvi)
#' # Build an LDA model
#' lda_model <- sits_train(samples_MT_ndvi, sits_lda())
#' # get a point with a 16 year time series
#' data(point_ndvi)
#' # classify the point
#' class.tb <- sits_classify (point_ndvi, lda_model)
#' # plot the classification
#' sits_plot(class.tb)
#' }
#' @export
sits_lda <- function(data.tb = NULL, formula = sits_formula_logref(), ...) {
    # function that returns MASS::lda model based on a sits sample tibble
    result_fun <- function(data.tb){

        # data normalization
        stats.tb <- .sits_normalization_param(data.tb)
        train_data_DT <- sits_distances(sits_normalize_data(data.tb, stats.tb))

        # is the input data the result of a TWDTW matching function?
        ensurer::ensure_that(train_data_DT, "reference" %in% names(.),
                             err_desc = "sits_lda: input data does not contain distance")

        # if parameter formula is a function call it passing as argument the input data sample. The function must return a valid formula.
        if (class(formula) == "function")
            formula <- formula(train_data_DT)

        # call MASS::lda method and return the trained lda model
        result_lda <- MASS::lda(formula = formula, data = train_data_DT, ..., na.action = stats::na.fail)

        # construct model predict closure function and returns
        model_predict <- function(values_DT) {
            # retrieve the prediction (values and probs)
            preds <- stats::predict(result_lda, newdata = values_DT)
            # return probabilities
            prediction_DT <- data.table::as.data.table(preds$posterior)

            return(prediction_DT)
        }
        return(model_predict)
    }

    result <- .sits_factory_function(data.tb, result_fun)
    return(result)
}

#' @title Train a sits classification model using quadratic discriminant analysis
#' @name sits_qda
#'
#' @author Alexandre Xavier Ywata de Carvalho, \email{alexandre.ywata@@ipea.gov.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function receives a tibble with a set of attributes X for each observation Y
#' These attributes are distance metrics between patterns and observations, obtained by a distance
#' function in sits (see \code{\link[sits]{sits_distances}}).
#' The function performs a quadratic discriminant analysis (qda) to obtain a predictive model.
#' This function is a front-end to the "qda" method in the "MASS" package.
#' Please refer to the documentation in that package for more details.
#'
#' @param data.tb          Time series with the training samples.
#' @param formula          Symbolic description of the model to be fit. Package sits offers a set of such formulas (default: sits_formula_logref).
#' @param ...              Other parameters to be passed to MASS::qda function.
#' @return A model function to be passed in sits_predict.
#'
#' @examples
#' \donttest{
#' # Retrieve the set of samples for the Mato Grosso region (provided by EMBRAPA)
#' data(samples_MT_ndvi)
#' # Build a QDA model
#' qda_model <- sits_train(samples_MT_ndvi, sits_qda())
#' # get a point with a 16 year time series
#' data(point_ndvi)
#' # classify the point
#' class.tb <- sits_classify (point_ndvi, qda_model)
#' # plot the classification
#' sits_plot(class.tb)
#' }
#' @export
sits_qda <- function(data.tb = NULL, formula = sits_formula_logref(), ...) {
    # function that returns MASS::qda model based on a sits sample tibble
    result_fun <- function(data.tb){

        # data normalization
        stats.tb <- .sits_normalization_param(data.tb)
        train_data_DT <- sits_distances(sits_normalize_data(data.tb, stats.tb))

        # if parameter formula is a function call it passing as argument the input data sample. The function must return a valid formula.
        if (class(formula) == "function")
            formula <- formula(train_data_DT)

        # call MASS::qda method and return the trained lda model
        result_qda <- MASS::qda(formula = formula, data = train_data_DT, ..., na.action = stats::na.fail)

        # construct model predict closure function and returns
        model_predict <- function(values_DT){
            # retrieve the prediction (values and probs)
            preds <- stats::predict(result_qda, newdata = values_DT)
            # return probabilities
            prediction_DT <- data.table::as.data.table(preds$posterior)

            return(prediction_DT)
        }
        return(model_predict)
    }
    result <- .sits_factory_function(data.tb, result_fun)
    return(result)
}

#' @title Train a sits classifiaction model using multinomial log-linear regions via neural networks
#' @name sits_mlr
#'
#' @author Alexandre Xavier Ywata de Carvalho, \email{alexandre.ywata@@ipea.gov.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Use multinomial log-linear (mlr) fitting model via neural networks to classify data.
#' These attributes are distance metrics between patterns and observations, obtained by a distance
#' function in sits (see \code{\link[sits]{sits_distances}}).
#' This function is a front-end to the "multinom" method in the "nnet" package.
#' Please refer to the documentation in that package for more details.
#'
#' @param data.tb          Time series with the training samples.
#' @param formula          Symbolic description of the model to be fit. Package sits offers a set of such formulas (default: sits_formula_logref).
#' @param n_weights        Maximum number of weights (should be proportional to size of input data).
#' @param maxit            Maximum number of iterations (default 300).
#' @param ...              Other parameters to be passed to nnet::multinom function.
#' @return A model function to be passed in sits_predict.
#' @examples
#' \donttest{
#' # Retrieve the set of samples for the Mato Grosso region (provided by EMBRAPA)
#' data(samples_MT_ndvi)
#' # Build an MLR model
#' mlr_model <- sits_train(samples_MT_ndvi, sits_mlr())
#' # get a point with a 16 year time series
#' data(point_ndvi)
#' # classify the point
#' class.tb <- sits_classify (point_ndvi, mlr_model)
#' # plot the classification
#' sits_plot(class.tb)
#' }
#' @export
sits_mlr <- function(data.tb = NULL, formula = sits_formula_linear(),
                     n_weights = 20000, maxit = 2000, ...) {
    # function that returns nnet::multinom model based on a sits sample tibble
    result_fun <- function(data.tb) {

        stats.tb <- .sits_normalization_param(data.tb)
        train_data_DT <- sits_distances(sits_normalize_data(data.tb, stats.tb))

        # if parameter formula is a function call it passing as argument the input data sample. The function must return a valid formula.
        if (class(formula) == "function")
            formula <- formula(train_data_DT)

        # call nnet::multinom method and return the trained multinom model
        result_mlr <- nnet::multinom(formula = formula,
                                     data = train_data_DT,
                                     maxit = maxit,
                                     MaxNWts = n_weights, ..., na.action = stats::na.fail)

        # construct model predict closure function and returns
        model_predict <- function(values_DT){
            # return probabilities
            prediction_DT <- data.table::as.data.table(stats::predict(result_mlr, newdata = values_DT, type = "probs"))

            return(prediction_DT)
        }
        return(model_predict)
    }

    result <- .sits_factory_function(data.tb, result_fun)
    return(result)
}

#' @title Train a sits classifiction model using fast random forest algorithm
#' @name sits_rfor
#'
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Alexandre Xavier Ywata de Carvalho, \email{alexandre.ywata@@ipea.gov.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Use Fast Random Forest algorithm to classify data.
#' This function is a front-end to the "ranger" method in the "ranger" package.
#' Please refer to the documentation in that package for more details.
#'
#' @param data.tb          Time series with the training samples.
#' @param num.trees        Number of trees to grow. This should not be set to too small a number,
#'                         to ensure that every input row gets predicted at least a few times. (default: 2000).
#' @param ...              Other parameters to be passed to \code{\link[ranger]{ranger}} function.
#' @return Either a model function to be passed in sits_predict or an function prepared that can be called further to compute multinom training model.
#' @examples
#' \donttest{
#' # Retrieve the set of samples for the Mato Grosso region (provided by EMBRAPA)
#' data(samples_MT_ndvi)
#' # Build a random forest model
#' rfor_model <- sits_train(samples_MT_ndvi, sits_rfor())
#' # get a point with a 16 year time series
#' data(point_ndvi)
#' # classify the point
#' class.tb <- sits_classify (point_ndvi, rfor_model)
#' # plot the classification
#' sits_plot(class.tb)
#' }
#' @export
sits_rfor <- function(data.tb = NULL, num.trees = 2000, ...) {
    # function that returns a randomForest model based on a sits sample tibble
    result_fun <- function(data.tb){

        # get the labels of the data
        labels <- sits_labels(data.tb)$label

        # create a named vector with integers match the class labels
        int_labels <- c(1:length(labels))
        names(int_labels) <- labels

        # calculate the distances
        train_data_DT <- sits_distances(data.tb)

        # if parameter formula is a function call it passing as argument the input data sample. The function must return a valid formula.
        formula <- sits_formula_linear()(train_data_DT)

        # call `ranger::ranger` method and return the trained model
        result_ranger <- ranger::ranger(formula = formula,
                                      data = train_data_DT[,2:ncol(train_data_DT)],
                                      probability = TRUE,
                                      num.trees = num.trees, min.node.size = 1, ...)

        # construct model predict closure function and return it
        model_predict <- function(values_DT){
            # retrieve the prediction results
            preds  <-  stats::predict(result_ranger, data = values_DT, type = "response")
            # return the prediction values and their probabilities
            prediction_DT <- data.table::as.data.table(preds$predictions)

            return(prediction_DT)
        }
        return(model_predict)
    }

    result <- .sits_factory_function(data.tb, result_fun)
    return(result)
}

#' @title Train a sits classification model using a support vector machine
#' @name sits_svm
#'
#' @author Alexandre Xavier Ywata de Carvalho, \email{alexandre.ywata@@ipea.gov.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function receives a tibble with a set of attributes X for each observation Y.
#' These attributes are distance metrics between patterns and observations, obtained by a distance
#' function in sits (see \code{\link[sits]{sits_distances}}).
#' The SVM algorithm is used for multiclass-classification.
#' For this purpose, it uses the "one-against-one" approach, in which k(k-1)/2 binary
#' classifiers are trained; the appropriate class is found by a voting scheme.
#' This function is a front-end to the "svm" method in the "e1071" package.
#' Please refer to the documentation in that package for more details.
#'
#' @param data.tb          Time series with the training samples.
#' @param formula          Symbolic description of the model to be fit. Package sits offers a set of such formulas (default: sits_svm).
#' @param scale            Logical vector indicating the variables to be scaled.
#' @param cachesize        Cache memory in MB (default = 1000).
#' @param kernel           Kernel used in training and predicting. Available options are "linear", "polynomial", "radial", "sigmoid" (default: "radial").
#' @param degree           Exponential of polynomial type kernel (default: 3).
#' @param coef0            Parameter needed for kernels of type polynomial and sigmoid (default: 0).
#' @param cost             Cost of constraints violation.
#' @param tolerance        Tolerance of termination criterion (default: 0.001).
#' @param epsilon          Epsilon in the insensitive-loss function (default: 0.1).
#' @param cross            Number of cross validation folds applied on the training data to assess the quality of the model.
#' @param ...              Other parameters to be passed to e1071::svm function.
#'
#' @examples
#' \donttest{
#' # Retrieve the set of samples for the Mato Grosso region (provided by EMBRAPA)
#' data(samples_MT_ndvi)
#' # Build an SVM model
#' svm_model <- sits_train(samples_MT_ndvi, sits_svm())
#' # get a point
#' data(point_ndvi)
#' # classify the point
#' class.tb <- sits_classify (point_ndvi, svm_model)
#' # plot the classification
#' sits_plot(class.tb)
#'}
#' @export
sits_svm <- function(data.tb = NULL, formula = sits_formula_logref(), scale = FALSE, cachesize = 1000,
                     kernel = "radial", degree = 3, coef0 = 0, cost = 10, tolerance = 0.001, epsilon = 0.1, cross = 0, ...) {
    # function that returns e1071::svm model based on a sits sample tibble
    result_fun <- function(data.tb){

        # data normalization
        stats.tb <- .sits_normalization_param(data.tb)
        train_data_DT <- sits_distances(sits_normalize_data(data.tb, stats.tb))

        # if parameter formula is a function call it passing as argument the input data sample.
        # The function must return a valid formula.
        if (class(formula) == "function")
            formula <- formula(train_data_DT)

        # call e1071::svm method and return the trained svm model
        result_svm <- e1071::svm(formula = formula, data = train_data_DT, scale = scale, kernel = kernel,
                                 degree = degree, cost = cost, coef0 = coef0, cachesize = cachesize,
                                 tolerance = tolerance, epsilon = epsilon, cross = cross,
                                 probability = TRUE, ..., na.action = stats::na.fail)

        # construct model predict closure function and returns
        model_predict <- function(values_DT){
            # try load e1071 dependency
            ensurer::ensure_that("e1071", require(., quietly = TRUE, warn.conflicts = FALSE, character.only = TRUE),
                                 err_desc = "sits_svm: library 'e1071' load failed.")
            # get the prediction
            preds <- stats::predict(result_svm, newdata = values_DT, probability = TRUE)
            # retrieve the predicted probabilities
            prediction_DT <- data.table::as.data.table(attr(preds, "probabilities"))
            # reorder the matrix according to the column names
            data.table::setcolorder(prediction_DT, sort(colnames(prediction_DT)))

            return(prediction_DT)
        }
        return(model_predict)
    }
    result <- .sits_factory_function(data.tb, result_fun)
    return(result)
}

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
#' dl_model <- sits_train(cerrado2classes,
#'      sits_deeplearning(units = c(512, 512),
#'      dropout_rates = c(0.45, 0.25)))
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
#' @param predictors_index  Index of the valid columns whose names are used to compose formula (default: -2:0).
#' @return A function that computes a valid formula.
#'
#' @export
sits_formula_logref <- function(predictors_index = -2:0){
    # store configuration information about model formula
    sits.env$model_formula <- "log"

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
#' @param predictors_index  Index of the valid columns whose names are used to compose formula (default: NULL).
#' @return A function that computes a valid formula.
#'
#' @export
sits_formula_linear <- function(predictors_index = -2:0){
    # store configuration information about model formula
    sits.env$model_formula <- "linear"

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
#' @param predictors_index  Index of the valid columns whose names are used to compose formula (default: NULL).
#' @return A function that computes a valid formula.
#'
#' @export
sits_formula_smooth <- function(predictors_index = -2:0){
    # store configuration information about model formula
    sits.env$model_formula <-  "smooth"

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
#' @description Given a sits tibble time series and a model trained by \code{\link[sits]{sits_train}},
#'   returns a predicted label. Note that the \code{\link[sits]{.sits_predict}} function is
#'   called inside \code{\link[sits]{sits_classify}},
#'   and \code{\link[sits]{sits_classify_raster}}, so the user does not need
#'   to explicitly use it. Please see the above-mentioned classification functions.
#'
#' @param distances_DT  Set of distance metrics to each of the classes.
#' @param ml_model      Model trained by \code{\link[sits]{sits_train}}.
#' @param ...           Other parameters to be passed to the model function.
#' @return A vector of predicted labels.
.sits_predict <- function(distances_DT = NULL, ml_model, ...){
    prediction <- ml_model(distances_DT)
    return(prediction)
}
