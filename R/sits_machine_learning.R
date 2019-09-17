#' @title Train sits classification models
#' @name sits_train
#'
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Alexandre Xavier Ywata de Carvalho, \email{alexandre.ywata@@ipea.gov.br}
#'
#' @description Given a tibble with a set of distance measures,
#'    returns trained models. Currenly, sits supports the following models:
#' 'svm' (see \code{\link[sits]{sits_svm}}),
#' random forests (see \code{\link[sits]{sits_rfor}}),
#' linear discriminant analysis (see \code{\link[sits]{sits_lda}}),
#' quadratic discriminant analysis (see \code{\link[sits]{sits_qda}}),
#' multinomial logit (see \code{\link[sits]{sits_mlr}}) and its variants
#' 'lasso' (see \code{\link[sits]{sits_mlr}}) and
#' 'ridge' (see \code{\link[sits]{sits_mlr}}),
#' extreme gradient boosting (see \code{\link[sits]{sits_xgboost}}),
#' and different deep learning functions, including multi-layer perceptrons
#' (see \code{\link[sits]{sits_deeplearning}}, 1D convolutional neural networks
#' \code{\link[sits]{sits_FCN}}, mixed 1D and MLP networks \code{\link[sits]{sits_TempCNN}}
#' a 1D version of ResNet \code{\link[sits]{sits_ResNet}}), and a combined LSTM-FCN model
#' \code{\link[sits]{sits_LSTM_FCN}}.
#'
#' @param  data             Time series with the training samples.
#' @param  ml_method        Machine learning method that returns a model for prediction.
#' @return A model fitted to input data to be passed to \code{\link[sits]{sits_classify}}
#'
#' @examples
#' \donttest{
#' # Retrieve the set of samples for the Mato Grosso region (provided by EMBRAPA)
#' # find a training model based on the distances and default values (RFOR model)
#' samples_2bands <- sits_select_bands(samples_mt_6bands, ndvi, evi)
#' ml_model <- sits_train(samples_2bands, sits_rfor(num_trees = 1000))
#' # get a point and classify the point with the ml_model
#' point.tb <- sits_select_bands(point_mt_6bands, ndvi, evi)
#' class.tb <- sits_classify(point.tb, ml_model)
#' sits_plot(class.tb)
#' }
#' @export
sits_train <- function(data, ml_method = sits_svm()) {
    # backward compatibility
    if ("coverage" %in% names(data))
        data <- .sits_tibble_rename(data)

    # is the input data a valid sits tibble?
    ensurer::ensure_that(data, "label" %in% names(.),
                         err_desc = "sits_train: input data does not contain a valid sits tibble")

    # is the train method a function?
    ensurer::ensure_that(ml_method, class(.) == "function",
                         err_desc = "sits_train: ml_method is not a valid function")

    # compute the training method by the given data
    result <- ml_method(data)

    # return a valid machine learning method
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
#' function in sits (see \code{\link[sits]{.sits_distances}}).
#' The method performs a linear discriminant analysis (lda) to obtain a predictive model.
#' This function is a front-end to the "lda" method in the "MASS" package.
#' Please refer to the documentation in that package for more details.
#'
#' @param data             Time series with the training samples.
#' @param formula          A symbolic description of the model to be fit. Package sits offers a set of such formulas (default: sits_formula_logref).
#' @param ...              Other parameters to be passed to MASS::lda function.
#' @return A model fitted to input data to be passed to \code{\link[sits]{sits_classify}}
#'
#' @examples
#' \donttest{
#' # Retrieve the set of samples for the Mato Grosso region (provided by EMBRAPA)
#' samples_2bands <- sits_select_bands(samples_mt_6bands, ndvi, evi)
#'
#' # Build a machine learning model based on deep learning
#' ml_model <- sits_train (samples_2bands, sits_lda())
#'
#' # get a point and classify the point with the ml_model
#' point.tb <- sits_select_bands(point_mt_6bands, ndvi, evi)
#' class.tb <- sits_classify(point.tb, ml_model)
#' sits_plot(class.tb)
#' }
#' @export
sits_lda <- function(data = NULL, formula = sits_formula_logref(), ...) {
    # backward compatibility
    if ("coverage" %in% names(data))
        data <- .sits_tibble_rename(data)
    # function that returns MASS::lda model based on a sits sample tibble
    result_fun <- function(data){

        # data normalization
        stats <- .sits_normalization_param(data)
        train_data_DT <- .sits_distances(.sits_normalize_data(data, stats))

        # is the input data the result of a TWDTW matching function?
        ensurer::ensure_that(train_data_DT, "reference" %in% names(.),
                             err_desc = "sits_lda: input data does not contain distance")

        # if parameter formula is a function call it passing as argument the input data sample.
        # The function must return a valid formula.
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

    result <- .sits_factory_function(data, result_fun)
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
#' function in sits (see \code{\link[sits]{.sits_distances}}).
#' The function performs a quadratic discriminant analysis (qda) to obtain a predictive model.
#' This function is a front-end to the "qda" method in the "MASS" package.
#' Please refer to the documentation in that package for more details.
#'
#' @param data          Time series with the training samples.
#' @param formula          Symbolic description of the model to be fit. Package sits offers a set of such formulas (default: sits_formula_logref).
#' @param ...              Other parameters to be passed to MASS::qda function.
#' @return A model fitted to input data to be passed to \code{\link[sits]{sits_classify}}
#'
#' @examples
#' \donttest{
#' # Retrieve the set of samples for the Mato Grosso region (provided by EMBRAPA)
#' # Select the NDVI band
#' samples_mt_ndvi <- sits_select_bands(samples_mt_4bands, ndvi)
#' # Train a QDA model
#' qda_model <- sits_train(samples_mt_ndvi, sits_qda())
#' # Classify a point
#' class.tb <- sits_classify(point_ndvi, qda_model)
#' # Plot results
#' sits_plot(class.tb)
#' }
#' @export
sits_qda <- function(data = NULL, formula = sits_formula_logref(), ...) {
    # backward compatibility
    if ("coverage" %in% names(data))
        data <- .sits_tibble_rename(data)
    # function that returns MASS::qda model based on a sits sample tibble
    result_fun <- function(data){

        # data normalization
        stats <- .sits_normalization_param(data)
        train_data_DT <- .sits_distances(.sits_normalize_data(data, stats))

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
    result <- .sits_factory_function(data, result_fun)
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
#' function in sits (see \code{\link[sits]{.sits_distances}}).
#' This function is a front-end to the "multinom" method in the "nnet" package.
#' Please refer to the documentation in that package for more details.
#'
#' @param data             Time series with the training samples.
#' @param formula          Symbolic description of the model to be fit. Package sits offers a set of such formulas (default: sits_formula_logref).
#' @param n_weights        Maximum number of weights (should be proportional to size of input data).
#' @param maxit            Maximum number of iterations (default 300).
#' @param ...              Other parameters to be passed to nnet::multinom function.
#' @return A model fitted to input data to be passed to \code{\link[sits]{sits_classify}}
#' @examples
#' \donttest{
#' # Retrieve the set of samples for the Mato Grosso region (provided by EMBRAPA)
#' samples_2bands <- sits_select_bands(samples_mt_6bands, ndvi, evi)
#'
#' # Build a machine learning model based on deep learning
#' ml_model <- sits_train (samples_2bands, sits_mlr())
#'
#' # get a point and classify the point with the ml_model
#' point.tb <- sits_select_bands(point_mt_6bands, ndvi, evi)
#' class.tb <- sits_classify(point.tb, ml_model)
#' sits_plot(class.tb)
#' }
#' @export
sits_mlr <- function(data = NULL, formula = sits_formula_linear(),
                     n_weights = 20000, maxit = 2000, ...) {
    # backward compatibility
    if ("coverage" %in% names(data))
        data <- .sits_tibble_rename(data)
    # function that returns nnet::multinom model based on a sits sample tibble
    result_fun <- function(data) {
        # data normalization
        stats <- .sits_normalization_param(data)
        train_data_DT <- .sits_distances(.sits_normalize_data(data, stats))

        # if parameter formula is a function call it passing as argument the input data sample. The function must return a valid formula.
        if (class(formula) == "function")
            formula <- formula(train_data_DT)

        # call nnet::multinom method and return the trained multinom model
        result_mlr <- nnet::multinom(formula = formula,
                                     data = train_data_DT,
                                     maxit = maxit,
                                     MaxNWts = n_weights,
                                     trace = FALSE, ..., na.action = stats::na.fail)

        # construct model predict closure function and returns
        model_predict <- function(values_DT){
            # return probabilities
            prediction_DT <- data.table::as.data.table(stats::predict(result_mlr, newdata = values_DT, type = "probs"))

            return(prediction_DT)
        }
        return(model_predict)
    }

    result <- .sits_factory_function(data, result_fun)
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
#' @param data          Time series with the training samples.
#' @param num_trees     Number of trees to grow. This should not be set to too small a number,
#'                         to ensure that every input row gets predicted at least a few times. (default: 2000).
#' @param importance   Variable importance mode, one of 'none', 'impurity', 'impurity_corrected', 'permutation'.
#'                     The 'impurity' measure is the Gini index for classification.
#' @param ...          Other parameters to be passed to \code{\link[ranger]{ranger}} function.
#' @return A model fitted to input data to be passed to \code{\link[sits]{sits_classify}}
#' @examples
#' \donttest{
#' # Retrieve the set of samples for the Mato Grosso region (provided by EMBRAPA)
#' samples_2bands <- sits_select_bands(samples_mt_6bands, ndvi, evi)
#'
#' # Build a machine learning model based on deep learning
#' ml_model <- sits_train (samples_2bands, sits_rfor())
#'
#' # get a point and classify the point with the ml_model
#' point.tb <- sits_select_bands(point_mt_6bands, ndvi, evi)
#' class.tb <- sits_classify(point.tb, ml_model)
#' sits_plot(class.tb)
#' }
#' @export
sits_rfor <- function(data = NULL, num_trees = 2000, importance = "impurity", ...) {
    # backward compatibility
    if ("coverage" %in% names(data))
        data <- .sits_tibble_rename(data)
    # function that returns a randomForest model based on a sits sample tibble
    result_fun <- function(data){

        valid_importance <- c("none", "impurity", "permutation")

        # is the input data consistent?
        ensurer::ensure_that(importance, (.) %in% valid_importance,
                             err_desc = "sits_rfor: invalid variable importance value")

        # get the labels of the data
        labels <- sits_labels(data)$label

        # create a named vector with integers match the class labels
        int_labels <- c(1:length(labels))
        names(int_labels) <- labels

        # calculate the distances
        train_data_DT <- .sits_distances(data)

        # if parameter formula is a function call it passing as argument the input data sample. The function must return a valid formula.
        formula <- sits_formula_linear()(train_data_DT)

        # call `ranger::ranger` method and return the trained model
        result_ranger <- ranger::ranger(formula = formula,
                                      data = train_data_DT[,2:ncol(train_data_DT)],
                                      probability = TRUE, importance = importance,
                                      num.trees = num_trees, min.node.size = 1, ...)

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

    result <- .sits_factory_function(data, result_fun)
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
#' function in sits (see \code{\link[sits]{.sits_distances}}).
#' The SVM algorithm is used for multiclass-classification.
#' For this purpose, it uses the "one-against-one" approach, in which k(k-1)/2 binary
#' classifiers are trained; the appropriate class is found by a voting scheme.
#' This function is a front-end to the "svm" method in the "e1071" package.
#' Please refer to the documentation in that package for more details.
#'
#' @param data             Time series with the training samples.
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
#' @return A model fitted to input data to be passed to \code{\link[sits]{sits_classify}}
#' @examples
#' \donttest{
#' # Retrieve the set of samples for the Mato Grosso region (provided by EMBRAPA)
#' samples_2bands <- sits_select_bands(samples_mt_6bands, ndvi, evi)
#'
#' # Build a machine learning model based on deep learning
#' ml_model <- sits_train (samples_2bands, sits_svm())
#'
#' # get a point and classify the point with the ml_model
#' point.tb <- sits_select_bands(point_mt_6bands, ndvi, evi)
#' class.tb <- sits_classify(point.tb, ml_model)
#' sits_plot(class.tb)
#' }
#' @export
sits_svm <- function(data = NULL, formula = sits_formula_logref(), scale = FALSE, cachesize = 1000,
                     kernel = "radial", degree = 3, coef0 = 0, cost = 10, tolerance = 0.001, epsilon = 0.1, cross = 0, ...) {
    # backward compatibility
    if ("coverage" %in% names(data))
        data <- .sits_tibble_rename(data)
    # function that returns e1071::svm model based on a sits sample tibble
    result_fun <- function(data){

        # data normalization
        stats <- .sits_normalization_param(data)
        train_data_DT <- .sits_distances(.sits_normalize_data(data, stats))

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
    result <- .sits_factory_function(data, result_fun)
    return(result)
}


#' @title Train a sits classification model with an extreme gradient boosting machine
#' @name sits_xgboost
#'
#' @author Alexandre Xavier Ywata de Carvalho, \email{alexandre.ywata@@ipea.gov.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function implements the extreme gradient boosting algorithm.
#' Boosting is the process of iteratively adding basis functions in a greedy fashion
#' so that each additional basis function further reduces the selected loss function.
#' This function is a front-end to the methods in the "xgboost" package.
#' Please refer to the documentation in that package for more details.
#'
#' @references             Tianqi Chen, Carlos Guestrin,
#'                         "XGBoost : Reliable Large-scale Tree Boosting System", SIG KDD 2016.
#'
#' @param data             Time series with the training samples.
#' @param eta              Learning rate: scale the contribution of each tree by a factor of 0 < eta < 1 when it is added to the current approximation.
#'                         Used to prevent overfitting. Default: 0.3
#' @param gamma            Minimum loss reduction to make a further partition of a leaf.  Default: 0.
#' @param max_depth        Maximum depth of a tree.
#'                         Increasing this value will make the model more complex
#'                         and more likely to overfit, Default: 6.
#' @param min_child_weight If the leaf node has a minimum sum of instance weights lower than min_child_weight, the tree splitting stops.
#' @param subsample        Controls the percentage of samples (observations) supplied to a tree. Default: 1.
#' @param nfold            Number of the subsamples for the cross-validation.
#' @param nrounds          Number of rounds to iterate the cross-validation (default: 100)
#' @param early_stopping_rounds Training with a validation set will stop if the performance doesn't improve for k rounds.
#' @param verbose          Print information on statistics during the process
#' @param ...              Other parameters to be passed to `xgboost::xgboost` function.
#' @return A model fitted to input data to be passed to \code{\link[sits]{sits_classify}}
#'
#' @examples
#' \donttest{
#' # Retrieve the set of samples for the Mato Grosso region (provided by EMBRAPA)
#' samples_2bands <- sits_select_bands(samples_mt_6bands, ndvi, evi)
#'
#' # Build a machine learning model based on xgboost
#' xgb_model <- sits_train(samples_2bands, sits_xgboost(eta = 0.5, gamma = 0, max.depth = 2))
#'
#' # get a point and classify the point with the ml_model
#' point.tb <- sits_select_bands(point_mt_6bands, ndvi, evi)
#' class.tb <- sits_classify(point.tb, xgb_model)
#' sits_plot(class.tb)
#' }
#' @export
sits_xgboost <- function(data = NULL, eta = 0.3, gamma = 0, max_depth = 6, min_child_weight = 1,
                         subsample = 1, nfold = 5, nrounds = 100, early_stopping_rounds = 20,
                         verbose = FALSE, ...) {
    # function that returns glmnet::multinom model based on a sits sample tibble
    result_fun <- function(data){

        # data normalization
        stats <- .sits_normalization_param(data)
        train_data_DT <- .sits_distances(.sits_normalize_data(data, stats))

        # get the labels of the data
        labels <- sits_labels(data)$label
        # create a named vector with integers match the class labels
        int_labels <- c(1:length(labels))
        names(int_labels) <- labels

        # reference labels for each sample expressed as numerical values
        references <- unname(int_labels[as.vector(train_data_DT$reference)]) - 1

        # define the parameters of the model
        params <- list(booster = "gbtree", objective = "multi:softprob", eta = eta, gamma = gamma,
                       max_depth = max_depth, min_child_weight = min_child_weight,
                       subsample = subsample)

        # run the cross-validation
        xgbcv <- xgboost::xgb.cv(params = params, data = as.matrix(train_data_DT[, 3:length(train_data_DT)]),
                                 label = references, num_class = length(labels),
                                 nrounds = nrounds, nfold = nfold, early_stopping_rounds = early_stopping_rounds,
                                 print_every_n = 10,  verbose = verbose, maximize = FALSE)

        # get the best iteration of the model based on the CV
        nrounds_best <- xgbcv$best_iteration

        # call gbm::gbm method and return the trained multinom model
        result_xgb <- xgboost::xgb.train(data = xgboost::xgb.DMatrix(data = as.matrix(train_data_DT[, 3:length(train_data_DT)]), label = references),
                                         num_class = length(labels),
                                         params = params, nrounds = nrounds_best,
                                         print_every_n = 10,  maximize = FALSE)

        # construct model predict closure function and returns
        model_predict <- function(values_DT){
            # transform input (data.table) into a matrix (remove first two columns)
            # retrieve the prediction probabilities
            prediction_DT <- data.table::as.data.table(stats::predict(result_xgb, data.matrix(values_DT[, -(1:2)]), reshape = TRUE))
            # adjust the names of the columns of the probs
            colnames(prediction_DT) <- labels
            # retrieve the prediction results
            return(prediction_DT)
        }
        return(model_predict)
    }

    result <- .sits_factory_function(data, result_fun)
    return(result)
}

#' @title Define a loglinear formula for classification models
#' @name sits_formula_logref
#'
#' @author Alexandre Xavier Ywata de Carvalho, \email{alexandre.ywata@@ipea.gov.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description A function to be used as a symbolic description of some fitting models such as svm and random forest.
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

#' @title Use time series values from a sits tibble as distances for training patterns
#' @name .sits_distances
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function allows using a set of labelled time series as
#' input to the machine learning models. The attributes used to train the model
#' are the series themselves. This function extracts the time series from a sits tibble
#' and "spreads" them in time to produce a tibble with distances.
#'
#' @param  data       A tibble with time series data and metadata.
#' @return A data.table where columns have the reference label and the time series values as distances.
#'
.sits_distances <- function(data) {
    # backward compatibility
    if ("coverage" %in% names(data))
        data <- .sits_tibble_rename(data)

    # create a list with the time series transposed from columns to rows
    ts.lst <- data$time_series %>%
        purrr::map(function(ts){
            as.data.frame(t(unlist(ts[-1])))
        })
    # bind the lists of time series together
    dist_DT <- data.table::rbindlist(ts.lst, use.names = FALSE)
    # create a data frame with the first two columns for training
    distances_DT <- data.table::data.table("original_row" = 1:nrow(data), "reference" = data$label)
    # join the two references columns with the data values
    distances_DT <- data.table::as.data.table(cbind(distances_DT, dist_DT))

    return(distances_DT)
}

#' @title Sample a percentage of a time series distance matrix
#' @name .sits_sample_distances
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Takes a sits tibble with different labels and
#'              returns a new tibble. For a given field as a group criterion, this new table contains a given number or percentage
#'              of the total number of samples per group. Parameter n indicantes the number of random samples with reposition.
#'              Parameter frac indicates a fraction of random samples without reposition. If frac > 1, no sampling is done.
#'
#' @param  distances_DT    A data.table object with the distances associated to a time series.
#' @param  frac            Percentage of samples to pick from a given group of data.
#' @return A data.table with a fixed quantity of samples of informed labels and all other.
.sits_sample_distances <- function(distances_DT, frac){
    # compute sampling
    result_DT <- distances_DT[, .SD[sample(.N, round(frac*.N))], by = reference]

    return(result_DT)
}
#' @title Normalize the time series in the given sits_tibble
#' @name .sits_normalize_data
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description This function normalizes the time series using the mean and
#' standard deviation of all the time series.
#'
#' @param data     A sits tibble.
#' @param stats    Statistics for normalization.
#' @param multicores  Number of cores to process.
#' @return A normalized sits tibble.
.sits_normalize_data <- function(data, stats, multicores = 1){
    # backward compatibility
    if ("coverage" %in% names(data))
        data <- .sits_tibble_rename(data)
    .sits_test_tibble(data)
    # find the number of cores
    if (purrr::is_null(multicores))
        multicores <- max(parallel::detectCores(logical = FALSE) - 1, 1)
    # avoid overhead on multicore processing
    if (nrow(data) < sits.env$config$minimum_number_samples)
        multicores <- 1
    # get the bands of the input data
    bands <- sits_bands(data)
    # check that the bands in the input are include in the statistics already calculated
    ensurer::ensure_that(bands, all(sort(.) == sort(colnames(stats[,-1]))),
                         err_desc = paste0("sits_normalize: bands in the data (",
                                           paste(bands, collapse = ", "),
                                           ") do not match bands in the model (",
                                           paste(colnames(stats[,-1]), collapse = ", "), ")"))

    # extract the values of the time series to a list of tibbles
    values.lst <- data$time_series

    normalize_list <- function(chunk.lst) {
        norm_chunk.lst <- chunk.lst %>%
            purrr::map(function(ts) {
                norm.lst <- bands %>%
                    purrr::map(function(b){
                        med      <- as.numeric(stats[1, b])
                        quant_2  <- as.numeric(stats[2, b])
                        quant_98 <- as.numeric(stats[3, b])
                        values <- suppressWarnings(tibble::as_tibble(normalize_data(as.matrix(ts[,b]), quant_2, quant_98)))
                        return(values)
                    })
                ts.tb <- dplyr::bind_cols(norm.lst)
                ts.tb <- dplyr::bind_cols(list(ts[,1], ts.tb))
                colnames(ts.tb) <- colnames(ts)
                return(ts.tb)
            })
        return(norm_chunk.lst)
    }

    if (multicores > 1) {
        parts.lst <- split(values.lst, cut(1:length(values.lst), 2, labels = FALSE))
        norm.lst <- dplyr::combine(parallel::mclapply(parts.lst, normalize_list, mc.cores = multicores))
    }
    else
        norm.lst <- normalize_list(values.lst)

    data$time_series <- norm.lst
    return(data)
}

#' @title Normalize the time series values in the case of a matrix
#' @name .sits_normalize_matrix
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description this function normalizes one band of the values read from a raster brick
#'
#' @param  data.mx        Matrix of values.
#' @param  stats       Statistics for normalization.
#' @param  band           Band to be normalized.
#' @param  multicores     Number of cores.
#' @return A normalized matrix.
.sits_normalize_matrix <- function(data.mx, stats, band, multicores) {
    # select the 2% and 98% quantiles
    quant_2   <- as.numeric(stats[2, band])
    quant_98  <- as.numeric(stats[3, band])

    # auxiliary function to normalize a block of data
    normalize_block <- function(chunk, quant_2, quant_98) {
        # normalize a block of data
        values_block.mx <- normalize_data(chunk, quant_2, quant_98)
    }

    # parallel processing for normalization
    if (multicores > 1) {
        chunk.lst <- .sits_raster_split_data(data.mx, multicores)
        rows.lst  <- parallel::mclapply(chunk.lst, normalize_block, quant_2, quant_98, mc.cores = multicores)
        data.mx <- do.call(rbind, rows.lst)
        rm(chunk.lst)
        rm(rows.lst)
        gc()
    }
    else
        data.mx <- normalize_data(data.mx, quant_2, quant_98)

    return(data.mx)
}


#' @title Normalize the time series in the given sits_tibble
#' @name .sits_normalization_param
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description this function normalizes the time series using the mean and
#' standard deviation of all the time series.
#'
#' @param data     A sits tibble.
#' @return A tibble with statistics.
.sits_normalization_param <- function(data) {
    .sits_test_tibble(data)

    DT <- data.table::data.table(dplyr::bind_rows(data$time_series))
    DT[, Index := NULL]

    # compute statistics
    DT_med      <- DT[, lapply(.SD, stats::median, na.rm = TRUE)]
    DT_quant_2  <- DT[, lapply(.SD, function(x) stats::quantile(x, 0.02, na.rm = TRUE))]
    DT_quant_98 <- DT[, lapply(.SD, function(x) stats::quantile(x, 0.98, na.rm = TRUE))]

    stats <- dplyr::bind_cols(stats = c("med", "quant_2", "quant_98"),
                                 dplyr::bind_rows(DT_med, DT_quant_2, DT_quant_98))

    return(stats)
}

