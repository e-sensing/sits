#' Functions for machine learning associated to the SITS package
#' The attributes for the training functions are the DTW distances
#' computed by the TWTDW function (see documentation on sits_TWDTW_matches)
#'
#'
#' models supported: 'svm', 'random forests', 'boosting', 'lda',
#'                   'multinomial logit', 'lasso', 'ridge', 'elnet', 'best model'

#' @title Train SITS classification models
#' @name sits_train
#'
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Alexandre Xavier Ywata de Carvalho, \email{alexandre.ywata@@ipea.gov.br}
#'
#' @description Given a tibble with a set of distance measures,
#' returns trained models using support vector machines. This function will
#' use the TWDTW alignment information for all classes as the attributes
#' of the chosen machine learning methods. Please use this function in the following way:
#' 1. call sits_patterns to produce a set a labelled patterns from a reference data set
#' 2. call a method to get distances between a time series and patterns to produce a set of alignements
#' 3. use the distances tibble as an input to the training function
#'
#'
#' @param distances.tb     a time series with a set of distance measures for each training sample
#' @param tr_method        a traning method that returns a model for prediction
#' @return result          a model fitted into input data given by train_method parameter
#' @export
#'
sits_train <- function(distances.tb, tr_method = sits_svm()){

    # is the input data the result of a TWDTW matching function?
    ensurer::ensure_that(distances.tb, "reference" %in% names (.), err_desc = "sits_train: input data does not contain TWDTW matches")

    # is the train method a function?
    ensurer::ensure_that(tr_method, class(.) == "function", err_desc = "sits_train: train_method is not a valid function")

    # compute the training method by the given data
    result <- tr_method(distances.tb)
    return(result)
}

#' @title Train SITS classifiction models with the SVM method
#' @name sits_svm
#'
#' @author Alexandre Xavier Ywata de Carvalho, \email{alexandre.ywata@@ipea.gov.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description This function is a front-end to the "svm" method in the "e1071" package.
#'              Please refer to the documentation in that package for more details.
#'
#' @param distances.tb     a time series with a set of distance measures for each training sample
#' @param formula          a symbolic description of the model to be fit. SITS offers a set of such formulas (default: sits_svm)
#' @param kernel           the kernel used in training and predicting (options = linear, polynomial, radial basis, sigmoid)
#' @param degree           exponential of polynomial type kernel
#' @param coef0	           parameter needed for kernels of type polynomial and sigmoid (default: 0)
#' @param cost             cost of constraints violation
#' @param tolerance	       tolerance of termination criterion (default: 0.001)
#' @param epsilon	       epsilon in the insensitive-loss function (default: 0.1)
#' @param ...              other parameters to be passed to e1071::svm function
#' @return result          either an model function to be passed in sits_predict or an function prepared that can be called further to compute multinom training model
#' @export
#'
sits_svm <- function(distances.tb = NULL, formula = sits_formula_logref(), kernel = "linear",
                     degree = 3, coef0 = 0, cost = 1, tolerance = 0.001, epsilon = 0.1, ...) {

    # function that returns e1071::svm model based on a sits sample tibble
    result_fun <- function(tb){

        # is the input data the result of a TWDTW matching function?
        ensurer::ensure_that(tb, "reference" %in% names (.), err_desc = "sits_svm: input data does not contain distance")

        # if parameter formula is a function call it passing as argument the input data sample. The function must return a valid formula.
        if (class(formula) == "function")
            formula <- formula(tb)

        # call e1071::svm method and return the trained svm model
        result_svm <- e1071::svm(formula = formula, data = tb, kernel = kernel,
                                 degree = degree, cost = cost, coef0 = coef0, tolerance = tolerance, epsilon = epsilon, ...)

        # construct model predict enclosure function and returns
        model_predict <- function(test_distances.tb){
            return(stats::predict(result_svm, newdata = test_distances.tb))
        }
        return(model_predict)
    }

    result <- .sits_factory_function (distances.tb, result_fun)
}
#' @title Train SITS classifiction models
#' @name sits_lda
#'
#' @author Alexandre Xavier Ywata de Carvalho, \email{alexandre.ywata@@ipea.gov.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Performs a linear discriminant analysis (lda) to classify data.
#' This function is a front-end to the "lda" method in the "MASS" package.
#' Please refer to the documentation in that package for more details.
#'
#' @param distances.tb     a time series with a set of distance measures for each training sample
#' @param formula          a symbolic description of the model to be fit. SITS offers a set of such formulas (default: sits_formula_logref)
#' @param ...              other parameters to be passed to MASS::lda function
#' @return result          either an model function to be passed in sits_predict or an function prepared that can be called further to compute multinom training model
#' @export
#'
sits_lda <- function(distances.tb = NULL, formula = sits_formula_logref(), ...) {

    # function that returns MASS::lda model based on a sits sample tibble
    result_fun <- function(train_distances.tb){

        # is the input data the result of a TWDTW matching function?
        ensurer::ensure_that(train_distances.tb, "reference" %in% names (.), err_desc = "sits_lda: input data does not contain distance")

        # if parameter formula is a function call it passing as argument the input data sample. The function must return a valid formula.
        if (class(formula) == "function")
            formula <- formula(train_distances.tb)

        # call MASS::lda method and return the trained lda model
        result_lda <- MASS::lda(formula = formula, data = train_distances.tb, ...)

        # construct model predict enclosure function and returns
        model_predict <- function(test_distances.tb){
            return(stats::predict(result_lda, newdata = test_distances.tb))
        }
        return(model_predict)
    }

    result <- .sits_factory_function (distances.tb, result_fun)
    return(result)
}

#' @title Train SITS classifiction models
#' @name sits_mlr
#'
#' @author Alexandre Xavier Ywata de Carvalho, \email{alexandre.ywata@@ipea.gov.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Use multinomial log-linear (mlr) fitting model via neural networks to classify data.
#' This function is a front-end to the "multinom" method in the "nnet" package.
#' Please refer to the documentation in that package for more details.
#'
#' @param distances.tb     a time series with a set of distance measures for each training sample
#' @param formula          a symbolic description of the model to be fit. SITS offers a set of such formulas (default: sits_formula_logref)
#' @param ...              other parameters to be passed to nnet::multinom function
#' @return result          either an model function to be passed in sits_predict or an function prepared that can be called further to compute multinom training model
#' @export
#'
sits_mlr <- function(distances.tb = NULL, formula = sits_formula_logref(), ...) {

    # function that returns nnet::multinom model based on a sits sample tibble
    result_fun <- function(train_distances.tb){

        # is the input data the result of a TWDTW matching function?
        ensurer::ensure_that(train_distances.tb, "reference" %in% names (.), err_desc = "sits_mlr: input data does not contain distance")

        # if parameter formula is a function call it passing as argument the input data sample. The function must return a valid formula.
        if (class(formula) == "function")
            formula <- formula(train_distances.tb)

        # call nnet::multinom method and return the trained multinom model
        result_mlr <- nnet::multinom(formula = formula, data = train_distances.tb, ...)

        # construct model predict enclosure function and returns
        model_predict <- function(test_distances.tb){
            return(stats::predict(result_mlr, newdata = test_distances.tb))
        }
        return(model_predict)
    }

    result <- .sits_factory_function (distances.tb, result_fun)
    return(result)
}

#' @title Train SITS classifiction models
#' @name sits_glm
#'
#' @author Alexandre Xavier Ywata de Carvalho, \email{alexandre.ywata@@ipea.gov.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Use generalized liner model (glm) via penalized maximim likelihood to classify data.
#' This function is a front-end to the "cv.glmnet" method in the "glmnet" package.
#' Please refer to the documentation in that package for more details.
#'
#' @param distances.tb     a time series with a set of distance measures for each training sample
#' @param family           Response type. Can be either "gaussian", "binomial", "poisson", "multinomial", "cox", or "mgaussian". (default: "multinomial")
#' @param alpha            the elasticnet mixing parameter, with 0<=alpha<=1. Set alpha = 0.0 to obtain ridge model, and alpha = 1.0 to obtain lasso model).
#'                         (refer to `glmnet::cv.glmnet` function for more details)
#' @param lambda_kfolds    number of folds to find best lambda parameter (default: 10)
#' @param ...              other parameters to be passed to `glmnet::cv.glmnet` function
#' @return result          either an model function to be passed in sits_predict or an function prepared that can be called further to compute multinom training model
#' @export
#'
sits_glm <- function(distances.tb = NULL, family = "multinomial", alpha = 1.0, lambda_kfolds = 10, ...) {

    # function that returns glmnet::multinom model based on a sits sample tibble
    result_fun <- function(train_distances.tb){

        # is the input data the result of a TWDTW matching function?
        ensurer::ensure_that(train_distances.tb, "reference" %in% names (.), err_desc = "sits_glm: input data does not contain distance")

        # call glmnet::multinom method and return the trained multinom model
        result_glm <- glmnet::cv.glmnet(y = factor(data.matrix(train_distances.tb$reference)),
                                        x = log(data.matrix(train_distances.tb[,3:NCOL(train_distances.tb)])),
                                        family = family, alpha = alpha, k = lambda_kfolds, ...)

        # construct model predict enclosure function and returns
        model_predict <- function(test_distances.tb){
            return(stats::predict(result_glm,
                                  s = result_glm$lambda.min,
                                  newx = log(data.matrix(test_distances.tb[,3:NCOL(test_distances.tb)])), type = "class"))
        }
        return(model_predict)
    }

    result <- .sits_factory_function (distances.tb, result_fun)
    return(result)
}

#' @title Train SITS classifiction models
#' @name sits_rfor
#'
#' @author Alexandre Xavier Ywata de Carvalho, \email{alexandre.ywata@@ipea.gov.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Use Random Forest algorithm to classify data.
#' This function is a front-end to the "randomForest" method in the "randomForest" package.
#' Please refer to the documentation in that package for more details.
#'
#' @param distances.tb     a time series with a set of distance measures for each training sample
#' @param n_tree           Number of trees to grow. This should not be set to too small a number,
#'                         to ensure that every input row gets predicted at least a few times. (default: 500)
#' @param ...              other parameters to be passed to `randomForest::randomForest` function
#' @return result          either an model function to be passed in sits_predict or an function prepared that can be called further to compute multinom training model
#' @export
#'
sits_rfor <- function(distances.tb = NULL, n_tree = 500, ...) {

    # function that returns `randomForest::randomForest` model based on a sits sample tibble
    result_fun <- function(train_distances.tb){

        # is the input data the result of a TWDTW matching function?
        ensurer::ensure_that(train_distances.tb, "reference" %in% names (.), err_desc = "sits_mlr: input data does not contain distance")

        # call `randomForest::randomForest` method and return the trained multinom model
        result_rfor <- randomForest::randomForest(y = data.matrix(train_distances.tb$reference),
                                                  x = log(data.matrix(train_distances.tb[,2:NCOL(train_distances.tb)])),
                                                  data = NULL, ntree = n_tree, nodesize = 1,
                                                  norm.votes = FALSE, train_distances.tb, ...)

        # construct model predict enclosure function and returns
        model_predict <- function(test_distances.tb){
            return(stats::predict(result_rfor, newdata = test_distances.tb, type = "response"))
        }
        return(model_predict)
    }

    result <- .sits_factory_function (distances.tb, result_fun)
    return(result)
}

#' @title Train SITS classifiction models
#' @name sits_formula_logref
#'
#' @author Alexandre Xavier Ywata de Carvalho, \email{alexandre.ywata@@ipea.gov.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description A function to be used as a symbolic description of some fitting models such as svm.
#' `predictors_index` parameter informs the positions of `tb` fields corresponding to formula independent variables.
#' If no value is given, the default is NULL, a value indicating that all fields will be used as predictors.
#'
#' @param predictors_index  the index of the valid columns whose names are used to compose formula (default: NULL)
#' @return result_fun       a function that computes a valid formula
#' @export
#'
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

#' @title Predict class based on the trained models
#' @name sits_predict
#'
#' @author Alexandre Xavier Ywata de Carvalho, \email{alexandre.ywata@@ipea.gov.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Given a SITS tibble time series and a model trained by \code{\link[sits]{sits_train}},
#' returns a SITS tibble with the classification.
#'
#' @param data.tb       a SITS tibble time series
#' @param distances.tb  a tibble with a set of distance metrics to each of the classes
#' @param model         a model trained by \code{\link[sits]{sits_train}}
#' @param ...           other parameters to be passed to the model function
#' @return data.tb      a SITS tibble with the predicted label
#'
#' @export
sits_predict <- function(data.tb = NULL, distances.tb = NULL, model, ...){

    # is the input data the result of a TWDTW matching function?
    ensurer::ensure_that(distances.tb, "reference" %in% names (.), err_desc = "sits_train_svm: input data does not contain TWDTW matches")

    # is the input model a model function?
    ensurer::ensure_that(model, class (.) == "function", err_desc = "sits_predict: model parameter is not a function model returned by sits_train.")

    data.tb$predicted <- as.character(model(distances.tb))

    return(data.tb)
}
