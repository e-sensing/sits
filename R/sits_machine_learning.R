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
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
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
#' @author Victor Maus, \email{vwmaus1@@gmail.com}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
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
#' @return result          either an e1071::svm class or an function prepared that can be called further to compute svm training model
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
        return(result_svm)
    }

    result <- .sits_factory_function (distances.tb, result_fun)
}

#' @title Train SITS classifiction models
#' @name sits_formula_logref
#'
#' @author Alexandre Xavier Ywata de Carvalho, \email{alexandre.ywata@@ipea.gov.br}
#' @author Victor Maus, \email{vwmaus1@@gmail.com}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
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
#' @author Victor Maus, \email{vwmaus1@@gmail.com}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Given a SITS tibble time series and a model trained by \code{\link[sits]{sits_train}},
#' returns a SITS tibble with the classification.
#'
#' @param data.tb a SITS tibble time series
#' @param distances.tb a tibble with a set of distance metrics to each of the classes
#' @param model a model trained by \code{\link[sits]{sits_train}}
#' @return data.tb - a SITS tibble with the predicted label
#'
#' @export
sits_predict <- function(data.tb = NULL, distances.tb = NULL, model){

    # is the input data the result of a TWDTW matching function?
    ensurer::ensure_that(distances.tb, "reference" %in% names (.), err_desc = "sits_train_svm: input data does not contain TWDTW matches")

    data.tb$predicted <- as.character(stats::predict(model, newdata = distances.tb))

    return(data.tb)
}
