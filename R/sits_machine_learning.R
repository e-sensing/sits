#' Functions for machine learning associated to the SITS package
#' The attributes for the training functions are the DTW distances
#' computed by the TWTDW function (see documentation on sits_TWDTW_matches)
#'
#'
#' models supported: 'svm', 'random forests', 'boosting', 'lda',
#'                   'multinomial logit', 'lasso', 'ridge', 'elnet', 'best model'

#' @title Train SITS classifiction models using support vector machines
#' @name sits_train_svm
#'
#' @author Alexandre Xavier Ywata de Carvalho, \email{alexandre.ywata@@ipea.gov.br}
#' @author Victor Maus, \email{vwmaus1@@gmail.com}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Given a SITS tibble time series with DTW matches produced by TWDTW,
#'              returns trained models using support vector machines. This function will
#'              use the TWDTW alignment information for all classes as the attributes
#'              of the SVM. Please use this function in the following way:
#'              1. call sits_patterns to produce a set a labelled patterns from a reference data set
#'              2. call sits_TWDTW_matches to produce a set of alignements
#'              3. use the alignment response as an input to the training function
#'
#'              This function is a front-end to the "svm" method in the "e1071" package.
#'              In the context of time series classification, the only relevant types are "C-classification" and "nu-classification".
#'              Please refer to the documentation in that package for more details.
#'
#' @param data_tb     a SITS tibble time series with an alignment column
#' @param kernel      the kernel used in training and predicting (options = linear, polynomial, radial basis, sigmoid)
#' @param degree      exponential of polynomial type kernel
#' @param coef0	      parameter needed for kernels of type polynomial and sigmoid (default: 0)
#' @param cost	      cost of constraints violation (default: 1): it is the Cconstant of the regularization term in the Lagrange formulation.
#' @param tolerance	  tolerance of termination criterion (default: 0.001)
#' @param epsilon	  epsilon in the insensitive-loss function (default: 0.1)
#' @return result.svm an svm model fit for the input data
#' @export
#'
sits_train_svm <- function(data_tb = NULL, kernel = "linear",
                           degree = 3, coef0 = 0, cost = 100, tolerance = 0.001, epsilon = 0.1){

    # is the input data the result of a TWDTW matching function?
    ensurer::ensure_that(data_tb, "matches" %in% names (.), err_desc = "sits_train_svm: input data does not contain TWDTW matches")

    # Spread TWDTW matches
    spread_tb <- .sits_spread_matches(data_tb)

    categories <- names(spread_tb)[-2:0]
    lognomes <- paste0('log(', categories, ')')
    formula1 <- stats::as.formula(paste("factor(reference) ~ ", paste(lognomes, collapse = " + ")))

    result.svm <- e1071::svm(formula1, data = spread_tb,
                             type = "C-classification", kernel = kernel,
                             degree = degree, epsilon = epsilon, cost = cost)
}

#' @title Train SITS classifiction models
#' @name sits_train
#'
#' @author Alexandre Xavier Ywata de Carvalho, \email{alexandre.ywata@@ipea.gov.br}
#' @author Victor Maus, \email{vwmaus1@@gmail.com}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Given a SITS tibble time series with DTW matches produced by TWDTW,
#' returns trained models using support vector machines. This function will
#' use the TWDTW alignment information for all classes as the attributes
#' of the SVM. Please use this function in the following way:
#' 1. call sits_patterns to produce a set a labelled patterns from a reference data set
#' 2. call sits_TWDTW_matches to produce a set of alignements
#' 3. use the alignment response as an input to the training function
#'
#' This function is a front-end to the "svm" method in the "e1071" package.
#' In the context of time series classification, the only relevant types are "C-classification" and "nu-classification".
#' Please refer to the documentation in that package for more details.
#'
#' @param data_tb          a SITS tibble time series with an alignment column
#' @param training_method  a function that returns some fitting model
#' @return result          a model fitted into input data given by train_method parameter
#' @export
#'
sits_train <- function(data_tb, training_method = sits_svm(formula = sits_formula_logref(predictors_index = -2:0), kernel = "linear",
                                                           degree = 3, coef0 = 0, tolerance = 0.001, epsilon = 0.1)){

    # is the input data the result of a TWDTW matching function?
    ensurer::ensure_that(data_tb, "matches" %in% names (.), err_desc = "sits_train: input data does not contain TWDTW matches")

    # is the train method a function?
    ensurer::ensure_that(training_method, class(.) == "function", err_desc = "sits_train: train_method is not a valid function")

    # Spread TWDTW matches
    spread_tb <- .sits_spread_matches(data_tb)

    # compute the training method by the given data
    result <- training_method(spread_tb)
    return(result)
}

#' @title Train SITS classifiction models
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
#' @param data_tb   a SITS tibble time series with an alignment column. If data_tb is NULL, the function returns
#' a function to be called further to compute svm training model according to given parameters
#' @param formula   a symbolic description of the model to be fit. SITS offers a set of such formulas (default: sits_formula_logref)
#' @param kernel    the kernel used in training and predicting (options = linear, polynomial, radial basis, sigmoid)
#' @param degree    exponential of polynomial type kernel
#' @param coef0	    parameter needed for kernels of type polynomial and sigmoid (default: 0)
#' @param tolerance	tolerance of termination criterion (default: 0.001)
#' @param epsilon	epsilon in the insensitive-loss function (default: 0.1)
#' @param ...       other parameters to be passed to e1071::svm function
#' @return result   either an e1071::svm class or an function prepared that can be called further to compute svm training model
#' @export
#'
sits_svm <- function(data_tb = NULL, formula = sits_formula_logref(), kernel = "linear",
                     degree = 3, coef0 = 0, tolerance = 0.001, epsilon = 0.1, ...) {

    # function that returns e1071::svm model based on a sits sample tibble
    result_fun <- function(tb){

        # if parameter formula is a function call it passing as argument the input data sample. The function must return a valid formula.
        if (class(formula) == "function")
            formula <- formula(tb)

        # call e1071::svm method and return the trained svm model
        result_svm <- e1071::svm(formula = formula, data = tb, kernel = kernel,
                                 degree = degree, coef0 = coef0, tolerance = tolerance, epsilon = epsilon, ...)
        return(result_svm)
    }

    # if no data is given, we prepare a function to compute svm as model
    if (is.null(data_tb))
        result <- result_fun
    # ...otherwise compute svm model and return it
    else
        result <- result_fun(data_tb)
    return(result)
}

#' @title Train SITS classifiction models
#' @name sits_lda
#'
#' @author Alexandre Xavier Ywata de Carvalho, \email{alexandre.ywata@@ipea.gov.br}
#' @author Victor Maus, \email{vwmaus1@@gmail.com}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Performs a linear discriminant analysis (lda) to classify data.
#' This function is a front-end to the "lda" method in the "MASS" package.
#' Please refer to the documentation in that package for more details.
#'
#' @param data_tb   a SITS tibble time series with an `matches` column. If `data_tb` is NULL, the function returns
#' a pre-parameterized function to be called further in order to compute `lda` training model
#' @param formula   a symbolic description of the model to be fit. SITS offers a set of such formulas (default: sits_formula_logref)
#' @param ...       other parameters to be passed to MASS::lda function
#' @return result   either an MASS::lda class or an function prepared that can be called further to compute lda training model
#' @export
#'
sits_lda <- function(data_tb = NULL, formula = sits_formula_logref(), ...) {

    # function that returns MASS::lda model based on a sits sample tibble
    result_fun <- function(tb){

        # if parameter formula is a function call it passing as argument the input data sample. The function must return a valid formula.
        if (class(formula) == "function")
            formula <- formula(tb)

        # call MASS::lda method and return the trained lda model
        result_lda <- MASS::lda(formula = formula, data = tb, ...)
        return(result_lda)
    }

    # if no data is given, we prepare a function to compute lda as model
    if (is.null(data_tb))
        result <- result_fun
    # ...otherwise compute lda model and return it
    else
        result <- result_fun(data_tb)
    return(result)
}

#' @title Train SITS classifiction models
#' @name sits_mlr
#'
#' @author Alexandre Xavier Ywata de Carvalho, \email{alexandre.ywata@@ipea.gov.br}
#' @author Victor Maus, \email{vwmaus1@@gmail.com}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Use multinomial log-linear (mlr) fitting model via neural networks to classify data.
#' This function is a front-end to the "multinom" method in the "nnet" package.
#' Please refer to the documentation in that package for more details.
#'
#' @param data_tb   a SITS tibble time series with an `matches` column. If `data_tb` is NULL, the function returns
#' a pre-parameterized function to be called further in order to compute `multinom` training model
#' @param formula   a symbolic description of the model to be fit. SITS offers a set of such formulas (default: sits_formula_logref)
#' @param ...       other parameters to be passed to nnet::multinom function
#' @return result   either an nnet::multinom class or an function prepared that can be called further to compute multinom training model
#' @export
#'
sits_mlr <- function(data_tb = NULL, formula = sits_formula_logref(), ...) {

    # function that returns nnet::multinom model based on a sits sample tibble
    result_fun <- function(tb){

        # if parameter formula is a function call it passing as argument the input data sample. The function must return a valid formula.
        if (class(formula) == "function")
            formula <- formula(tb)

        # call nnet::multinom method and return the trained multinom model
        result_mlr <- nnet::multinom(formula = formula, data = tb, ...)
        return(result_mlr)
    }

    # if no data is given, we prepare a function to compute multinom as model
    if (is.null(data_tb))
        result <- result_fun
    # ...otherwise compute multinom model and return it
    else
        result <- result_fun(data_tb)
    return(result)
}

#' @title Train SITS classifiction models
#' @name sits_glm
#'
#' @author Alexandre Xavier Ywata de Carvalho, \email{alexandre.ywata@@ipea.gov.br}
#' @author Victor Maus, \email{vwmaus1@@gmail.com}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Use generalized liner model (glm) via penalized maximim likelihood to classify data.
#' This function is a front-end to the "glmnet" method in the "glmnet" package.
#' Please refer to the documentation in that package for more details.
#'
#' @param data_tb   a SITS tibble time series with an `matches` column. If `data_tb` is NULL, the function returns
#' a pre-parameterized function to be called further in order to compute `multinom` training model
#' @param formula   a symbolic description of the model to be fit. SITS offers a set of such formulas (default: sits_formula_logref)
#' @param ...       other parameters to be passed to nnet::multinom function
#' @return result   either an nnet::multinom class or an function prepared that can be called further to compute multinom training model
#' @export
#'
sits_glm <- function(data_tb = NULL, formula = sits_formula_logref(), ...) {

    # function that returns nnet::multinom model based on a sits sample tibble
    result_fun <- function(tb){

        # if parameter formula is a function call it passing as argument the input data sample. The function must return a valid formula.
        if (class(formula) == "function")
            formula <- formula(tb)

        # call nnet::multinom method and return the trained multinom model
        result_mlr <- nnet::multinom(formula = formula, data = tb, ...)
        return(result_mlr)
    }

    # if no data is given, we prepare a function to compute multinom as model
    if (is.null(data_tb))
        result <- result_fun
    # ...otherwise compute multinom model and return it
    else
        result <- result_fun(data_tb)
    return(result)
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
sits_formula_logref <- function(predictors_index = NULL){

    # this function returns a formula like 'factor(reference~log(f1)+log(f2)+...+log(fn)' where f1, f2, ..., fn are,
    # respectivelly, the reference and predictors fields of tibble in `tb` parameter.
    result_fun <- function(tb){

        # if no predictors_index are given, assume that all tb's fields are used
        if (is.null(predictors_index))
            predictors_index <- 1:NROW(tb)

        # get predictors names
        categories <- names(tb)[c(predictors_index)]

        # compute formula result
        result_for <- stats::as.formula(paste0("factor(reference)~", paste0(paste0('log(', categories, ')'), collapse = "+")))
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
#' @description Given a SITS tibble time series and a model trained by \code{\link[sits]{sits_train_svm}},
#' returns a SITS tibble with the classification.
#'
#' @param data_tb a SITS tibble time series
#' @param model a model trained by \code{\link[sits]{sits_train_svm}}
#' @param ... other parameters to pass to \code{\link[sits]{sits_patterns}} and
#' \code{\link[sits]{sits_TWDTW_matches}}
#'
#' @export
sits_predict <- function(data_tb = NULL, model){

    # is the input data the result of a TWDTW matching function?
    ensurer::ensure_that(data_tb, "matches" %in% names (.), err_desc = "sits_train_svm: input data does not contain TWDTW matches")

    # Spread TWDTW matches
    spread_tb <- .sits_spread_matches(data_tb)

    data_tb$predicted <- as.character(stats::predict(model, newdata = spread_tb))

    return(data_tb)
}
