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
#' @param data.tb     a SITS tibble time series with an alignment column
#' @param kernel      the kernel used in training and predicting (options = linear, polynomial, radial basis, sigmoid)
#' @param degree      exponential of polynomial type kernel
#' @param coef0	      parameter needed for kernels of type polynomial and sigmoid (default: 0)
#' @param cost	      cost of constraints violation (default: 1): it is the Cconstant of the regularization term in the Lagrange formulation.
#' @param tolerance	  tolerance of termination criterion (default: 0.001)
#' @param epsilon	  epsilon in the insensitive-loss function (default: 0.1)
#' @return result.svm an svm model fit for the input data
#' @export
#'
sits_train_svm <- function(data.tb = NULL, kernel = "linear",
                           degree = 3, coef0 = 0, cost = 100, tolerance = 0.001, epsilon = 0.1){

    # is the input data the result of a TWDTW matching function?
    ensurer::ensure_that(data.tb, "matches" %in% names (.), err_desc = "sits_train_svm: input data does not contain TWDTW matches")

    # Spread TWDTW matches
    spread.tb <- .sits_spread_matches(data.tb)

    categories <- names(spread.tb)[-2:0]
    lognomes <- paste0('log(', categories, ')')
    formula1 <- stats::as.formula(paste("factor(reference) ~ ", paste(lognomes, collapse = " + ")))

    result.svm <- e1071::svm(formula1, data = spread.tb,
                             type = "C-classification", kernel = kernel,
                             degree = degree, epsilon = epsilon, cost = cost)
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
#' @param data.tb a SITS tibble time series
#' @param model a model trained by \code{\link[sits]{sits_train_svm}}
#' @param ... other parameters to pass to \code{\link[sits]{sits_patterns}} and
#' \code{\link[sits]{sits_TWDTW_matches}}
#'
#' @export
sits_predict <- function(data.tb = NULL, model){

    # is the input data the result of a TWDTW matching function?
    ensurer::ensure_that(data.tb, "matches" %in% names (.), err_desc = "sits_train_svm: input data does not contain TWDTW matches")

    # Spread TWDTW matches
    spread.tb <- .sits_spread_matches(data.tb)

    data.tb$predicted <- as.character(stats::predict(model, newdata = spread.tb))

    return(data.tb)
}
