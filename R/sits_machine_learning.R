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
#' @param data.tb       tibble   a SITS tibble time series with an alignment column
#' @param type          string   whether svm is used for classification or for regression
#' @param kernel        string   the kernel used in training and predicting (options = linear, polynomial, radial basis, sigmoid)
#' @param degree        int      exponential of polynomial type kernel
#' @param coef0	    num      parameter needed for kernels of type polynomial and sigmoid (default: 0)
#' @param cost	         num      cost of constraints violation (default: 1) —it is the ‘C’-constant of the regularization term in the Lagrange formulation.
#' @param tolerance	    num      tolerance of termination criterion (default: 0.001)
#' @param epsilon	    num      epsilon in the insensitive-loss function (default: 0.1)
#' @return model.fit    an svm model fit for the input data
#' @export
#' 
sits_train_svm <- function(data.tb, type = "C-classification", kernel = "linear",
                           degree = 3, coef0 = 0, cost = 100, tolerance = 0.001, epsilon = 0.1){
     
     # is the input data the result of a TWDTW matching function?
     ensurer::ensure_that(data.tb, "matches" %in% names (.), err_desc = "sits_train_svm: input data does not contain TWDTW matches")
     
     # Spread TWDTW matches  
     matches.tb <- .sits_spread_matches(data.tb)
     
     
     
     model.fit <- e1071::svm(formula1, data = data, kernel = kernel, 
                             degree = degree, type = type, 
                             epsilon = epsilon, cost = cost)
}