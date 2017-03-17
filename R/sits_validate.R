
#' @title validate temporal patterns
#' @name sits_validate
#' @author Victor Maus, \email{vwmaus1@@gmail.com}
#'
#' @description This function callis \code{\link[dtwSat]{dtwSat::twdtwCrossValidate}}, 
#' which splits the set of time series into training and validation and compute accuracy metrics. 
#' The function uses stratified sampling and a simple
#' random sampling for each stratum. For each data partition this function
#' performs a TWDTW analysis and returns the Overall Accuracy, User's Accuracy,
#' Produce's Accuracy, error matrix (confusion matrix), and a \code{\link[base]{data.frame}}
#' with the classification (Predicted), the reference classes (Reference),
#' and the results of the TWDTW analysis.
#'
#' @param data.tb a SITS tibble
#'
#' @param times Number of partitions to create.
#'
#' @param p the percentage of data that goes to training.
#' @export
sits_validate <- function (data.tb, times = 100, p = 0.1, formula = y ~ s(x), ...){

     twdtw.ts <- .sits_toTWDTW_time_series (data.tb)

     validation.lst <- dtwSat::twdtwCrossValidate(twdtw.ts, times, p, formula = formula, ...)
     
     return(validation.lst)

}
