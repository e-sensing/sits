#' @title Assess time series classification
#' @name sits_assess
#' @author Victor Maus, \email{vwmaus1@@gmail.com}
#' @author Gilberto Camara, \email{gilberto.camara@inpe.br}
#'
#' @description Performs an accuracy assessment
#' of the classified maps. The function returns Overall Accuracy,
#' User's Accuracy, Produce's Accuracy, error matrix (confusion matrix),
#' and estimated area according to [1-2]. The function returns the metrics
#' for each time interval and a summary considering all classified intervals.
#'
#' @param results.tb an object of "sits_table" with the results of a time series classification
#'
#' @param area a numeric vector with the area for each class
#'
#' @param conf.int specifies the confidence level (0-1).
#'
#' @param rm.nosample if sum of columns and sum of rows of the error matrix are zero
#' then remove class. Default is TRUE.
#'
#' @export
#' @references
#' [1] Olofsson, P., Foody, G.M., Stehman, S.V., Woodcock, C.E. (2013).
#' Making better use of accuracy data in land change studies: Estimating
#' accuracy and area and quantifying uncertainty using stratified estimation.
#' Remote Sensing of Environment, 129, pp.122-131.
#'
#' @references
#' [2] Olofsson, P., Foody G.M., Herold M., Stehman, S.V., Woodcock, C.E., Wulder, M.A. (2014)
#' Good practices for estimating area and assessing accuracy of land change. Remote Sensing of
#' Environment, 148, pp. 42-57.
#'
sits_assess <-  function (results.tb, area, conf.int = 0.95, rm.nosample = TRUE){
     #get the names of the labels
     labels <- results.tb$distances[[1]] %>%
          dplyr::select(-dplyr::ends_with("year"), -dplyr::ends_with("date"), -dplyr::starts_with("classif")) %>%
          colnames()
     confusion.mx <- matrix (0, nrow = length(labels), ncol = length(labels), dimnames = list(labels, labels))

     for (i in 1:nrow(results.tb)) {
          curr <-results.tb[i,]
          lab <- curr$label
          if (!(lab %in% labels)) next()
          class <- as.character(curr$distances[[1]][1,"classification"])
          print (class)
          confusion.mx [class, lab] <- confusion.mx [class, lab] + 1
     }
     return (confusion.mx)
}
