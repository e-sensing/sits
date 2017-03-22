
#' @title validate temporal patterns
#' @name sits_validate
#' @author Victor Maus, \email{vwmaus1@@gmail.com}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Splits the set of time series into training and validation and
#' compute accuracy metrics. The function uses stratified sampling and a simple
#' random sampling for each stratum. For each data partition this function
#' performs a TWDTW analysis and returns the Overall Accuracy, User's Accuracy,
#' Produce's Accuracy, error matrix (confusion matrix), and a \code{\link[base]{data.frame}}
#' with the classification (Predicted), the reference classes (Reference),
#' and the results of the TWDTW analysis.
#'
#' @param data.tb a SITS tibble
#' @param bands   the bands used for classification
#' @param method  method to create patterns ("gam", "dendogram" or "centroids")
#' @param times Number of partitions to create.
#' @param p the percentage of data that goes to training.
#' @param reference.lst a conversion of label names for the reference classes (optional)
#' @param predicted.lst a conversion of label names for the predicted classes (optional)
#' @export

sits_validate <- function (data.tb, bands = NULL, method = "gam", times = 100, p = 0.1,
                           conversion.lst = NULL, ...){

     ensurer::ensure_that(data.tb, !purrr::is_null(.),
                          err_desc = "sits_validate: input data not provided")
     ensurer::ensure_that(bands, !purrr::is_null (.),
                          err_desc = "sits_validate: Missing bands vector")
     ensurer::ensure_that(bands, !purrr::is_null (.),
                          err_desc = "sits_validate: Missing bands vector")
     # are the bands to be classified part of the input data ?
     ensurer::ensure_that(data.tb, !(FALSE %in% bands %in% (sits_bands(.))),
                          err_desc = "sits_validate: invalid input bands")

     # what are the labels of the samples?
     labels <- dplyr::distinct (data.tb, label)
     # if the conversion list is NULL, create an identity list
     if (purrr::is_null(conversion.lst)) {


     }

     # create partitions different splits of the input data
     partitions.lst <- .create_partitions (data.tb, times, perc)

     partitions.lst %>%
          map (function (p){
               patterns.tb <- sits_patterns(p, method)
               non_p.tb <- dplyr::anti_join(data.tb, p,
                                by = c("longitude", "latitude", "start_date",
                                       "end_date", "label", "coverage"))
               results.tb  <- sits_TWDTW (non_p.tb, patterns.tb, bands)

               i <- which.min(results.tb$alignments[[1]]$distance)


          })

     validation.lst <- dtwSat::twdtwCrossValidate(twdtw.ts, times, p, formula = y ~ s(x))

     error.matrix = table(Predicted=data$Predicted, Reference=data$Reference)
     UA = diag(error.matrix) / rowSums(error.matrix)
     PA = diag(error.matrix) / colSums(error.matrix)
     O  = sum(diag(error.matrix)) / sum(rowSums(error.matrix))
     list(OverallAccuracy=O, UsersAccuracy=UA, ProducersAccuracy=PA, ErrorMatrix=error.matrix, data=data)
}
