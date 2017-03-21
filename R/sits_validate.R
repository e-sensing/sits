
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
#' @param times Number of partitions to create.
#' @param p the percentage of data that goes to training.
#' @export

sits_validate <- function (data.tb, method = "gam", times = 100, p = 0.1,
                           reference.lst = NULL, predicted.lst = NULL, ...){

     # create partitions different splits of the input data
     partitions.lst <- .create_partitions (data.tb, times, perc)

     partitions.lst %>%
          map (function (p){
               patterns.tb <- sits_patterns(p, method)
               results.tb  <- sits_TWDTW ()

          })


     twdtw.ts <- .sits_toTWDTW_time_series (data.tb, times)

     validation.lst <- dtwSat::twdtwCrossValidate(twdtw.ts, times, p, formula = y ~ s(x))

     error.matrix = table(Predicted=data$Predicted, Reference=data$Reference)
     UA = diag(error.matrix) / rowSums(error.matrix)
     PA = diag(error.matrix) / colSums(error.matrix)
     O  = sum(diag(error.matrix)) / sum(rowSums(error.matrix))
     list(OverallAccuracy=O, UsersAccuracy=UA, ProducersAccuracy=PA, ErrorMatrix=error.matrix, data=data)
}

.create_partitions <- function (data.tb, times, perc) {

     partitions.lst <- tibble::lst()

     for (i in 1:times){
          frac.tb <- sits_label_perc (data.tb, perc)
          partitions.lst [[i]] <- frac.tb
     }
     return (partitions.lst)
}

# twdtwCrossValidate.twdtwTimeSeries = function(object, times, p, ...){
#
#      partitions = createDataPartition(y = labels(object), times, p, list = TRUE)
#
#      res = lapply(partitions, function(I){
#           training_ts = subset(object, I)
#           validation_ts = subset(object, -I)
#           patt = createPatterns(training_ts, ...)
#           twdtw_res = twdtwApply(x = validation_ts, y = patt, n=1, ...)
#           df = do.call("rbind", lapply(twdtw_res[], function(xx) {
#                i = which.min(xx$distance)
#                if(length(i)<1)
#                     return(data.frame(Alig.N=NA, from=NA, to=NA, distance=NA, label = "Unclassified"))
#                xx[i,]
#           }))
#           ref = labels(twdtw_res)$timeseries
#           pred = df$label
#           data = data.frame(.adjustFactores(ref, pred, levels=NULL, labels=NULL), df[,!names(df)%in%"labels"])
#           error.matrix = table(Predicted=data$Predicted, Reference=data$Reference)
#           UA = diag(error.matrix) / rowSums(error.matrix)
#           PA = diag(error.matrix) / colSums(error.matrix)
#           O  = sum(diag(error.matrix)) / sum(rowSums(error.matrix))
#           list(OverallAccuracy=O, UsersAccuracy=UA, ProducersAccuracy=PA, ErrorMatrix=error.matrix, data=data)
#      })
