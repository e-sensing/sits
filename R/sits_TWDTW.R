#' @title Classify a sits tibble using TWDTW (using the dtwSat package)
#' @name sits_TWDTW
#' @author Victor Maus, \email{vwmaus1@@gmail.com}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Returns a sits table with the results of the TWDTW classifier.
#' The TWDTW classifier compares the values of a satellite image time series with
#' the values of known patters and tries to match each pattern to a part of the time series
#'
#' The TWDTW (time-weighted dynamical time warping) is a version of the
#' Dynamic Time Warping method for land use and land cover mapping using a sequence
#' of multi-band satellite images. Methods based on dynamic time warping are flexible to
#' handle irregular sampling and out-of-phase time series, and they have achieved significant
#' results in time series analysis. In contrast to standard DTW, the TWDTW method is sensitive to seasonal
#' changes of natural and cultivated vegetation types. It also considers inter-annual climatic and
#' seasonal variability.
#'
#' @references Maus V, Camara G, Cartaxo R, Sanchez A, Ramos FM, de Queiroz GR (2016).
#' A Time-Weighted Dynamic Time Warping Method for Land-Use and Land-Cover Mapping. IEEE
#'  Journal of Selected Topics in Applied Earth Observations and Remote Sensing, 9(8):3729-3739,
#'  August 2016. ISSN 1939-1404. doi:10.1109/JSTARS.2016.2517118.
#'
#' @param  series.tb     a table in SITS format with a time series to be classified using TWTDW
#' @param  patterns.tb   a set of known temporal signatures for the chosen classes
#' @param  bands         string - the bands to be used for classification
#' @param  alpha         (double) - the steepness of the logistic function used for temporal weighting
#' @param  beta          (integer) - the midpoint (in days) of the logistic function
#' @param  theta         (double)  - the relative weight of the time distance compared to the dtw distance
#' @param  interval      the period between two classifications
#' @param  span          the minimum period for a match between a pattern and a signal)
#' @param  keep          keep internal values for plotting matches
#' @return matches       a SITS table with the information on matches for the data
#' @export
sits_TWDTW <- function (series.tb, patterns.tb, bands,
                        alpha = -0.1, beta = 100, theta = 0.5,
                        interval = "12 month", span  = 250, keep  = FALSE){

     # create a tibble to store the results
     results.tb <- sits_table()

     # select the bands for patterns time series and convert to TWDTW format
     twdtw_patterns <- patterns.tb %>%
          sits_select (bands) %>%
          .sits_toTWDTW_time_series()

     # Define the logistic function
     log_fun <- dtwSat::logisticWeight(alpha = alpha, beta = beta)

     series.tb %>%
          purrr::by_row (function (row) {
               # select the bands for the samples time series and convert to TWDTW format
               twdtw_series <- row %>%
                    sits_select (bands) %>%
                    .sits_toTWDTW_time_series()

               # set the start and end dates
               start_date <- lubridate::as_date(utils::head(row$time_series[[1]],1)$Index)
               end_date   <- lubridate::as_date(utils::tail(row$time_series[[1]],1)$Index)

               # define the temporal intervals of each classification
               breaks <- seq(from = start_date, to = end_date, by = interval)

               #classify the data using TWDTW
               matches = dtwSat::twdtwApply(x          = twdtw_series,
                                            y          = twdtw_patterns,
                                            weight.fun = log_fun,
                                            theta      = theta,
                                            span       = span,
                                            keep       = keep)
               # store the alignments and matches in two lists
               # alignments
               align.lst <- tibble::lst()
               align.lst[[1]] <- matches[][[1]]

               # matches
               match.lst <- tibble::lst()
               match.lst[[1]] <- matches

               # add the aligments and matches to the results
               res.tb <- dplyr::mutate (row, alignments  = align.lst)
               res.tb <- dplyr::mutate (res.tb, matches    = match.lst)

               # add the row to the results.tb tibble
               results.tb <<- dplyr::bind_rows(results.tb, res.tb)
          })
     return (results.tb)
}
#' @title Export data to be used by the dtwSat package
#' @name .sits_toTWDTW_time_series
#' @author Victor Maus, \email{vwmaus1@@gmail.com}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Converts data from a SITS table to an instance of a TWDTW time series class,
#' Returns a twdtwTimeSeries object (S4)
#'
#'
#' @param  data.tb       a table in SITS format with time series to be converted to TWTDW time series
#' @return ts.tw         a time series in TWDTW format (an object of the twdtwTimeSeries class)
.sits_toTWDTW_time_series <- function (data.tb){
     zoo.ls <- data.tb$time_series %>%
          purrr::map (function (ts) {
               df <- data.frame (ts)
               return (zoo::zoo (df[,2:ncol(df),drop=FALSE], df[,1]))
          })
     labels.fc <-  as.factor (data.frame (dplyr::select (data.tb, label))[,1])

     ts.tw <-  methods::new("twdtwTimeSeries", timeseries = zoo.ls,
               labels = labels.fc)
     return (ts.tw)
}

#' @title Transform patterns from TWDTW format to SITS format
#' @name .sits_fromTWDTW_time_series
#'
#' @description reads a set of TWDTW patterns and transforms them into a SITS table
#'
#' @param patterns  - a TWDTW object containing a set of patterns to be used for classification
#' @param coverage  - the name of the coverage from where the time series have been obtained
#' @return sits.tb  - a SITS table containing the patterns
.sits_fromTWDTW_time_series <- function (patterns, coverage){
     # get the time series from the patterns
     tb.lst <- purrr::map2 (patterns@timeseries, patterns@labels, function (ts, lab) {
          # tranform the time series into a row of a sits table
          ts.tb <- zoo::fortify.zoo(ts)
          # store the sits table in a list
          mylist        <- list()
          mylist [[1]]  <- tibble::as_tibble (ts.tb)
          # add the row to the sits table
          row   <- tibble::tibble(longitude    = 0.00,
                          latitude     = 0.00,
                          start_date   = ts.tb[1,"Index"],
                          end_date     = ts.tb[nrow(ts.tb),"Index"],
                          label        = as.character (lab),
                          coverage     = coverage,
                          time_series  = mylist)
          return (row)
     })
     # create a sits table to store the result
     patterns.tb <- sits_table()
     patterns.tb <- tb.lst %>%
          purrr::map_df (function (row) {
               dplyr::bind_rows (patterns.tb, row)
          })
     return (patterns.tb)
}
