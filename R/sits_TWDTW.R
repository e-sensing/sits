#' Classify a sits tibble using TWDTW (using the dtwSat package)
#'
#' \code{sits_classify} returns a sits table with values only
#'
#' A sits table has the metadata and data for each time series
#' <longitude, latitude, start_date, end_date, label, coverage, time_series>
#'
#' The TWDTW (time-weighted dynamical time warping) is a version of the
#' Dynamic Time Warping method for land use and land cover mapping using a sequence
#' of multi-band satellite images. Methods based on dynamic time warping are flexible to
#' handle irregular sampling and out-of-phase time series, and they have achieved significant
#' results in time series analysis.
#' In contrast to standard DTW, the TWDTW method is sensitive to seasonal
#' changes of natural and cultivated vegetation types. It also considers inter-annual climatic and
#' seasonal variability.
#'
#' Reference: Maus V, Camara G, Cartaxo R, Sanchez A, Ramos FM, de Queiroz GR (2016).
#' A Time-Weighted Dynamic Time Warping Method for Land-Use and Land-Cover Mapping. IEEE
#'  Journal of Selected Topics in Applied Earth Observations and Remote Sensing, 9(8):3729-3739,
#'  August 2016. ISSN 1939-1404. doi:10.1109/JSTARS.2016.2517118.
#'
#' @param  samples.tb    a table in SITS format with time series to be classified using TWTDW
#' @param  patterns.tb   a set of known temporal signatures for the chosen classes
#' @param  bands         string - the bands to be used for classification
#' @param  alpha         (double) - the steepness of the logistic function used for temporal weighting
#' @param  beta          (integer) - the midpoint (in days) of the logistic function
#' @param  theta         (double)  - the relative weight of the time distance compared to the dtw distance
#' @param  start_date    date - the starting date of the classification
#' @param  end_date      date - the end date of the classification
#' @param  interval      the period between two classifications
#' @return matches       a SITS table with the information on matches for the data
#' @export
#'
#'
sits_classify <- function (point.tb, patterns.tb, bands,
                           alpha = -0.1, beta = 100, theta = 0.5,
                           start_date = as.Date("2000-09-01"),
                           end_date   = as.Date("2016-08-31"),
                           interval   = "12 month") {
     # select the bands for the samples time series and convert to TWDTW format
     ts_samples <- point.tb %>%
          sits_select (bands) %>%
          sits_toTWDTW()

     # select the bands for patterns time series and convert to TWDTW format
     ts_patterns <- patterns.tb %>%
          sits_select (bands) %>%
          sits_toTWDTW()

     # Define the logistic function
     log_fun = dtwSat::logisticWeight(alpha = alpha, beta = beta)

     # define the temporal intervals of each classification
     breaks = seq(from = start_date, to = end_date, by = interval)

     #classify the data using TWDTW
     matches = dtwSat::twdtwApply(x          = ts_samples,
                                  y          = ts_patterns,
                                  weight.fun = log_fun,
                                  theta      = theta,
                                  breaks     = breaks,
                                  keep       = TRUE)

     # # plot the classification
     # plot(x = matches, type = "classification", overlap = 0.5)
     # # plot the alignments
     # plot(x = matches, type = "alignments")

     results.tb <- .sits_from_matches (point.tb, matches, breaks, interval)

     return (matches)
}


#' Convert information on matches obtained from TWDTW to a SITS table
#'
#' \code{.sits_from_matches} returns a sits table with additional information on matches
#'
#' A sits table has the metadata and data for each time series
#' <longitude, latitude, start_date, end_date, label, coverage, time_series>
#' @param   matches       an object of class "twdtwMatches"
#' @return  samples.tb    a table in SITS format with time series to be classified using TWTDW
#'
#'
#.sits_table_from_matches
# ..@ alignments:List of 1
# .. ..$ :List of 8
# .. .. ..$ Cerrado       :List of 7
# .. .. .. ..$ label    : Named chr "Cerrado"
# .. .. .. .. ..- attr(*, "names")= chr "Cerrado"
# .. .. .. ..$ from     : Date[1:16], format: "2001-11-17" "2011-09-30" ...
# .. .. .. ..$ to       : Date[1:16], format: "2002-05-25" "2012-06-09" ...
# .. .. .. ..$ distance : num [1:16] 2.81 3.35 3.51 3.56 3.72 ...
.sits_from_matches <- function (point.tb, matches, breaks, interval){

     # convert labels to a vector of strings
     labels <- as.character(matches@patterns@labels, stringsAsFactors = FALSE)
     aligns <- matches@alignments[[1]]

     distances.tb <- tibble::tibble (start_date = breaks,
                                     end_date   = breaks + lubridate::period(interval) - lubridate::days(1))

     distances.lst <- labels %>%
          purrr::map (function (lab){
               dist <- tibble::tibble(from = aligns[[lab]]$from, to = aligns[[lab]]$to,
                                      val = aligns[[lab]]$distance)
               colnames (dist) <- c("start_date", "end_date", lab)
               dist <- dplyr::arrange(dist, start_date)
          })
     distances.lst <- distances.lst %>%
          purrr::map (function (tb) dplyr::filter (tb, lubridate::days(end_date - start_date) > lubridate::days(250)))

     }

     #               dist <- dplyr::mutate (dist, start_date = breaks)

     distances.tb <- distances.lst %>%
          purrr::map (function (tb){
               tb <- dplyr::select (tb, -dplyr::contains ("end_date"))
          })
     print (distances.lst)
     #

}


#' Export data to be used by the dtwSat package
#'
#' \code{sits_toTWDTW} returns a twdtwTimeSeries object (S4)
#'
#' Converts data from a SITS table to an instance of a TWDTW time series class
#'
#' Reference: Maus V, Camara G, Cartaxo R, Sanchez A, Ramos FM, de Queiroz GR (2016).
#' A Time-Weighted Dynamic Time Warping Method for Land-Use and Land-Cover Mapping. IEEE
#'  Journal of Selected Topics in Applied Earth Observations and Remote Sensing, 9(8):3729-3739,
#'  August 2016. ISSN 1939-1404. doi:10.1109/JSTARS.2016.2517118.
#'
#' @param  data.tb       a table in SITS format with time series to be converted to TWTDW time series
#' @return ts.tw         a time series in TWDTW format (an object of the twdtwTimeSeries class)
#' @export
#'
#'
sits_toTWDTW <- function (data.tb){
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

#'
#' Transform patterns from TWDTW format to SITS format
#' \code{sits_fromTWDTW} reads a set of TWDTW patterns
#' transforms them into a SITS table
#'
#' @param patterns  - a TWDTW object containing a set of patterns to be used for classification
#' @param coverage  - the name of the coverage from where the time series have been obtained
#' @return sits.tb  - a SITS table containing the patterns
#' @export
#'
sits_fromTWDTW <- function (patterns, coverage){
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
