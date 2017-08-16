#' @title Find matches between a set of SITS patterns and segments of sits tibble using TWDTW
#' @name sits_TWDTW_matches
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
#' @param  data.tb     a table in SITS format with a time series to be classified using TWTDW
#' @param  patterns.tb   a set of known temporal signatures for the chosen classes
#' @param  bands         string - the bands to be used for classification
#' @param  dist.method   A character. Method to derive the local cost matrix.
#' @param  alpha         (double) - the steepness of the logistic function used for temporal weighting
#' @param  beta          (integer) - the midpoint (in days) of the logistic function
#' @param  theta         (double)  - the relative weight of the time distance compared to the dtw distance
#' @param  span          minimum number of days between two matches of the same pattern in the time series (approximate)
#' @param  keep          keep internal values for plotting matches
#' @return matches       a SITS table with the information on matches for the data
#' @export
sits_TWDTW_matches <- function (data.tb = NULL, patterns.tb, bands, dist.method = "euclidean",
                        alpha = -0.1, beta = 100, theta = 0.5, span  = 250, keep  = FALSE){

     # add a progress bar
     progress_bar <- NULL
     if (nrow (data.tb) > 10) {
          message("Matching patterns to time series...")
          progress_bar <- utils::txtProgressBar(min = 0, max = nrow(data.tb), style = 3)
          i <- 0
     }

     # create a tibble to store the results of the TWDTW matches
     matches.tb <- sits_table()

     # select the bands for patterns time series and convert to TWDTW format
     twdtw_patterns <- patterns.tb %>%
          sits_select (bands) %>%
          .sits_toTWDTW_time_series()

     # Define the logistic function
     log_fun <- dtwSat::logisticWeight(alpha = alpha, beta = beta)

     data.tb %>%
          purrrlyr::by_row (function (row.tb) {
               # select the bands for the samples time series and convert to TWDTW format
               twdtw_series <- row.tb %>%
                    sits_select (bands) %>%
                    .sits_toTWDTW_time_series()

               #classify the data using TWDTW
               matches = dtwSat::twdtwApply(x          = twdtw_series,
                                            y          = twdtw_patterns,
                                            weight.fun = log_fun,
                                            theta      = theta,
                                            span       = span,
                                            keep       = keep,
                                            dist.method = dist.method)

               # add the matches to the results
               matches.lst <- .sits_fromTWDTW_matches(matches)

               # include the matches in the SITS table
               res.tb <- row.tb %>%
                   dplyr::mutate(matches = matches.lst)

               # add the row to the results.tb tibble
               matches.tb <<- dplyr::bind_rows(matches.tb, res.tb)

               # update progress bar
               if (!purrr::is_null(progress_bar)) {
                    i <<- i + 1
                    utils::setTxtProgressBar(progress_bar, i)
               }
          })
     if (!purrr::is_null(progress_bar)) close(progress_bar)
     return (matches.tb)
}

#' @title Classify a sits tibble using the matches found by the TWDTW methods
#' @name sits_TWDTW_classify
#' @author Victor Maus, \email{vwmaus1@@gmail.com}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Returns a sits table with the results of the TWDTW classifier.
#' The TWDTW classifier uses the result of the sits_TWDTW_matches function to
#' find the best alignments of these matches in intervals chosen by the user
#' @references Maus V, Camara G, Cartaxo R, Sanchez A, Ramos FM, de Queiroz GR (2016).
#' A Time-Weighted Dynamic Time Warping Method for Land-Use and Land-Cover Mapping. IEEE
#'  Journal of Selected Topics in Applied Earth Observations and Remote Sensing, 9(8):3729-3739,
#'  August 2016. ISSN 1939-1404. doi:10.1109/JSTARS.2016.2517118.
#'
#' @param  data.tb       a table in SITS format with the matches that have been produced by TWTDW
#' @param  patterns.tb   patterns SITS tibble used to matching
#' @param  start_date    date - the start of the classification period
#' @param  end_date      date - the end of the classification period
#' @param  interval      the period between two classifications
#' @param  overlap       minimum overlapping between one match and the interval of classification
#' @return matches       a SITS table with the information on matches for the data
#'
#' @export
#'
sits_TWDTW_classify <- function (data.tb, patterns.tb, start_date = NULL, end_date = NULL,
                        interval = "12 month", overlap = 0.5){

     ensurer::ensure_that(data.tb, "matches" %in% names(.), err_desc = "sits_TWDTW_classify: input tibble should have a matches collumn  \n Please run sits_TWDTW_matches first")

     # create a tibble to store the results
     # class.tb <- sits_table()

    class.tb <- data.tb %>%
          purrrlyr::by_row (function (row) {

               if (purrr::is_null (start_date)) {
                    start_date  <- row$start_date
                    end_date    <- row$end_date
                    interval <- lubridate::as_date(end_date) - lubridate::as_date(start_date)
               }

               # define the temporal intervals of each classification
               breaks <- seq(from = as.Date(start_date), to = as.Date(end_date), by = interval)

               match.twdtw <- row %>%
                   .sits_toTWDTW_matches(patterns.tb)

               classify <- dtwSat::twdtwClassify(x = match.twdtw[[1]], breaks = breaks, overlap = overlap)
               class.lst <- .sits_fromTWDTW_matches(classify)

               # add the classification results to the input row
               return(class.lst[[1]])

               # add the row to the results.tb tibble
               # class.tb <<- dplyr::bind_rows(class.tb, res.tb)
          }, .to = "best_matches")
     return (class.tb)
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
#' @return ts.twdtw      a time series in TWDTW format (an object of the twdtwTimeSeries class)
.sits_toTWDTW_time_series <- function (data.tb){
    # transform each sits time series into a list of zoo
    ts <- data.tb$time_series %>%
        purrr::map(function (ts) zoo::zoo(ts[,2:ncol(ts), drop=FALSE], ts$Index))

    # create a new twdtwTimeSeries object from list above
    ts.twdtw <- methods::new("twdtwTimeSeries", timeseries = ts,
                             labels = as.character(data.tb$label))
    return (ts.twdtw)
}

#' @title Transform patterns from TWDTW format to SITS format
#' @name .sits_fromTWDTW_time_series
#'
#' @description reads a set of TWDTW patterns and transforms them into a SITS table
#'
#' @param patterns  - a TWDTW object containing a set of patterns to be used for classification
#' @param coverage  - the name of the coverage from where the time series have been obtained
#'
#' @return sits.tb  - a SITS table containing the patterns
#'
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

#' @title Export data to be used by the dtwSat package
#' @name .sits_toTWDTW_matches
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Converts data from a SITS table to an instance of a TWDTW matches class,
#' Returns a dtwSat::twdtwMatches object (S4)
#'
#' @param  data.tb       a table in SITS format with time series to be converted to TWTDW time series
#' @param  patterns.tb   patterns SITS tibble used to matching
#' @return ts.twdtw      a time series in TWDTW format (an object of the twdtwTimeSeries class)
#'
.sits_toTWDTW_matches <- function(data.tb, patterns.tb){
    # compute patterns dtwSat::twdtwTimeSeries object
    pat.twdtw <- patterns.tb %>%
        .sits_toTWDTW_time_series()

    # traverse data.tb and, for each row, create a list of dtwSat::twdtwMatches objects
    data.tb %>%
        purrrlyr::by_row(function (row.tb){
            # get predicted labels (pattern labels in matches)
            labels <- base::unique(row.tb$matches[[1]]$predicted)

            # traverse predicted labels and, for each entry, generate the alignments' information
            # required by dtwSat::twdtwMatches@alignments
            align.lst <- labels %>%
                purrr::map(function (lbl){
                    entry.lst <- list(label = c(lbl))
                    entry.lst <- c(entry.lst, row.tb$matches[[1]] %>%
                                       dplyr::filter(predicted == lbl) %>%
                                       dplyr::select(-predicted) %>%
                                       purrr::map(function (col) col))
                    entry.lst <- c(entry.lst, list(K = length(entry.lst$from),
                                                   matching = list(), internals = list()))
                    entry.lst
                })

            # names of each entry in list of alignments
            names(align.lst) <- labels

            # generate a dtwSat::twdtwTimeSeries object for the correspondent time series matched by patterns
            ts.twdtw <- row.tb %>%
                .sits_toTWDTW_time_series()

            # with all required information, creates a new dtwSat::twdtwMatches object for this row
            ts.twdtw <- methods::new("twdtwMatches", timeseries = ts.twdtw,
                                     patterns = pat.twdtw, alignments = list(align.lst))
        }, .to = "matches", .labels = FALSE) %>%
        .$matches
}

#' @title Transform patterns from TWDTW format to SITS format
#' @name .sits_fromTWDTW_time_series
#'
#' @description reads one TWDTW matches object and transforms it into a tibble ready to be stored into a SITS table column.
#'
#' @param  match.twdtw  a TWDTW Matches object of class dtwSat::twdtwMatches (S4)
#' @return result.tb    a tibble containing the matches information
#'
.sits_fromTWDTW_matches <- function(match.twdtw){
    result.tb <- tibble::as_tibble(match.twdtw[[1]]) %>%
        dplyr::mutate(predicted = as.character(label)) %>%
        dplyr::select(-Alig.N, -label) %>%
        list()
    return(result.tb)
}
