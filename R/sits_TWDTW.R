#' @title Find matches between a set of SITS patterns and segments of sits tibble using TWDTW
#' @name sits_TWDTW_matches
#' @author Victor Maus, \email{vwmaus1@@gmail.com}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Returns  the results of the TWDTW matching function.
#' The TWDTW matching function compares the values of a satellite image time series with
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
#' @param  data.tb        a table in SITS format with a time series to be classified using TWTDW
#' @param  patterns.tb   a set of known temporal signatures for the chosen classes
#' @param  bands         string - the bands to be used for classification
#' @param  dist.method   A character. Method to derive the local cost matrix.
#' @param  alpha         (double) - the steepness of the logistic function used for temporal weighting
#' @param  beta          (integer) - the midpoint (in days) of the logistic function
#' @param  theta         (double)  - the relative weight of the time distance compared to the dtw distance
#' @param  span          minimum number of days between two matches of the same pattern in the time series (approximate)
#' @param  keep          keep internal values for plotting matches
#' @return matches       a dtwSat S4 object with the matches
#' @export
sits_TWDTW_matches <- function (data.tb = NULL, patterns.tb = NULL, bands = NULL, dist.method = "euclidean",
                        alpha = -0.1, beta = 100, theta = 0.5, span  = 250, keep  = FALSE){

    # add a progress bar
    progress_bar <- NULL
    if (nrow (data.tb) > 10) {
        message("Matching patterns to time series...")
        progress_bar <- utils::txtProgressBar(min = 0, max = nrow(data.tb), style = 3)
        i <- 0
    }
    # does the input data exist?
    .sits_test_tibble (data.tb)
    .sits_test_tibble (patterns.tb)

    # handle the case of null bands
    if (purrr::is_null (bands)) bands <- sits_bands(data.tb)

    # create a list to store the results of the TWDTW matches
    matches.lst <- list()

    # select the bands for patterns time series and convert to TWDTW format
    twdtw_patterns <- patterns.tb %>%
        sits_select_bands (bands = bands) %>%
        sits_toTWDTW()

    # Define the logistic function
    log_fun <- dtwSat::logisticWeight(alpha = alpha, beta = beta)

    data.tb %>%
        purrrlyr::by_row (function (row.tb) {
            # select the bands for the samples time series and convert to TWDTW format
            twdtw_series <- row.tb %>%
                sits_select_bands (bands = bands) %>%
                sits_toTWDTW()

            #classify the data using TWDTW
            matches = dtwSat::twdtwApply(x          = twdtw_series,
                                         y          = twdtw_patterns,
                                         weight.fun = log_fun,
                                         theta      = theta,
                                         span       = span,
                                         keep       = keep,
                                         dist.method = dist.method)

            # add the matches to the lsit
            matches.lst[[length(matches.lst) + 1]] <<- matches


            # update progress bar
            if (!purrr::is_null(progress_bar)) {
                i <<- i + 1
                utils::setTxtProgressBar(progress_bar, i)
            }
        })
    if (!purrr::is_null(progress_bar)) close(progress_bar)
    return (matches.lst)
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
#' @param  matches       a dtwSat S4 object with the matches that have been produced by the sits_TWTDW_matches function
#' @param  data.tb       the SITS tibble used as input for the TWDTW matching function
#' @param  start_date    date - the start of the classification period
#' @param  end_date      date - the end of the classification period
#' @param  interval      the period between two classifications
#' @param  overlap       minimum overlapping between one match and the interval of classification
#' @return class.tb      a SITS table with the information on matches for the data
#'
#' @export
#'
sits_TWDTW_classify <- function (matches, data.tb, start_date = NULL, end_date = NULL,
                        interval = "12 month", overlap = 0.5){

    # create a tibble to store the results
    i <- 1
    class.tb <- data.tb %>%
          purrrlyr::by_row (function (row) {

               if (purrr::is_null (start_date)) {
                    start_date  <- row$start_date
                    end_date    <- row$end_date
                    interval <- lubridate::as_date(end_date) - lubridate::as_date(start_date)
               }

               # define the temporal intervals of each classification
               breaks <- seq(from = as.Date(start_date), to = as.Date(end_date), by = interval)

               classify <- dtwSat::twdtwClassify(x = matches[[i]], breaks = breaks, overlap = overlap)
               class.lst <- sits_fromTWDTW_matches(classify)

               i <- i + 1

               # add the classification results to the input row
               return(class.lst[[1]])

          }, .to = "predicted")
    return (class.tb)
}

#' @title Spread matches from a sits matches tibble
#' @name sits_spread_matches
#' @author Victor Maus, \email{vwmaus1@@gmail.com}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Alexandre Xavier Ywata de Carvalho, \email{alexandre.ywata@@ipea.gov.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Given a SITS tibble with a set of TWDTW matches, returns a tibble whose columns have
#' the reference label and the TWDTW distances for each temporal pattern.
#'
#' @param  data.tb    a SITS matches tibble
#' @return result.tb  a tibble where whose columns have the reference label and the TWDTW distances for each temporal pattern
#' @export
sits_spread_matches <- function(data.tb){

    # Get best TWDTW aligniments for each class
    data.tb$matches <- data.tb$matches %>%
        purrr::map(function (data.tb){
            data.tb %>%
                dplyr::group_by(predicted) %>%
                dplyr::summarise(distance=min(distance))
        })

    # Select best match and spread pred to columns
    result.tb <- data.tb %>%
        dplyr::transmute(original_row = 1:NROW(.), reference = label, matches = matches) %>%
        tidyr::unnest(matches, .drop = FALSE) %>%
        tidyr::spread(key = predicted, value = distance)

    return(result.tb)
}
