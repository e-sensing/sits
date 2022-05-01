#' @title Find matches between patterns and time series using TWDTW
#' @name sits_twdtw_classify
#' @author Victor Maus, \email{vwmaus1@@gmail.com}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Returns  the results of the TWDTW matching function.
#' The TWDTW matching function compares the values of a satellite image time
#' series with the values of known patters and tries to match each pattern
#' to a part of the time series
#'
#' The TWDTW (time-weighted dynamical time warping) is a version of the
#' Dynamic Time Warping method for LUCC mapping using a sequence
#' of multi-band satellite images. Methods based on dynamic time warping
#' are flexible to handle irregular sampling and out-of-phase time series,
#' and they have achieved significant
#' results in time series analysis. In contrast to standard DTW, the TWDTW
#' method is sensitive to seasonal changes of natural and cultivated
#' vegetation types. It also considers inter-annual climatic and
#' seasonal variability.
#'
#' @references
#' Maus V, Camara G, Cartaxo R, Sanchez A, Ramos F, Queiroz G (2016).
#' A Time-Weighted Dynamic Time Warping Method for Land-Use and
#' Land-Cover Mapping. IEEE Journal of Selected Topics in Applied Earth
#' Observations and Remote Sensing, 9(8):3729-3739,
#'  August 2016. ISSN 1939-1404. doi:10.1109/JSTARS.2016.2517118.
#'
#' @param  samples       A sits tibble to be classified using TWDTW.
#' @param  patterns      Patterns to be used for classification.
#' @param  bands         Names of the bands to be used for classification.
#' @param  dist_method   Name of the method to derive the local cost matrix.
#' @param  alpha         Steepness of the logistic function used for
#'                       temporal weighting (a double value).
#' @param  beta          Midpoint (in days) of the logistic function.
#' @param  theta         Relative weight of the time distance compared
#'                       to the dtw distance.
#' @param  span          Minimum number of days between two matches of the
#'                       same pattern in the time series (approximate).
#' @param  keep          Keep internal values for plotting matches?
#' @param  start_date    Start date of the classification period.
#' @param  end_date      End date of the classification period.
#' @param  interval      Period between two classifications in months.
#' @param  overlap       Minimum overlapping between one match and
#'                       the interval of classification.
#' @param  .plot         Plot the output?
#' @return A dtwSat S4 object with the matches.
#'
#' @examples
#' if (sits_active_tests()) {
#' # Retrieve the set of samples for the Mato Grosso region
#' samples <- sits_select(samples_modis_4bands, bands = c("NDVI", "EVI"))
#'
#' # get a point and classify the point with the ml_model
#' point <- sits_select(samples_modis_4bands, bands = c("NDVI", "EVI"))
#'
#' # plot the series
#' plot(point)
#'
#' # obtain a set of patterns for these samples
#' patterns <- sits_patterns(samples)
#' plot(patterns)
#'
#' # find the matches between the patterns and the time series
#' # using the TWDTW algorithm
#' # (uses the dtwSat R package)
#' matches <- sits_twdtw_classify(point, patterns,
#'   bands = c("NDVI", "EVI"),
#'   alpha = -0.1, beta = 100, theta = 0.5, keep = TRUE
#' )
#' }
#'
#' @export
sits_twdtw_classify <- function(samples,
                                patterns,
                                bands = NULL,
                                dist_method = "euclidean",
                                alpha = -0.1,
                                beta = 100,
                                theta = 0.5,
                                span = 0,
                                keep = FALSE,
                                start_date = NULL,
                                end_date = NULL,
                                interval = "12 month",
                                overlap = 0.5,
                                .plot = TRUE) {
    # verifies if dtwSat package is installed
    suppressWarnings(.check_require_packages("dtwSat"))

    # does the input data exist?
    .sits_tibble_test(samples)

    # check the bands
    bands <- .sits_tibble_bands_check(samples, bands)

    # create a list to store the results of the TWDTW matches
    matches <- list()

    # select the bands for patterns time series and convert to TWDTW format
    twdtw_patterns <- patterns %>%
        sits_select(bands = bands) %>%
        .sits_twdtw_from_tibble()

    # Define the logistic function
    log_fun <- dtwSat::logisticWeight(alpha = alpha, beta = beta)

    matches_lst <- samples %>%
        slider::slide(function(row) {
            # select the bands for the samples time series
            # and convert to TWDTW format
            twdtw_series <- row %>%
                sits_select(bands = bands) %>%
                .sits_twdtw_from_tibble()

            # classify the data using TWDTW
            matches <- dtwSat::twdtwApply(
                x = twdtw_series,
                y = twdtw_patterns,
                weight.fun = log_fun,
                theta = theta,
                span = span,
                keep = keep,
                dist.method = dist_method
            )
            return(matches)
        })

    if (.plot) {
        .sits_plot_twdtw_alignments(matches_lst)
    }

    # Classify a sits tibble using the matches found by the TWDTW methods
    samples <- .sits_twdtw_breaks(matches_lst,
                                  samples,
                                  start_date = start_date,
                                  end_date = end_date,
                                  interval,
                                  overlap
    )
    # plot the classification
    if (.plot) {
        .sits_plot_twdtw_class(matches_lst,
                               start_date = start_date,
                               end_date = end_date,
                               interval = interval,
                               overlap = overlap
        )
    }
    return(samples)
}

#' @title Classify a sits tibble using the matches found by the TWDTW methods
#' @name .sits_twdtw_breaks
#' @keywords internal
#' @author Victor Maus, \email{vwmaus1@@gmail.com}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Returns a sits tibble with the results of the TWDTW classifier.
#' The TWDTW classifier uses the result of the sits_TWDTW_matches function to
#' find the best alignments of these matches in intervals chosen by the user
#' @references
#' Maus V, Camara G, Cartaxo R, Sanchez A, Ramos FM, de Queiroz GR (2016).
#' A Time-Weighted Dynamic Time Warping Method for Land-Use and Land-Cover
#' Mapping. IEEE Journal of Selected Topics in Applied Earth Observations
#' and Remote Sensing, 9(8):3729-3739, August 2016.
#' ISSN 1939-1404. doi:10.1109/JSTARS.2016.2517118.
#'
#' @param  matches       A dtwSat S4 object with the matches
#'                       produced by the sits_TWDTW_matches function.
#' @param  samples       Input for the TWDTW matching function.
#' @param  start_date    The start date of the classification period.
#' @param  end_date      The end date of the classification period.
#' @param  interval      The period between two classifications.
#' @param  overlap       Minimum overlap between one match and
#'                       the interval of classification.
#' @return A sits tibble with the information on matches for the data.
.sits_twdtw_breaks <- function(matches,
                               samples,
                               start_date = NULL,
                               end_date = NULL,
                               interval = "12 month",
                               overlap = 0.5) {

    # verifies if dtwSat package is installed
    .check_require_packages("dtwSat")

    # create a tibble to store the results
    predicted_lst <-
        purrr::map2(
            matches, seq_len(nrow(samples)),
            function(match, i) {
                if (purrr::is_null(start_date)) {
                    start_date <- lubridate::as_date(samples[i, ]$start_date)
                    end_date <- lubridate::as_date(samples[i, ]$end_date)
                    interval <- lubridate::as_date(end_date) -
                        lubridate::as_date(start_date)
                }
                # classify using the TWDTWclassify function
                twdtw_obj <- dtwSat::twdtwClassify(
                    x = match,
                    from = as.Date(start_date),
                    to = as.Date(end_date),
                    by = interval,
                    overlap = overlap
                )
                predicted <- .sits_twdtw_matches_to_prediction(twdtw_obj)
                # add the classification results to the input row
                return(predicted)
            }
        )
    samples$predicted <- predicted_lst

    return(samples)
}

#' @title Export data to be used by the dtwSat package
#' @name .sits_twdtw_from_tibble
#' @keywords internal
#' @author Victor Maus, \email{vwmaus1@@gmail.com}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Converts sits tibble to an instance of a TWDTW class.
#'
#' @param  samples      A tibble in sits format with time series
#'                      to be converted to TWDTW time series.
#' @return An object of the twdtwTimeSeries class).
.sits_twdtw_from_tibble <- function(samples) {
    # verifies if methods and zoo packages are installed
    .check_require_packages(c("methods", "zoo"))

    # verifies if  package is installed
    # transform each sits time series into a list of zoo
    ts <- samples$time_series %>%
        purrr::map(
            function(ts) zoo::zoo(ts[, 2:ncol(ts), drop = FALSE], ts$Index)
        )

    # create a new twdtwTimeSeries object from list above
    ts_twdtw <- methods::new("twdtwTimeSeries",
                             timeseries = ts,
                             labels = as.character(samples$label)
    )
    return(ts_twdtw)
}

#' @title Transform patterns from TWDTW format to sits format
#' @name .sits_twdtw_matches_to_prediction
#' @keywords internal
#'
#' @description Reads one TWDTW matches object and transforms it into a
#'              tibble ready to be stored into a sits tibble column.
#'
#' @param  match  A TWDTW Matches object of class dtwSat::twdtwMatches
#' @return A list with information on the matches
.sits_twdtw_matches_to_prediction <- function(match) {
    results <- tibble::as_tibble(match[[1]]) %>%
        dplyr::mutate(predicted = as.character(.data[["label"]])) %>%
        dplyr::select(-.data[["Alig.N"]], -.data[["label"]]) %>%
        list()
    return(results)
}
