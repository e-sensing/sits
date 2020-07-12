#' @title Find matches between a set of sits patterns and segments of sits tibble using TWDTW
#' @name sits_twdtw_classify
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
#' @param  samples       A sits tibble to be classified using TWTDW.
#' @param  patterns      Patterns to be used for classification.
#' @param  bands         Names of the bands to be used for classification.
#' @param  dist.method   Name of the method to derive the local cost matrix.
#' @param  alpha         Steepness of the logistic function used for temporal weighting (a double value).
#' @param  beta          Midpoint (in days) of the logistic function (an integer).
#' @param  theta         Relative weight of the time distance compared to the dtw distance (a double value).
#' @param  span          Minimum number of days between two matches of the same pattern in the time series (approximate).
#' @param  keep          Keep internal values for plotting matches? (A logical value).
#' @param  start_date    Start date of the classification period.
#' @param  end_date      End date of the classification period.
#' @param  interval      Period between two classifications in months.
#' @param  overlap       Minimum overlapping between one match and the interval of classification.
#' @return A dtwSat S4 object with the matches.
#' @examples
#' \donttest{
#' # Retrieve the set of samples for the Mato Grosso region (provided by EMBRAPA)
#' samples <- sits_select_bands(samples_mt_6bands, ndvi, evi)
#'
#' # get a point and classify the point with the ml_model
#' point <- sits_select_bands(point_mt_6bands, ndvi, evi)
#'
#' # plot the series
#' plot(point)
#'
#' # obtain a set of patterns for these samples
#' patterns <- sits_patterns(samples)
#' plot(patterns)
#'
#' # find the matches between the patterns and the time series using the TWDTW algorithm
#' # (uses the dtwSat R package)
#' matches <- sits_twdtw_classify(point, patterns, bands = c("ndvi", "evi"),
#' alpha= -0.1, beta = 100, theta = 0.5, keep = TRUE)
#' }
#' @export
sits_twdtw_classify <- function(samples,
                                patterns,
                                bands = NULL,
                                dist.method = "euclidean",
                                alpha = -0.1,
                                beta = 100,
                                theta = 0.5,
                                span  = 0,
                                keep  = FALSE,
                                start_date = NULL,
                                end_date = NULL,
                                interval = "12 month",
                                overlap = 0.5){
    # verifies if dtwSat package is installed
    if (!requireNamespace("dtwSat", quietly = TRUE)) {
        stop("dtwSat needed for this function to work. Please install it.",
             call. = FALSE)
    }
    # backward compatibility
    samples <- .sits_tibble_rename(samples)

    # does the input data exist?
    .sits_test_tibble(samples)

    # add a progress bar
    progress_bar <- NULL
    if (nrow(samples) > 10) {
        message("Matching patterns to time series...")
        progress_bar <- utils::txtProgressBar(min = 0,
                                              max = nrow(samples),
                                              style = 3)
        i <- 0
    }

    # handle the case of null bands
    if (purrr::is_null(bands)) bands <- sits_bands(samples)

    # create a list to store the results of the TWDTW matches
    matches.lst <- list()

    # select the bands for patterns time series and convert to TWDTW format
    twdtw_patterns <- patterns %>%
        .sits_select_bands_(bands = bands) %>%
        .sits_to_twdtw()

    # Define the logistic function
    log_fun <- dtwSat::logisticWeight(alpha = alpha, beta = beta)

    n_rows_data <- NROW(samples)
    for (r in 1:n_rows_data) {
        row.tb <- samples[r, ]
        # select the bands for the samples time series and convert to TWDTW format
        twdtw_series <- row.tb %>%
            .sits_select_bands_(bands = bands) %>%
            .sits_to_twdtw()

        #classify the data using TWDTW
        matches <- dtwSat::twdtwApply(x          = twdtw_series,
                                      y          = twdtw_patterns,
                                      weight.fun = log_fun,
                                      theta      = theta,
                                      span       = span,
                                      keep       = keep,
                                      dist.method = dist.method)

        # add the matches to the list
        matches.lst[[length(matches.lst) + 1]] <- matches


        # update progress bar
        if (!purrr::is_null(progress_bar)) {
            i <- i + 1
            utils::setTxtProgressBar(progress_bar, i)
        }
    }
    if (!purrr::is_null(progress_bar)) close(progress_bar)

    .sits_plot_twdtw_alignments(matches.lst)

    # Classify a sits tibble using the matches found by the TWDTW methods
    samples  <- .sits_twdtw_breaks(matches.lst,
                                   samples,
                                   start_date = start_date,
                                   end_date = end_date,
                                   interval,
                                   overlap)
    # plot the classification
    .sits_plot_twdtw_classification(matches.lst,
                                    start_date = start_date,
                                    end_date = end_date,
                                    interval = interval,
                                    overlap = overlap)
    return(samples)
}

#' @title Classify a sits tibble using the matches found by the TWDTW methods
#' @name .sits_twdtw_breaks
#' @author Victor Maus, \email{vwmaus1@@gmail.com}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Returns a sits tibble with the results of the TWDTW classifier.
#' The TWDTW classifier uses the result of the sits_TWDTW_matches function to
#' find the best alignments of these matches in intervals chosen by the user
#' @references Maus V, Camara G, Cartaxo R, Sanchez A, Ramos FM, de Queiroz GR (2016).
#' A Time-Weighted Dynamic Time Warping Method for Land-Use and Land-Cover Mapping. IEEE
#'  Journal of Selected Topics in Applied Earth Observations and Remote Sensing, 9(8):3729-3739,
#'  August 2016. ISSN 1939-1404. doi:10.1109/JSTARS.2016.2517118.
#'
#' @param  matches       A dtwSat S4 object with the matches that have been produced by the sits_TWTDW_matches function.
#' @param  samples       A sits tibble used as input for the TWDTW matching function.
#' @param  start_date    The start date of the classification period.
#' @param  end_date      The end date of the classification period.
#' @param  interval      The period between two classifications.
#' @param  overlap       Minimum overlapping between one match and the interval of classification.
#' @return A sits tibble with the information on matches for the data.
.sits_twdtw_breaks <- function(matches,
                               samples,
                               start_date = NULL,
                               end_date = NULL,
                               interval = "12 month",
                               overlap = 0.5){

    # verifies if dtwSat package is installed
    if (!requireNamespace("dtwSat", quietly = TRUE)) {
        stop("dtwSat needed for this function to work.
             Please install it.", call. = FALSE)
    }

    # create a tibble to store the results
    i <- 1
    predicted.lst <-
        purrr::pmap(list(samples$start_date, samples$end_date),
                    function(row_start_date, row_end_date) {

                        if (purrr::is_null(start_date)) {
                            start_date  <- lubridate::as_date(row_start_date)
                            end_date    <- lubridate::as_date(row_end_date)
                            interval <- lubridate::as_date(end_date) -
                                        lubridate::as_date(start_date)
                        }

                        # classify using the TWDTWclassify function
                        classify <- dtwSat::twdtwClassify(x = matches[[i]],
                                                          from = as.Date(start_date),
                                                          to = as.Date(end_date),
                                                          by = interval,
                                                          overlap = overlap)
                        predicted.tb <- .sits_from_twdtw_matches(matches[[i]])

                        i <<- i + 1
                        # add the classification results to the input row
                        return(predicted.tb)

                    })

    samples$predicted <- predicted.lst

    return(samples)
}

#' @title Export data to be used by the dtwSat package
#' @name .sits_to_twdtw
#' @author Victor Maus, \email{vwmaus1@@gmail.com}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Converts data from a sits tibble to an instance of a TWDTW time series class.
#'
#' @param  samples      A tibble in sits format with time series
#'                      to be converted to TWTDW time series.
#' @return An object of the twdtwTimeSeries class).
.sits_to_twdtw <- function (samples){
    # verifies if methods package is installed
    if (!requireNamespace("methods", quietly = TRUE)) {
        stop("methods needed for this function to work.
             Please install it.", call. = FALSE)
    }

    # verifies if zoo package is installed
    if (!requireNamespace("zoo", quietly = TRUE)) {
        stop("zoo needed for this function to work.
              Please install it.", call. = FALSE)
    }
    # transform each sits time series into a list of zoo
    ts <- samples$time_series %>%
        purrr::map(function(ts) zoo::zoo(ts[,2:ncol(ts), drop=FALSE], ts$Index))

    # create a new twdtwTimeSeries object from list above
    ts.twdtw <- methods::new("twdtwTimeSeries", timeseries = ts,
                             labels = as.character(samples$label))
    return (ts.twdtw)
}

#' @title Transform patterns from TWDTW format to sits format
#' @name .sits_from_twdtw_matches
#'
#' @description Reads one TWDTW matches object and transforms it into a tibble ready to be stored into a sits tibble column.
#'
#' @param  match.twdtw  A TWDTW Matches object of class dtwSat::twdtwMatches (S4).
#' @return A tibble containing the matches information.
.sits_from_twdtw_matches <- function(match.twdtw){
    result.lst <- tibble::as_tibble(match.twdtw[[1]]) %>%
        dplyr::mutate(predicted = as.character(label)) %>%
        dplyr::select(-Alig.N, -label) %>%
        list()
    return(result.lst[[1]])
}
#' @title Plot classification alignments using the dtwSat package
#' @name .sits_plot_twdtw_alignments
#' @author Victor Maus, \email{vwmaus1@@gmail.com}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description        Plots the alignments from TWDTW classification
#' @param matches      A list of dtwSat S4 match objects
#'                     (produced by sits_TWDTW_matches).
.sits_plot_twdtw_alignments <- function(matches){
    # verifies if dtwSat package is installed
    if (!requireNamespace("dtwSat", quietly = TRUE)) {
        stop("dtwSat needed for this function to work.
             Please install it.", call. = FALSE)
    }

    matches %>%
        purrr::map(function(m.twdtw) {
            dtwSat::plot(m.twdtw, type = "alignments") %>%
                graphics::plot()
        })
    return(invisible(matches))
}

#' @title Plot classification results using the dtwSat package
#' @name .sits_plot_twdtw_classification
#' @author Victor Maus, \email{vwmaus1@@gmail.com}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description         Plots the results of TWDTW classification (uses dtwSat).
#'
#' @param  matches      dtwSatS4 matches objects produced by sits_TWDTW_matches.
#' @param  start_date   Start date of the plot (used for classifications).
#' @param  end_date     End date of the plot (used for classifications).
#' @param  interval     Interval between classifications.
#' @param  overlap      Minimum overlapping between one match
#'                      and the interval of classification.
#'                      For details see dtwSat::twdtwApply help.
.sits_plot_twdtw_classification <- function(matches,
                                            start_date = NULL,
                                            end_date = NULL,
                                            interval = "12 month",
                                            overlap = 0.5) {
    # verifies if dtwSat package is installed
    if (!requireNamespace("dtwSat", quietly = TRUE)) {
        stop("dtwSat needed for this function to work.
             Please install it.", call. = FALSE)
    }

    matches %>%
        purrr::map(function(m.twdtw) {
            if (purrr::is_null(start_date) | purrr::is_null(end_date))
                dplot <- dtwSat::plot(m.twdtw, type = "classification",
                                      overlap = 0.5)
            else
                dplot <- dtwSat::plot(m.twdtw, type = "classification",
                                      from = start_date,
                                      to = end_date,
                                      by = interval,
                                      overlap = overlap)
            graphics::plot(dplot)

        })
    return(invisible(matches))
}

