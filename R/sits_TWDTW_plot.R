#' @title Plot classification alignments using the dtwSat package
#' @name sits_plot_TWDTW_alignments
#' @author Victor Maus, \email{vwmaus1@@gmail.com}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description        plots the alignments from TWDTW classification (uses dtwSat)
#' @param matches      a list of dtwSat S4 matches objects produced by sits_TWDTW_matches
#'
#' @export
sits_plot_TWDTW_alignments <- function (matches){

    matches %>%
        purrr::map( function (m.twdtw) {
            dtwSat::plot (m.twdtw, type = "alignments") %>%
                graphics::plot()
        })
    return (invisible(matches))
}

#' @title Plot classification results using the dtwSat package
#' @name sits_plot_TWDTW_classification
#' @author Victor Maus, \email{vwmaus1@@gmail.com}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description         Plots the results of TWDTW classification (uses dtwSat)
#' @param  matches      A list of dtwSat S4 matches objects produced by sits_TWDTW_matches
#' @param  start_date   Start date of the plot (used for showing classifications)
#' @param  end_date     End date of the plot (used for showing classifications)
#' @param  interval     Interval between classifications (used for showing classifications)
#' @param  overlap      Minimum overlapping between one match and the interval of classification. For details see dtwSat::twdtwApply help.
#' @export
sits_plot_TWDTW_classification <- function (matches, start_date = NULL, end_date = NULL, interval = "12 month", overlap = 0.5){

    matches %>%
        purrr::map(function (m.twdtw) {
            if (purrr::is_null (start_date) | purrr::is_null (end_date))
                dplot <- dtwSat::plot (m.twdtw, type = "classification", overlap = 0.5)
            else
                dplot <- dtwSat::plot (m.twdtw, type = "classification", from = start_date,
                                       to = end_date, by = interval, overlap = overlap)
            graphics::plot(dplot)

        })
    return (invisible(matches))
}

#' @title Plot matches between a label pattern and a time series using the dtwSat package
#' @name sits_plot_TWDTW_matches
#' @author Victor Maus, \email{vwmaus1@@gmail.com}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Plots the matches from TWDTW classification for one label
#'
#' @param matches       a list of dtwSat S4 matches objects produced by sits_TWDTW_matches
#' @param patterns.tb   a set of known temporal signatures for the chosen classes
#' @param n_matches     number of matches of a given label to be displayed
#' @export
sits_plot_TWDTW_matches <- function (matches, patterns.tb, n_matches = 4) {

    matches %>%
        purrr::map(function (m.twdtw) {
            dtwSat::plot (m.twdtw, type = "matches", patterns.labels = patterns.tb$label, k = n_matches) %>%
                graphics::plot()
        })
    return (invisible(matches))
}
