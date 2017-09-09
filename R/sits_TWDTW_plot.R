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

#' @title Plotting subintervals classification
#' @author Victor Maus, \email{vwmaus1@@gmail.com}
#'
#' @description Method for plotting the classification of each
#' subinterval of the time series based on TWDTW analysis.
#'
#' @param x An object of class \code{\link[dtwSat]{twdtwMatches}}.
#' @param timeseries.labels the label or index of the time series.
#' Default is 1.
#' @param patterns.labels a vector with labels of the patterns. If not
#' declared the function will plot one alignment for each pattern.
#'
#' @param attr An \link[base]{integer} vector or \link[base]{character} vector
#' indicating the attribute for plotting. If not declared the function will plot
#' all attributes.
#' @param ... additional arguments passed to \code{\link[dtwSat]{twdtwClassify}}.
#'
#' @return A \link[ggplot2]{ggplot} object.
#' @export

plotClassification <-  function(x, timeseries.labels=NULL, patterns.labels=NULL, attr, ...){

#    if(length(timeseries.labels)>6) timeseries.labels = timeseries.labels[1:6]
#    x = subset(x, timeseries.labels, patterns.labels)

    ## Get data
    if(missing(attr)) attr = names(dtwSat::getTimeSeries(x,1)[[1]])
    df.x = do.call("rbind", lapply(as.list(x), function(xx){
        ts = dtwSat::getTimeSeries(xx)[[1]][,attr,drop=FALSE]
        data.frame(Time=index(ts), ts, Series=labels(xx)$timeseries)
    }))
    df.x = reshape2::melt(df.x, id.vars=c("Time","Series"))

    y.labels = scales::pretty_breaks()(range(df.x$value, na.rm = TRUE))
    y.breaks = y.labels

    df.pol = do.call("rbind", lapply(as.list(x), function(xx){
        best_class = xx[[1]]
        df.pol = do.call("rbind", lapply(1:nrow(best_class), function(i){
            data.frame(
                Time = c(best_class$from[i], best_class$to[i], best_class$to[i], best_class$from[i]),
                Group = rep(i, 4),
                Class = rep(as.character(best_class$label[i]), 4),
                value = rep(range(y.breaks, na.rm = TRUE), each=2))
        }))
        df.pol$Group = factor(df.pol$Group)
        df.pol$Class = factor(df.pol$Class)
        df.pol$Series = rep(as.character(labels(xx)$timeseries), length(df.pol$Time))
        df.pol
    }))

    I = min(df.pol$Time, na.rm = TRUE)-30 <= df.x$Time &
        df.x$Time <= max(df.pol$Time, na.rm = TRUE)+30

    df.x = df.x[I,,drop=FALSE]

    gp <-  ggplot2::ggplot() +
        ggplot2::facet_wrap(~Series, scales = "free_x", ncol=1) +
        ggplot2::geom_polygon(data=df.pol, ggplot2::aes_string(x='Time', y='value', group='Group', fill='Class'), alpha=.7) +
        ggplot2::scale_fill_brewer(palette="Set3") +
        ggplot2::geom_line(data=df.x, ggplot2::aes_string(x='Time', y='value', colour='variable')) +
        ggplot2::scale_y_continuous(expand = c(0, 0), breaks=y.breaks, labels=y.labels) +
        ggplot2::scale_x_date(breaks=ggplot2::waiver(), labels=ggplot2::waiver()) +
        ggplot2::theme(legend.position = "bottom") +
        ggplot2::guides(colour = ggplot2::guide_legend(title = "Bands")) +
        ggplot2::ylab("Value") +
        ggplot2::xlab("Time")

    gp
}
