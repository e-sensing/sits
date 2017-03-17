# -----------------------------------------------------------
#' Plot a set of time series
#'
#' \code{sits_plot} plot one or more time series
#'
#' Plot one SITS time series for a single place and interval
#'
#' @param    data.tb    tibble - a SITS table with the list of time series to be plotted
#' @param    type       string - the type of plot to be generated
#' @param    colors     the color pallete to be used (default is "Set1")
#' @param    label      the class label
#' @param    k          number of alignments
#' @return   data.tb    tibble - the input SITS table (useful for chaining functions)
#' @keywords STIS
#' @export
#'
sits_plot <- function (data.tb = NULL, type = "allyears", colors = "Dark2", label = NULL, k = 4) {
     # check the input exists
     ensurer::ensure_that(data.tb, !purrr::is_null(.), err_desc = "sits_plot: input data not provided")

     switch(type,
            "allyears"       = .sits_plot_allyears (data.tb, colors),
            "one_by_one"     = .sits_plot_one_by_one (data.tb, colors),
            "together"       = .sits_plot_together (data.tb, colors),
            "patterns"       = .sits_plot_patterns (data.tb),
            "classification" = .sits_plot_classification (data.tb),
            "alignments"     = .sits_plot_alignments (data.tb),
            "matches"        = .sits_plot_matches (data.tb, label, k),
            message (paste ("sits_plot: valid plot types are allyears,
                            one_by_one, together, patterns, classification, alignments, matches", "\n", sep = ""))
            )

     # return the original SITS table - useful for chaining
     return (invisible (data.tb))
}

.sits_plot_allyears <- function (data.tb, colors) {
     locs <- dplyr::distinct (data.tb, longitude, latitude)
     locs %>%
          dplyr::rowwise() %>%
          dplyr::do({
               long = as.double (.$longitude)
               lat  = as.double (.$latitude)
               # filter only those rows with the same label
               data2.tb <- dplyr::filter (data.tb, longitude == long, latitude == lat)
               # use ggplot to plot the time series together
               data2.tb %>%
                    .sits_ggplot_series(colors) %>%
                    graphics::plot()
               return (data2.tb)
          })
}

.sits_plot_one_by_one <- function (data.tb, colors){
     data.tb %>%
          dplyr::rowwise() %>%
          dplyr::do({
               .sits_ggplot_series (.,colors) %>%
                    graphics::plot()
               return (data.tb)
          })

}
.sits_plot_patterns <- function (data.tb) {
     data.tb %>%
          .sits_toTWDTW_time_series() %>%
          dtwSat::plot (type = "patterns") %>%
          graphics::plot()
}

.sits_plot_classification <- function (data.tb){
     # retrieve a dtwSat S4 twdtwMatches object
     data.tb %>%
          dplyr::rowwise() %>%
          dplyr::do({
               dtwSat::plot (.$matches, type = "classification", overlap = 0.5) %>%
                    graphics::plot()
               return(data.tb)
          })
}
.sits_plot_alignments <- function (data.tb){
     data.tb %>%
          dplyr::rowwise() %>%
          dplyr::do({
               dtwSat::plot (.$matches, type = "alignments") %>%
                    graphics::plot()
               return(data.tb)
          })
}

.sits_plot_matches <- function (data.tb, label, k) {
     ensurer::ensure_that(label, !purrr::is_null(.), err_desc = "sits_plot matches: label must be provided")
     data.tb %>%
          dplyr::rowwise() %>%
          dplyr::do({
               dtwSat::plot (.$matches, type = "matches", patterns.labels = label, k = k) %>%
               graphics::plot()
          return(data.tb)
     })
}
# -----------------------------------------------------------
#' Plot a set of time series for the same spatio-temporal reference
#'
#' \code{.sits_plot_together} plots all time series for the same label together
#' This function is useful to find out the spread of the values of the time serie
#' for a given label
#'
#' @param    data.tb    tibble - a SITS table with the list of time series to be plotted
#' @param    colours    the color pallete to be used (default is "Set1")
#' @return   data.tb    tibble - the input SITS table (useful for chaining functions)
#' @keywords STIS
#'
.sits_plot_together <- function (data.tb, colors) {
     # this function plots all the values of all time series together (for one band)
     plot_samples <- function (ts, band, label, number) {
          series.tb <- ts %>%
               data.frame ()                      %>%  # convert list to data frame
               tibble::as_tibble()                        %>%  # convert data fram to tibble
               dplyr::select (Index, dplyr::starts_with(band))       # select only the index and the chosen band

          # create a data frame with the mean and plus and minus 1 standard deviation
          # convert to tibble and create now coluns with mean +- std
          means.tb <- data.frame  (Index = series.tb$Index,
                                   rmean = rowMeans(series.tb[,2:ncol(series.tb)]),
                                   std  = apply   (series.tb[,2:ncol(series.tb)],1,stats::sd)) %>%
               tibble::as_tibble() %>%
               dplyr::mutate (stdplus = rmean + std, stdminus = rmean - std)

          # melt the data into long format (required for ggplot to work)
          melted.tb <- series.tb %>%
               reshape2::melt (id.vars = "Index")
          # make the plot title
          title <- paste ("Samples (", number, ") for class ", label, " in band = ", band, sep="")
          # plot all data together
          g <- .sits_ggplot_together (melted.tb, means.tb, title)
          graphics::plot (g)
     }

     # how many different labels are there?
     labels <- dplyr::distinct (data.tb, label)

     labels %>%
          dplyr::rowwise() %>%
          dplyr::do({
               lb = as.character (.$label)
               # filter only those rows with the same label
               data2.tb <- dplyr::filter (data.tb, label == lb)
               # how many time series are to be plotted?
               number <- nrow (data2.tb)
               # what are the band names?
               bands  <- sits_bands (data2.tb)
               # what is the reference date?
               ref_date <- data2.tb[1,]$start_date
               # align all time series to the same date
               data2.tb <- sits_align(data2.tb, ref_date)
               # extract the time series
               ts <- data2.tb$time_series
               # plot all samples for the same label
               bands %>%
                    purrr::map (function (band) {plot_samples(ts, band, lb, number)})
               return (data2.tb)
          })
}

#'
#' Plot one timeSeries using ggplot
#'
#' \code{.sits_ggplot_series} plots a set of time series using ggplot
#'
#' This function is used for showing the same lat/long location in
#' a series of time steps.
#'
#' @param row         a row of a SITS table with the time series to be plotted
#' @param colors      string - the set of Brewer colors to be used for plotting
#' @keywords SITS
#' @family   SITS plot functions
.sits_ggplot_series <- function (row, colors = "Dark2") {
     # create the plot title
     plot_title <- .sits_plot_title (row)
     #extract the time series
     data.ts <- row$time_series
     # melt the data into long format
     melted.ts <- data.ts %>%
          reshape2::melt (id.vars = "Index") %>%
          as.data.frame()
     # plot the data with ggplot
     g <- ggplot2::ggplot(melted.ts, ggplot2::aes(x = Index, y = value, group = variable)) +
          ggplot2::geom_line(ggplot2::aes(color = variable)) +
          ggplot2::labs (title = plot_title) +
          # ylim(0.0, 1.0) +
          ggplot2::scale_color_brewer (palette = colors)
     # scale_colour_hue(h=c(90, 270)) - alternative to brewer
     return (g)
}
#'
#' Plot many timeSeries together using ggplot
#'
#' \code{.sits_ggplot_together} plots a set of  time series together
#'
#' @param melted.tb   a tibble with the time series (a lot of data already melted)
#' @param means.tb    a tibble with the means and std deviations of the time series
#' @param plot_title  the title for the plot
#' @keywords SITS
#' @family   SITS plot functions
.sits_ggplot_together <- function (melted.tb, means.tb, plot_title) {
     g <- ggplot2::ggplot(data = melted.tb, ggplot2::aes(x = Index, y = value, group = variable)) +
          ggplot2::geom_line(colour = "#819BB1", alpha = 0.5) +
          ggplot2::labs (title = plot_title) +
          ggplot2::geom_line (data = means.tb, ggplot2::aes (x = Index, y = rmean), colour = "#B16240", size = 2, inherit.aes=FALSE) +
          ggplot2::geom_line (data = means.tb, ggplot2::aes (x = Index, y = stdplus), colour = "#B19540", size = 1, inherit.aes=FALSE) +
          ggplot2::geom_line (data = means.tb, ggplot2::aes (x = Index, y = stdminus), colour = "#B19540", size = 1, inherit.aes=FALSE)
     return (g)

}

#'
#' Create a plot title to use with ggplot
#'
#' \code{sits_plot_title} creates a plot title from row information
#'
#' @param row      data row with <longitude, latitude, label> information
#' @return title   string - the title to be used in the plot
#' @keywords SITS
#' @family   SITS plotting functions
.sits_plot_title <- function (row) {
     title <- paste ("location (",
                     row$latitude,  ", ",
                     row$longitude, ") - ",
                     row$label,
                     sep = "")
     return (title)
}


