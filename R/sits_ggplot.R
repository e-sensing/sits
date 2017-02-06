#'
#' Plot one or more timeSeries using ggplot
#'
#' \code{sits_ggplot_series} plots a set of time series using ggplot
#'
#' This function is used for showing the same lat/long location in
#' a series of time steps.
#'
#' @param tb          a SITS table with the time series to be plotted
#' @param colors      string - the set of Brewer colors to be used for plotting
#' @keywords SITS
#' @family   SITS auxiliary functions
#' @export
sits_ggplot_series <- function (tb, colors = "Dark2") {
     # create the plot title
     plot_title <- sits_plot_title (tb[1,])
     #extract the time series
     data.ts <- tb$time_series
     # melt the data into long format
     melted.ts <- data.ts %>%
          reshape2::melt (id.vars = "Index")
     # plot the data with ggplot
     g <- ggplot(melted.ts, aes(x=Index, y=value, group = variable)) +
          geom_line(aes(color = variable)) +
          labs (title = plot_title) +
          # ylim(0.0, 1.0) +
          scale_color_brewer (palette = colors)
     # scale_colour_hue(h=c(90, 270)) - alternative to brewer
     return (g)
}
#'
#' Plot many timeSeries together using ggplot
#'
#' \code{sits_ggplot_together} plots a set of  time series together
#'
#' @param melted.tb   a tibble with the time series (a lot of data already melted)
#' @param means.tb    a tibble with the means and std deviations of the time series
#' @param plot_title  the title for the plot
#' @keywords SITS
#' @family   SITS plotting functions
#' @export
sits_ggplot_together <- function (melted.tb, means.tb, plot_title) {
     g <- ggplot(melted.tb, aes(x=Index, y=value, group = variable)) +
          geom_line(colour = "#819BB1", alpha = 0.5) +
          labs (title = plot_title) +
          geom_line (data = means.tb, aes (x = Index, y = mean), colour = "#B16240", size = 2, inherit.aes=FALSE) +
          geom_line (data = means.tb, aes (x = Index, y = stdplus), colour = "#B19540", size = 1, inherit.aes=FALSE) +
          geom_line (data = means.tb, aes (x = Index, y = stdminus), colour = "#B19540", size = 1, inherit.aes=FALSE)
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
#' @export
sits_plot_title <- function (row) {
     title <- paste ("location (",
                     row$latitude,  ", ",
                     row$longitude, ") - ",
                     row$label,
                     sep = "")
     return (title)
}
