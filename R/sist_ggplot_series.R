#'
#' Plot one or more timeSeries using ggplot
#'
#' \code{sits_ggplot_series} plots a set of  time series
#'
#'
#' @param tb          a SITS table with the time series to be plotted
#' @param colors      string - the set of Brewer colors to be used for plotting
#' @keywords SITS
#' @family   SITS auxiliary functions
#' @export
#'
sits_ggplot_series <- function (tb, colors = "Dark2") {
     # create the plot title
     plot_title <- sits_plot_title (tb[1,])
     #extract the time series
     data.ts <- tb$time_series
     # melt the data into long format
     melted.ts <- data.ts %>%
          melt (id.vars = "Index")
     # plot the data with ggplot
     g <- ggplot(melted.ts, aes(x=Index, y=value, group = variable)) +
          geom_line(aes(color = variable)) +
          labs (title = plot_title) +
          scale_color_brewer (palette = colors)
     # scale_colour_hue(h=c(90, 270)) - alternative to brewer
     return (g)
}
