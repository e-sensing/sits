# -----------------------------------------------------------
#' Plot a set of time series for the same spatio-temporal reference
#'
#' \code{sits_plot_table} plot one time series for a single place and interval
#'
#' Plot one SITS time series for a single place and interval
#'
#' @param ts   a tibble the list of time series to be plotted
#' @param colours   the color pallete to be used (default is "Set1")
#' @return          none
#' @keywords STIS
#' @family   STIS auxiliary functions
#' @examples sits_plot  (ts, "Set1")
#'
#'
sits_plot <- function (data.tb, type = "joined", colours = "Dark2") {

     if (type != "joined" && type != "separated" && type != "grouped") {
          message (paste ("sits_plot: valid plot types are joined, separate or grouped", "\n", sep = ""))
     }

     # plot all points joined in time
     if (type == "joined") {
          plot_title <- sits_make_plot_title (data.tb[1,])
          data.tb$time_series %>%
               sits_ggplot_series(plot_title) %>%
               plot()
     }
     # plot one by one
     else if (type == "separated") {
          for (i in 1:nrow (data.tb)) {
               plot_title <- sits_make_plot_title (data.tb[i,])
               data.tb[i,]$time_series %>%
                    sits_ggplot_series (plot_title) %>%
                    plot ()
          }
     } else if (type == "grouped"){
          label <- data.tb[1,]$label
          number <- nrow (data.tb)
          bands  <- sits_band_names (data.tb[1,]$time_series)
          sits_plot_all_samples (data.tb$time_series, bands, label, number)
     }
}
# Plot a set of time series for a given value and label
#'
#' \code{sits_plot_all_samples} plots all samples for a given table and band
#'
#'
#' @param ts        the list of time series to be plotted (a list of tibbles)
#' @param bands     thes bands to be plotted
#' @return          none
sits_plot_all_samples <- function (ts, bands, label, number) {

     # this function converts the index of a time series to the julian day of a reference year
     sits_julian_conv <- function (ts) {
          # convert the time index to a reference year
          from <- ts[[1,"Index"]]
          ts <- mutate (ts, Index = Index - ymd(from) + ymd (from.global))
          return (ts)
     }
     # this function plots all the values of all time series together (for one band)
     plot_samples <- function (ts, band, label, number) {
          series.tb <- ts %>%
               map (sits_julian_conv)             %>%  # convert all series to same julian day
               data.frame ()                      %>%  # convert list to data frame
               as_tibble()                        %>%  # convert data fram to tibble
               select (Index, starts_with(band))       # select only the index and the chosen band

          # create a data frame with the mean and plus and minus 1 standard deviation
          # convert to tibble and create now coluns with mean +- std
          means.tb <- data.frame  (Index = series.tb$Index,
                                   mean = rowMeans(series.tb[,2:ncol(series.tb)]),
                                   std  = apply   (series.tb[,2:ncol(series.tb)],1,sd)) %>%
               as_tibble() %>%
               mutate (stdplus = mean + std, stdminus = mean - std)

          # melt the data into long format (required for ggplot to work)
          melted.tb <- series.tb %>%
               melt (id.vars = "Index")
          # make the plot title
          title <- paste ("Samples (", number, ") for class ", label, " in band = ", band, sep="")
          # plot all data together
          g <- sits_ggplot_together (melted.tb, means.tb, title)
          plot (g)
     }
     for (band in bands) plot_samples(ts, band, label, number)
}


#'
#' Plot one or more timeSeries using ggplot
#'
#' \code{sits_ggplot_series} plots a set of  time series
#'
#'
#' @param data.ts     the time series to be plotted (tibble in SITS format)
#' @param plot_title  string - the plot title
#' @param colors      string - the set of Brewer colors to be used for plotting
#' @keywords SITS
#' @family   SITS auxiliary functions
#'
sits_ggplot_series <- function (data.ts, plot_title, colors = "Dark2") {
     # melt the data into long format
     melted.tb <- data.ts %>%
          melt (id.vars = "Index")
     # plot the data with ggplot
     g <- ggplot(melted.tb, aes(x=Index, y=value, group = variable)) +
          geom_line(aes(color = variable)) +
          labs (title = plot_title) +
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
#' @keywords SITS
#' @family   SITS auxiliary functions
#'
# plot the data with ggplot
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
#' Create a plot title with ggplot
#'
#' \code{sits_make_plot_title} creates a plot title from row information
#'
#' @param row      data row with <longitude, latitude, label> information
#' @return title   string - the title to be used in the plot
#' @keywords SITS
#' @family   SITS auxiliary functions
#'
sits_make_plot_title <- function (row) {
     title <- paste ("location (",
                     row$latitude,  ", ",
                     row$longitude, ") - ",
                     row$label,
                     sep = "")
     return (title)
}
