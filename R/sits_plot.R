# -----------------------------------------------------------
#' Plot a set of time series for the same spatio-temporal reference
#'
#' \code{sits_plot_table} plot one time series for a single place and interval
#'
#' Plot one SITS time series for a single place and interval
#'
#' @param    data.tb    tibble - a SITS table with the list of time series to be plotted
#' @param    colours    the color pallete to be used (default is "Set1")
#' @return   data.tb    tibble - the input SITS table (useful for chaining functions)
#' @keywords STIS
#' @family   STIS main functions
#' @export
#' @examples sits_plot  ts <- (ts, type = "grouped", colors = "Set1")
#'
sits_plot <- function (data.tb, type = "joined", colors = "Dark2") {

     if (type != "joined" && type != "separated" && type != "grouped" && type = "patterns") {
          message (paste ("sits_plot: valid plot types are joined, separated, grouped or patterns", "\n", sep = ""))
     }

     # plot all points joined in time
     if (type == "joined") {
          # create the plot title
          plot_title <- sits_make_plot_title (data.tb[1,])
          # use ggplot to plot the time series together
          data.tb$time_series %>%
               sits_ggplot_series(plot_title, colors) %>%
               plot()
     }
     # plot one by one
     else if (type == "separated") {
          # plot series one by one
          for (i in 1:nrow (data.tb)) {
               # create the plot title
               plot_title <- sits_make_plot_title (data.tb[i,])
               # use ggplot to plot each time series separately
               data.tb[i,]$time_series %>%
                    sits_ggplot_series (plot_title, colors) %>%
                    plot ()
          }
     } else if (type == "grouped"){
          # retrieve the label of the class to be plotted
          label1 <- data.tb[1,]$label
          # filter only those rows with the same label
          data2.tb <- filter (data.tb, label == label1)
          # how many time series are to be plotted?
          number <- nrow (data2.tb)
          # what are the band names?
          bands  <- sits_band_names (data2.tb[1,]$time_series)
          # plot all samples for the same label
          sits_plot_all_samples (data2.tb$time_series, bands, label, number)
     }
     # return the original SITS table - useful for chaining
     return (data.tb)
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


#' @description Method for plotting the temporal patterns.
#'
#' @param x An object of class \code{\link[dtwSat]{twdtwTimeSeries}},
#' \code{\link[zoo]{zoo}}, or list of \code{\link[zoo]{zoo}}.
#' @param labels a vector with labels of the time series. If not declared
#' the function will plot all time series.
#' @param year An integer. The base year to shift the dates of the time series to.
#' If NULL then it does not shif the time series. Default is 2005.
#' @param attr An \link[base]{integer} vector or \link[base]{character} vector
#' indicating the attribute for plotting. If not declared the function will plot
#' all attributes.
#'
#' @return A \link[ggplot2]{ggplot} object.
#'
#' @seealso
#' \code{\link[dtwSat]{twdtwTimeSeries-class}} and
#' \code{\link[dtwSat]{plotTimeSeries}}
#'
#' @examples
#' patt = twdtwTimeSeries(MOD13Q1.patterns.list)
#' plotPatterns(patt)
#' plotPatterns(patt, attr="evi")
#'
#' @export
# plotPatterns = function(x, labels=NULL, attr, year=2005){
#
#      if(is(x, "twdtwMatches")) x = x@patterns
#      if(is(x, "twdtwTimeSeries")) x = subset(x, labels)
#      x = twdtwTimeSeries(x, labels)
#      labels = labels(x)
#
#      # Shift dates
#      if(!is.null(year)) x = shiftDates(x, year=year)
#
#      # Build data.frame
#      if(missing(attr)) attr = names(x[[1]])
#      df.p = do.call("rbind", lapply(labels, function(p){
#           ts = x[[p]][,attr,drop=FALSE]
#           data.frame(Time=index(ts), ts, Pattern=p)
#      }))
#      df.p = melt(df.p, id.vars=c("Time","Pattern"))
#
#      # Plot temporal patterns
#      gp = ggplot(df.p, aes_string(x="Time", y="value", colour="variable") ) +
#           geom_line() +
#           facet_wrap(~Pattern) +
#           theme(legend.position = "bottom") +
#           scale_x_date(labels = date_format("%b"))
#
#      gp
#
# }
