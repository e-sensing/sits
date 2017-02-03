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
sits_plot <- function (data.tb, type = "allyears", colors = "Dark2") {

     if (type != "allyears" && type != "one_by_one" && type != "together" && type != "patterns") {
          message (paste ("sits_plot: valid plot types are allyears, one_by_one, together or patterns", "\n", sep = ""))
     }

     # plot all points joined in time
     if (type == "allyears") {
          # use ggplot to plot the time series together
          data.tb %>%
               sits_ggplot_series(colors) %>%
               plot()
     }
     # plot one by one
     else if (type == "one_by_one") {
          # plot series one by one
          for (i in 1:nrow (data.tb)) {
               row <- data.tb[i,]
               # use ggplot to plot each time series separately
               row %>%
                    sits_ggplot_series (colors) %>%
                    plot ()
          }
     } else if (type == "together"){
          sits_plot_together (data.tb, colors = colors)
     }
     # return the original SITS table - useful for chaining
     return (data.tb)
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
