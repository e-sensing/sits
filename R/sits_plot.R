# -----------------------------------------------------------
#' Plot a set of time series for the same spatio-temporal reference
#'
#' \code{sits_plot} plot one time series for a single place and interval
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
          locs <- dplyr::distinct (data.tb, longitude, latitude)
          for (i in 1:nrow (locs)) {
               long = as.double (locs [i,"longitude"])
               lat  = as.double (locs [i, "latitude"])
               # filter only those rows with the same label
               data2.tb <- dplyr::filter (data.tb, longitude == long, latitude == lat)
               # use ggplot to plot the time series together
               data2.tb %>%
                    sits_ggplot_series(colors) %>%
                    plot()
          }
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
     } else if (type == "patterns"){
          data.tb %>%
               sits_toTWDTW() %>%
               dtwSat::plot (type = "patterns") %>%
               plot()
     }

     # return the original SITS table - useful for chaining
     return (data.tb)
}


