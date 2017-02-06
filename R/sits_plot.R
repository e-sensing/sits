# -----------------------------------------------------------
#' Plot a set of time series
#'
#' \code{sits_plot} plot one or more time series
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
#' @examples sits_plot  ts <- (ts, type = "grouped", colors = "Set1")
#' @export
#'
sits_plot_together <- function (data.tb, colors = "Dark2") {

     # this function converts the index of a time series to the julian day of a reference year
     sits_julian_conv <- function (ts) {
          # convert the time index to a reference year
          start_date <- ts[[1,"Index"]]
          ts <- dplyr::mutate (ts, Index = Index - ymd(start_date) + ymd (start_date.gl))
          return (ts)
     }
     # this function plots all the values of all time series together (for one band)
     plot_samples <- function (ts, band, label, number) {
          series.tb <- ts %>%
               purrr::map (sits_julian_conv)             %>%  # convert all series to same julian day
               data.frame ()                      %>%  # convert list to data frame
               as_tibble()                        %>%  # convert data fram to tibble
               dplyr::select (Index, starts_with(band))       # select only the index and the chosen band

          # create a data frame with the mean and plus and minus 1 standard deviation
          # convert to tibble and create now coluns with mean +- std
          means.tb <- data.frame  (Index = series.tb$Index,
                                   mean = rowMeans(series.tb[,2:ncol(series.tb)]),
                                   std  = apply   (series.tb[,2:ncol(series.tb)],1,sd)) %>%
               as_tibble() %>%
               dplyr::mutate (stdplus = mean + std, stdminus = mean - std)

          # melt the data into long format (required for ggplot to work)
          melted.tb <- series.tb %>%
               reshape2::melt (id.vars = "Index")
          # make the plot title
          title <- paste ("Samples (", number, ") for class ", label, " in band = ", band, sep="")
          # plot all data together
          g <- sits_ggplot_together (melted.tb, means.tb, title)
          plot (g)
     }

     # how many different labels are there?
     labels <- distinct (data.tb, label)

     for (i in 1:nrow (labels)) {
          lb = as.character (labels [i,"label"])
          # filter only those rows with the same label
          data2.tb <- dplyr::filter (data.tb, label == lb)
          # extract the time series from the tibble
          ts <- data2.tb$time_series
          # how many time series are to be plotted?
          number <- nrow (data2.tb)
          # what are the band names?
          bands  <- sits_bands (data2.tb)
          # plot all samples for the same label
          for (band in bands) plot_samples(ts, band, lb, number)
     }
}


