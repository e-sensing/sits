# Plot a set of time series for a given value and label
#'
#' \code{sits_plot_together} plots all samples of a SITS table having the same label
#'
#' @param data.tb   tibble - a table with a set of time series
#' @return          none
sits_plot_together <- function (data.tb, colors = "Dark2") {

     # this function converts the index of a time series to the julian day of a reference year
     sits_julian_conv <- function (ts) {
          # convert the time index to a reference year
          from <- ts[[1,"Index"]]
          ts <- dplyr::mutate (ts, Index = Index - ymd(from) + ymd (from.gl))
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
          bands  <- sits_band_names (data2.tb[1,]$time_series)
          # plot all samples for the same label
          for (band in bands) plot_samples(ts, band, lb, number)
     }
}
