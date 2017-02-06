#' Classify a sits tibble using TWDTW (using the dtwSat package)
#'
#' \code{sits_TWDTW} returns a sits table with values only
#'
#' A sits table has the metadata and data for each time series
#' <longitude, latitude, start_date, end_date, label, coverage, time_series>
#'
#' The TWDTW (time-weighted dynamical time warping) is a version of the
#' Dynamic Time Warping method for land use and land cover mapping using a sequence
#' of multi-band satellite images. Methods based on dynamic time warping are flexible to
#' handle irregular sampling and out-of-phase time series, and they have achieved significant
#' results in time series analysis.
#' The standard DTW compares a temporal signature of a known event (e.g., a person’s speech) with an unknown time series. It finds all
#' possible alignments between two time series and provides a dissimilarity measure.
#' In contrast to standard DTW, the TWDTW method is sensitive to seasonal
#' changes of natural and cultivated vegetation types. It also considers inter-annual climatic and
#' seasonal variability.
#'
#' Reference: Maus V, Camara G, Cartaxo R, Sanchez A, Ramos FM, de Queiroz GR (2016).
#' “A Time-Weighted Dynamic Time Warping Method for Land-Use and Land-Cover Mapping.” IEEE
#'  Journal of Selected Topics in Applied Earth Observations and Remote Sensing, 9(8):3729-3739,
#'  August 2016. ISSN 1939-1404. doi:10.1109/JSTARS.2016.2517118.
#'
#' @param  samples.tb    a table in SITS format with time series to be classified using TWTDW
#' @param  patterns.tb   a set of known temporal signatures for the chosen classes
#' @param  bands         string - the bands to be used for classification
#' @param  alpha         (double) - the steepness of the logistic function used for temporal weighting
#' @param  beta          (integer) - the midpoint (in days) of the logistic function
#' @return matches       a SITS table with the information on matches for the data
#' @export
#'
#'
sits_TWTDW <- function (samples.tb, patterns.tb, bands, alpha = -0.1, beta = 100) {

     # internal function that converts a SITS table to a TWDTW time series
     toTWDTW <- function (data.tb){
          zoo.ls <- data.tb$time_series %>%
               map (function (ts) {
                    df <- data.frame (ts)
                    return (zoo (df[,2:ncol(df)], df[,1]))
               })
          labels.fc <-  as.factor (data.frame (select (data.tb, label))[,1])

          ptt = new("twdtwTimeSeries", timeseries = zoo.ls,
                    labels = labels.fc)
          return (ptt)
     }

     # select the bands for the samples time series and convert to TWDTW format
     ts_samples <- samples.tb %>%
          sits_select (bands) %>%
          toTWDTW()

     # select the bands for patterns time series and convert to TWDTW format
     ts_patterns <- patterns.tb %>%
     sits_select (bands) %>%
     sits_toTWDTW()

     # Define the logistic function
     log_fun = logisticWeight(alpha=-0.05, beta=100)

august_july = seq(from = as.Date("2000-09-01"),
                  to   = as.Date("2016-08-31"),
                  by   = "12 month")

matches = twdtwApply(x = ts_samples,
                     y = ts_patterns,
                     weight.fun = log_fun,
                     breaks = august_july,
                     keep=TRUE)

# # plot the classification
# plot(x = matches, type = "classification", overlap = 0.5)
# # plot the alignments
# plot(x = matches, type = "alignments")

return (matches)

}
