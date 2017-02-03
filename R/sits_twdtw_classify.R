#' Classify a sits tibble using TWDTW
#'
#' \code{sits_twdtw_classify} returns a sits table with values only
#'
#' A sits table has the metadata and data for each time series
#' <longitude, latitude, from, to, label, coverage, time_series>
#'
#' @param  data.tb a tibble in SITS format with time series for different bands
#' @param  bands   string - the bands to be used for classification
#' @return table   a tibble in SITS format with values
#' @export
#'
#'
sits_twdtw_classify <- function (samples.tb, patterns.tb, bands, alpha = -0.1, beta = 100) {

# unclassified time series
ts_samples <- samples.tb %>%
     sits_select (bands) %>%
     sits_toTWDTW()

#patterns time series
ts_patterns <- patterns.tb %>%
     sits_select (bands) %>%
     sits_toTWDTW()

# Applying TWDTW Analysis
log_fun = logisticWeight(alpha=-0.05, beta=100)

august_july = seq(from = as.Date("2000-09-01"),
                  to = as.Date("2016-08-31"),
                  by = "12 month")

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
