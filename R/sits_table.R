#' Create a sits table to store the time series information
#'
#' \code{sits_table} returns an empty sits table
#' A sits table is a tibble with pre-defined columns that
#' has the metadata and data for each time series. The columns are
#' <longitude, latitude, start_date, end_date, label, coverage, time_series>
#'
#' Most functions on the sits package use a sits table as input (with additional parameters)
#' and a sits table as output. This allows for chaining of operation on time series
#'
#' @return table  a tibble in SITS format
#' @export

sits_table <- function () {
     df <- data.frame(longitude   = double(),
                      latitude    = double (),
                      start_date  = as.Date(character()),
                      end_date    = as.Date(character()),
                      label       = character(),
                      coverage    = character(),
                      stringsAsFactors = FALSE
     )
     tb <- as_tibble (df)
     tb <- add_column (tb, time_series = list())
     class (tb) <- append (class(tb), "sits_table")
     return (tb)
}
#' Return the values of one band of a SITS table
#'
#' \code{sits_value_rows} returns a sits table with values only (rowwise organized)
#'
#' A sits table has the metadata and data for each time series
#' <longitude, latitude, start_date, end_date, label, coverage, time_series>
#'
#' @param  data.tb a tibble in SITS format with time series for different bands
#' @param  band    string - a band whose values are to be extracted
#' @return table   a tibble in SITS format with values
#' @export

sits_values_rows <- function (data, band) {
     values <- data$time_series %>%
          data.frame() %>%
          as_tibble() %>%
          dplyr::select (dplyr::starts_with (band)) %>%
          t()
     return (values)
}

#' Return the values of one band of a SITS table (colwise organised)
#'
#' \code{sits_value_cols} returns a sits table with values only (colwise organised)
#'
#' A sits table has the metadata and data for each time series
#' <longitude, latitude, start_date, end_date, label, coverage, time_series>
#'
#' @param  data.tb a tibble in SITS format with time series for different bands
#' @param  band    string - a band whose values are to be extracted
#' @return table   a tibble in SITS format with values
#' @export

sits_values_cols <- function (data, band) {
     values <- data$time_series %>%
          data.frame() %>%
          as_tibble() %>%
          dplyr::select (dplyr::starts_with (band))
     return (values)
}
#' Filter bands on a SITS table
#'
#' \code{sits_select} returns a sits table with the selected bands
#'
#' @param data.tb - a sits table with the time series of the selected bands
#' @param bands   - a vector of bands
#' @return table  a tibble in SITS format with the selected bands
#' @export

sits_select <- function (data.tb, bands) {
     # create a new table to store the result
     new.tb <- sits_table()
     # select the metadata attributes from the input table
     new.tb <- dplyr::select (data.tb, longitude, latitude, start_date, end_date, label, coverage)
     # select the chosen bands for the time series
     new.tb$time_series <- data.tb$time_series %>%
          purrr::map (function (ts) ts <- ts [,c("Index", bands)])
     # return the result
     return (new.tb)
}
#' Finds the names of the bands of a time series
#'
#' \code{sits_renames} renames the bands of a sits table
#' @param in.tb    a SITS table with a list of SITS time series
#' @param bands    a list of new band names
#' @return out.tb  a SITS table with a list of renamed bands for the time series
#' @export
#'
sits_rename <-  function (in.tb, bands_new) {

     if (is.null(bands_new)) message (paste, "New band names should be provided")

     # rename the time series
     out.ts <- in.tb$time_series %>%
          purrr::map (function (ts) {
               ts_out <- ts
               colnames (ts_out) <- c("Index", bands_new)
               return (ts_out)
          })
     out.tb <- dplyr::select (in.tb, latitude, longitude, start_date, end_date, label, coverage)
     out.tb$time_series <- out.ts

     return (out.tb)
}
#' Finds the names of the bands of a time series
#'
#' \code{sits_bands} finds the names of the bands of time series in a sits table
#' @param sits.tb     a valid sits table
#' @return names      a string vector with the names of the bands
#' @export
#'
sits_bands <- function (sits.tb) {
     names <- sits.tb[1,]$time_series %>%
          data.frame() %>%
          colnames() %>%
          . [2:length(.)]
     return (names)
}

# -----------------------------------------------------------
#' Merge two satellite image time series
#'
#' \code{sits_merge} merges two STIS tables with the same spatio-temporal references
#'
#' #' To merge two series, we consider that they contain different
#' attributes but refer to the same coverage, and spatio-temporal location.
#'
#' This function is useful to merge different bands of the same spatio-temporal locations.
#' For example, one may want to put the raw and smoothed bands for the same set of locations
#' in the same table.
#'
#' @param ts1  the first SITS table to be merged
#' @param ts2  the second SITS table to be merged
#' @return new.tb    a merged SITS tibble with a nested set of time series
#' @keywords SITS
#' @family   SITS main functions
#' @export
#' @examples merged.tb <- merge_WTSS (sits1.tb, sits2.tb)
#'
sits_merge <-  function(sits1.tb, sits2.tb) {

     # merge the time series
     merge_one <-  function (ts1, ts2) {
          ts3 <- dplyr::left_join (ts1, ts2, by = "Index")
     }
     # first, select the metadata columns
     merged.tb <- sits1.tb %>%
          dplyr::select (latitude, longitude, start_date, end_date, label, coverage)

     # then merge the data sets
     sits1.data <- sits1.tb$time_series
     sits2.data <- sits2.tb$time_series
     # join the attributes and values using zoo merge
     merged.tb$time_series <- purrr::map2 (sits1.data, sits2.data, merge_one)

     return (merged.tb)
}
#' Return the dates of a sits table
#'
#' \code{sits_dates} returns a sits table with dates only (colwise organised)
#'
#' A sits table has the metadata and data for each time series
#' <longitude, latitude, start_date, end_date, label, coverage, time_series>
#'
#' @param  data.tb a tibble in SITS format with time series for different bands
#' @return table   a tibble in SITS format with values of time indexes
#' @export

sits_dates <- function (data) {
     values <- data$time_series %>%
          data.frame() %>%
          as_tibble() %>%
          dplyr::select (dplyr::starts_with ("Index"))
     return (values)
}
#'
#' Aligns dates of time series to a reference date
#'
#' \code{sits_align} convert the index of a sits time to a reference year
#' The reference year is taken from the date of the start of the time series
#' available in the coverage.
#'
#'
#' @param    data.tb    tibble - input SITS table (useful for chaining functions)
#' @param    ref_date   date   - a reference date where all series will start
#' @return   data1.tb   tibble - the converted SITS table (useful for chaining functions)
#' @keywords STIS
#' @family   STIS main functions
#' @examples data1.tb <- sits_align (data = "mydata.tb", ref_date = "2000-09-02")
#' @export
#'
sits_align <- function (data, ref_date) {
     ts <- data$time_series
     # convert the time index to a reference year
     ts1 <- ts %>%
          map (function (t) {
               df <- as.data.frame(t)
               start_date <- as.Date(df[1,"Index"])
               if (abs (yday(start_date) - yday(ref_date)) > 2) {
                    print (start_date, ref_date)
                    message (paste ("sits_align: time series do not start at the same date"))
               }
               dplyr::mutate (t, Index = Index - ymd(start_date) + ymd (ref_date))
          })
     data$time_series <- ts1
     return (data)
}
