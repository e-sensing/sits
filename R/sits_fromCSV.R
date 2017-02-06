#------------------------------------------------------------------
#' Obtain timeSeries from WTSS server, based on a CSV file.
#'
#' \code{sits_fromCSV} reads descriptive information about a set of
#' spatio-temporal locations from a CSV file. Then, it uses the WTSS time series service
#' to retrieve the time series, and stores the time series on a SITS table for later use.
#'
#' #' The CSV file should have the following column names:
#' "longitude", "latitude", "start_date", "end_date", "label"
#'
#' A sits table has the metadata and data for each time series
#' <longitude, latitude, start_date, end_date, label, coverage, time_series>
#'
#'
#' @param csv_file   string  - name of a CSV file with information <id, latitude, longitude, from, end, label>
#' @param n_max      integer - the maximum number of samples to be read
#' @return table     tibble  - a SITS table
#' @keywords SITS
#' @family   SITS main functions
#' @examples sits_fromCSV ("mysamples.csv", n_max = 5)
#' @export

sits_fromCSV <-  function (csv_file, n_max = Inf){

     # configure the format of the CSV file to be read
     cols_csv <- readr::cols(id          = col_integer(),
                             longitude   = col_double(),
                             latitude    = col_double(),
                             start_date  = col_date(),
                             end_date    = col_date(),
                             label       = col_character())
     # read sample information from CSV file
     samples.df <- readr::read_csv (csv_file, n_max = n_max, col_types = cols_csv)
     # create the table
     samples.tb <- sits_table()
     # retrieve the time series for WTSS and add it to the table
     for (i in 1:nrow (samples.df)) {
          row <- sits_fromWTSS (samples.df[i,]$longitude,
                                samples.df[i,]$latitude,
                                samples.df[i,]$start_date,
                                samples.df[i,]$end_date,
                                samples.df[i,]$label)
          samples.tb <- dplyr::bind_rows (samples.tb, row)
     }
     return (samples.tb)
}
