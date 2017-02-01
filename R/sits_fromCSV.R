#------------------------------------------------------------------
#' Obtain timeSeries from WTSS server, based on a CSV file.
#'
#' \code{sits_fromCSV} reads descriptive information about a set of
#' spatio-temporal locations from a CSV file. Then, it uses the WTSS time series service
#' to retrieve the time series, and stores the time series on a SITS table for later use.
#'
#' #' The CSV file should have the following column names:
#' "longitude", "latitude", "from", "to", "label"
#'
#' A sits table has the metadata and data for each time series
#' <longitude, latitude, from, to, label, coverage, time_series>
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
     # read sample information from CSV file
     cols_csv <- cols(id        = col_integer(),
                      longitude = col_double(),
                      latitude  = col_double(),
                      from      = col_date(),
                      to        = col_date(),
                      label     = col_character())
     samples.df <- read_csv (csv_file, n_max = n_max, col_types = cols_csv)
     # does the table exist? if not, it is created
     samples.tb <- sits_table()
     # retrieve the time series for WTSS and add it to the table
     for (i in 1:nrow (samples.df)) {
          row <- sits_fromWTSS (samples.df[i,]$longitude,
                                samples.df[i,]$latitude,
                                samples.df[i,]$from,
                                samples.df[i,]$to,
                                samples.df[i,]$label)
          samples.tb <- bind_rows (samples.tb, row)
     }
     return (samples.tb)
}
