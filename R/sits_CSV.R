#------------------------------------------------------------------
#' Obtain timeSeries from WTSS server, based on a CSV file.
#'
#' \code{sits_fromCSV} returns a list of WTSS time series from a sample data frame and
#' stores them in a global table for later use
#' A sits table has the metadata and data for each time series
#' <longitude, latitude, from, to, lable, time_series>
#'
#' The CSV file should have the following column names:
#' "longitude", "latitude", "from", "to", "label"
#' Longitude/latitude data is mandatory, the other parameters are optional
#'
#' @param csv_file   string  - name of a CSV file with information <id, latitude, longitude, from, end, label>
#' @param n_max      integer - the maximum number of samples to be read
#' @return table     the SITS table
#' @return an updated version of the global database
#' @keywords SITS
#' @family   SITS auxiliary functions
#' @examples sits_fromCSV ("mysamples.csv", table = "samples.db")
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
     # retrieve and scale the time series
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
