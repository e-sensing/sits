#------------------------------------------------------------------
#' Obtain timeSeries
#'
#' \code{sits_getdata} reads descriptive information about a data source to retrive a set of
#' time series. The following options are available:
#'
#' (a) The source is a SITS table - retrieves the metadata from the sits table and the data
#' from the WTSS service
#' (b) The source is a CSV file - retrieves the metadata from the CSV file and the data
#' from the WTSS service
#' (c) The source is a JSON file - retrieves the metadata and data from the JSON file.
#' (d) No source is given - it retrives the data based on <long, lat, wtss>
#' A sits table has the metadata and data for each time series
#' <longitude, latitude, start_date, end_date, label, coverage, time_series>
#'
#' @param file            the name of a file with information on the data to be retrieved (options - CSV, JSON, SHP)
#' @param table           an R object ("sits_table")
#' @param longitude       double - the longitude of the chosen location
#' @param latitude        double - the latitude of the chosen location)
#' @param wtss            WTSS configuration - information that describes the WTSS service (given by sits_configWTSS)
#' @param n_max           integer - the maximum number of samples to be read (optional)
#' @return data.tb        tibble  - a SITS table
#' @keywords SITS
#' @family   SITS data functions
#' @export

sits_getdata <- function (file        = NULL,
                          table       = NULL,
                          longitude   = -55.51810,
                          latitude    = -11.63884,
                          wtss        = NULL,
                          n_max       = Inf) {

     if (purrr::is_null (file) && purrr::is_null (table) && !purrr::is_null (wtss)) {
          label <- "NoClass"
          data.tb <- .sits_fromWTSS (longitude,
                                     latitude,
                                     wtss$start_date,
                                     wtss$end_date,
                                     label,
                                     wtss$wtss_obj,
                                     wtss$coverage,
                                     wtss$bands)
          return (data.tb)
     }
     if (!purrr::is_null (table) && !purrr::is_null (wtss)){
          data.tb <- .sits_getdata_from_table (table, wtss)
          return (data.tb)
     }
     if ((tools::file_ext(file) == "csv" || tools::file_ext(file) == "CSV") && !purrr::is_null (wtss)) {
          data.tb <- .sits_getdata_fromCSV (file, wtss, n_max)
          return (data.tb)
     }
     if (tools::file_ext(file) == "json" || tools::file_ext(source) == "JSON"){
          data.tb <- .sits_getdata_fromJSON (file)
          return (data.tb)
     }
     if ((tools::file_ext(file) == "shp" || tools::file_ext(file) == "SHP") && !purrr::is_null (wtss)){
          data.tb <- .sits_getdata_fromSHP (file, wtss)
          return (data.tb)
     }
     message (paste ("No valid input to retrieve time series data!!","\n",sep=""))
     stop(cond)
}
#------------------------------------------------------------------
#' Obtain timeSeries from WTSS server, based on a WTSS configuration.
#'
#' \code{.sitsgetdata_fromWTSS_config} reads configuration information about
#' the WTSS services from a wtss object passed as parameters. Then it uses the WTSS configuration
#' to call the atomic function .sits_getdata_fromWTSS to
#' obtain the required data.
#'
#' @param longitude      the longitude of the location of the time series
#' @param latitude       the latitude of the location of the time series
#' @param wtss           WTSS object - the WTSS object that describes the WTSS server
#' @return data.tb       tibble  - a SITS table
.sits_getdata_fromWTSS_config <-  function (latitude, longitude, wtss) {
     # create the table
     data.tb <- sits_table()
     label <- "NoClass"
     # for each row of the input, retrieve the time series
     data.tb <- .sits_fromWTSS (longitude, latitude, wtss$start_date, wtss$end_date, label,
                                wtss$wtss_obj, wtss$coverage, wtss$bands)
     return (data.tb)
}
#------------------------------------------------------------------
#' Obtain timeSeries from WTSS server, based on a SITS table.
#'
#' \code{.sits_getdata_from_table} reads descriptive information about a set of
#' spatio-temporal locations from a SITS table. Then it uses the WTSS service to
#' obtain the required data. This function is useful when you have a sits table
#' but you want to get the time series from a different set of bands.
#'
#'
#' @param table          an R object (type "sits_table")
#' @param wtss           WTSS object - the WTSS object that describes the WTSS server
#' @return data.tb       tibble  - a SITS table
.sits_getdata_from_table <-  function (source, wtss) {
     # create the table
     data.tb <- sits_table()

     for (i in 1:nrow(source)){
          row <- source[i,]
          if (is.na(row$start_date)) {row$start_date <- lubridate::as_date(wtss$start_date)}
          if (is.na(row$end_date)) { row$end_date <- lubridate::as_date(wtss$end_date)}
          if (is.na(row$label)) {row$label <- "NoClass"}
          t <- .sits_fromWTSS (row$longitude, row$latitude, row$start_date, row$end_date,
                                  row$label, wtss$wtss_obj, wtss$coverage, wtss$bands)
          data.tb <- dplyr::bind_rows (data.tb, t)
     }
     return (data.tb)
}
#------------------------------------------------------------------
#' Obtain timeSeries from a JSON file.
#'
#' \code{.sits_getdata_fromJSON} reads a set of data and metadata for satellite
#' image time series from a JSON file
#'
#'
#' @param json_file  string  - name of a JSON file with sits data and metadata
#' @return data.tb    tibble  - a SITS table
.sits_getdata_fromJSON <- function (json_file) {
     # add the contents of the JSON file to a SITS table
     table <- tibble::as_tibble (jsonlite::fromJSON (json_file))
     # convert Indexes in time series to dates
     for (i in 1:nrow(table)) {
          tb <- tibble::as_tibble(table[i,]$time_series[[1]])
          tb$Index <- lubridate::as_date(tb$Index)
          table[i,]$time_series[[1]] <- tb
     }
     # convert start and end date to Date format
     table <- dplyr::mutate (table, start_date = as.Date(start_date))
     table <- dplyr::mutate (table, end_date   = as.Date(end_date))
     return (table)
}

#------------------------------------------------------------------
#' Obtain timeSeries from WTSS server, based on a CSV file.
#'
#' \code{.sits_getdata_fromCSV} reads descriptive information about a set of
#' spatio-temporal locations from a CSV file. Then, it uses the WTSS time series service
#' to retrieve the time series, and stores the time series on a SITS table for later use.
#'
#' #' The CSV file should have the following column names:
#' "longitude", "latitude", "start_date", "end_date", "label"
#'
#' @param csv_file        string  - name of a CSV file with information <id, latitude, longitude, from, end, label>
#' @param wtss_server     WTSS object - the WTSS object that describes the WTSS server
#' @param coverage        string - the name of the coverage from which data is to be recovered
#' @param bands           list of string - a list of the names of the bands of the coverage
#' @param n_max           integer - the maximum number of samples to be read
#' @return data.tb        tibble  - a SITS table

.sits_getdata_fromCSV <-  function (csv_file, wtss, n_max = Inf){
     # configure the format of the CSV file to be read
     cols_csv <- readr::cols(id          = readr::col_integer(),
                             longitude   = readr::col_double(),
                             latitude    = readr::col_double(),
                             start_date  = readr::col_date(),
                             end_date    = readr::col_date(),
                             label       = readr::col_character())
     # read sample information from CSV file and put it in a tibble
     csv.tb <- readr::read_csv (csv_file, n_max = n_max, col_types = cols_csv)
     # create the table
     data.tb <- sits_table()
     # for each row of the input, retrieve the time series
     data.tb <- csv.tb %>%
          dplyr::rowwise() %>%
          dplyr::do (.sits_fromWTSS (.$longitude, .$latitude, .$start_date, .$end_date, .$label, wtss$wtss_obj, wtss$coverage, wtss$bands)) %>%
          dplyr::bind_rows (data.tb, .)
     return (data.tb)
}
#------------------------------------------------------------------
#' Obtain timeSeries from WTSS server, based on a SHP file.
#'
#' \code{.sits_getdata_fromSHP} reads a shapefile and retrieves a SITS table
#' containing time series from a coverage that are inside the SHP file.
#' The script uses the WTSS service, taking information about coverage, spatial and
#' temporal resolution from the WTSS configuration.
#'
#' @param shp_file   string  - name of a SHP file which provides the boundaries of a region of interest
#' @param crs        string  - the coordinate reference system used by the shapefile
#' @return table     tibble  - a SITS table
#' @keywords SITS
#' @family   SITS data input functions
#' @examples sits_fromSHP (file = "municipality.shp", wtss = "my_wtss.info")
#'
.sits_getdata_fromSHP <- function (shp_file, wtss) {
     # read the shapefile
     area_shp <- raster::shapefile(shp_file)

     # get the box
     box <- area_shp@bbox
     # get a time series from the WTSS server
     ts.lst <- wtss.R::timeSeries (wtss$URL,
                                   coverages  = wtss$coverage,
                                   attributes = wtss$bands,
                                   box        = box,
                                   start      = wtss$start_date,
                                   end        = wtss$end_date)
     return (ts.lst)
}
#' Obtain one timeSeries from WTSS server and load it on a sits table
#'
#' \code{.sits_fromWTSS} returns one set of time series provided by a WTSS server
#'
#' Given a location (lat/long), and start/end period, and the WTSS server information
#' retrieve a time series and include it on a stis table
#' @param longitude       double - the longitude of the chosen location
#' @param latitude        double - the latitude of the chosen location
#' @param start_date      date - the start of the period
#' @param end_date        date - the end of the period
#' @param label           string - the label to attach to the time series (optional)
#' @param wtss_server     WTSS object - the WTSS object that describes the WTSS server
#' @param coverage        string - the name of the coverage from which data is to be recovered
#' @param bands           list of string - a list of the names of the bands of the coverage
#' @return data.tb        tibble  - a SITS table

.sits_fromWTSS <- function (longitude,
                            latitude,
                            start_date,
                            end_date,
                            label,
                            wtss_obj,
                            coverage,
                            bands) {

     # get a time series from the WTSS server
     ts <- wtss.R::timeSeries (wtss_obj,
                       coverages  = coverage,
                       attributes = bands,
                       longitude  = longitude,
                       latitude   = latitude,
                       start      = start_date,
                       end        = end_date
     )

     # retrieve the time series information
     time_series <- ts[[coverage]]$attributes

     # scale the time series
     time_series[,bands] <-  time_series[,bands]*0.0001

     # convert the series to a tibble
     row.tb <- tibble::as_tibble (zoo::fortify.zoo (time_series))
     # clean the time series
     # create a list to store the zoo time series coming from the WTSS service
     ts.lst <- list()
     # transform the zoo list into a tibble to store in memory
     ts.lst[[1]] <- row.tb

     # create a table to store the WTSS data
     data.tb <- sits_table()
     # add one row to the table
     data.tb <- tibble::add_row (data.tb,
                         longitude    = longitude,
                         latitude     = latitude,
                         start_date   = as.Date(start_date),
                         end_date     = as.Date(end_date),
                         label        = label,
                         coverage     = coverage,
                         time_series  = ts.lst
     )

     # return the table with the time series
     return (data.tb)
}

