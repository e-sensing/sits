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
#' (d) No source is given - it retrives the data based on <long, lat, start_date, end_date,
#'
#' A sits table has the metadata and data for each time series
#' <longitude, latitude, start_date, end_date, label, coverage, time_series>
#'
#' @param source          string  - name of a source CSV file with information <id, latitude, longitude, from, end, label>
#' @param longitude       double - the longitude of the chosen location
#' @param latitude        double - the latitude of the chosen location
#' @param start_date      date - the start of the period
#' @param end_date        date - the end of the period
#' @param label           string - the label to attach to the time series (optional)
#' @param n_max           integer - the maximum number of samples to be read (optional)
#' @return table     tibble  - a SITS table
#' @keywords SITS
#' @family   SITS main functions
#' @examples
#'       sits_getdata (source = "mysamples.csv", n_max = 5)
#'       sits_getdata (source = "mydata.json")
#'       sits_getdata (source = mytable.tb)
#'       sits_getdata (longitude = -53.27, latitude = -12.23)
#' @export

sits_getdata <- function (source = NULL,
                          longitude  =          -55.51810,
                          latitude   =          -11.63884,
                          start_date =     start_date.gl,
                          end_date   =       end_date.gl,
                          label      =         "NoClass",
                          coverage   =       cov_name.gl,
                          bands      =          bands.gl,
                          crs        = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs",
                          n_max = Inf) {

     if (is.null (source)) {
          data.tb <- .sits_fromWTSS(longitude, latitude, start_date, end_date,
                                    label, coverage, bands)
          return (data.tb)
     }
     if ("sits_table" %in% class (source) || "tbl" %in% class (source) ){
          data.tb <- .sits_getdata_from_table (input.tb = source, bands = bands)
          return (data.tb)
     }
     if (tools::file_ext(source) == "csv" || tools::file_ext(source) == "CSV"){
          data.tb <- .sits_getdata_fromCSV (csv_file = source, coverage = coverage,
                                            bands = bands, n_max = n_max)
          return (data.tb)
     }
     if (tools::file_ext(source) == "json" || tools::file_ext(source) == "JSON"){
          data.tb <- .sits_getdata_fromJSON (json_file = source)
          return (data.tb)
     }
     if (tools::file_ext(source) == "shp" || tools::file_ext(source) == "SHP"){
          data.tb <- .sits_getdata_fromSHP (shp_file = source, crs = crs)
          return (data.tb)
     }
     message (paste ("No valid input to retrieve time series data!!","\n",sep=""))
     stop(cond)
}


.sits_getdata_from_table <-  function (input.tb, bands) {

     # create the table
     data.tb <- sits_table()
     # for each row of the input, retrieve the time series
     data.tb <- input.tb %>%
          dplyr::rowwise() %>%
          dplyr::do (.sits_fromWTSS (.$longitude, .$latitude, .$start_date, .$end_date, .$label, .$coverage, bands)) %>%
          dplyr::bind_rows (data.tb, .)
     return (data.tb)
}

.sits_getdata_fromJSON <- function (json_file) {

     # add the contents of the JSON file to a SITS table
     table <- as_tibble (jsonlite::fromJSON (json_file))
     # convert Indexes in time series to dates
     for (i in 1:nrow(table)) {
          tb <- as_tibble(table[i,]$time_series[[1]])
          tb$Index <- as_date(tb$Index)
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
#' \code{sits_fromCSV} reads descriptive information about a set of
#' spatio-temporal locations from a CSV file. Then, it uses the WTSS time series service
#' to retrieve the time series, and stores the time series on a SITS table for later use.
#'
#' #' The CSV file should have the following column names:
#' "longitude", "latitude", "start_date", "end_date", "label"
#'
#'
#' @param csv_file   string  - name of a CSV file with information <id, latitude, longitude, from, end, label>
#' @param n_max      integer - the maximum number of samples to be read
#' @param bands      vector  - the names of the bands to be read

.sits_getdata_fromCSV <-  function (csv_file, coverage, bands, n_max = Inf){

     # configure the format of the CSV file to be read
     cols_csv <- readr::cols(id          = col_integer(),
                             longitude   = col_double(),
                             latitude    = col_double(),
                             start_date  = col_date(),   end_date    = col_date(),
                             label       = col_character())
     # read sample information from CSV file and put it in a tibble
     csv.tb <- readr::read_csv (csv_file, n_max = n_max, col_types = cols_csv)
     # create the table
     data.tb <- sits_table()
     # for each row of the input, retrieve the time series
     data.tb <- csv.tb %>%
          dplyr::rowwise() %>%
          dplyr::do (.sits_fromWTSS (.$longitude, .$latitude, .$start_date, .$end_date, .$label, coverage, bands)) %>%
          dplyr::bind_rows (data.tb, .)
     return (data.tb)
}

#' Obtain one timeSeries from WTSS server and load it on a sits table
#'
#' \code{sits_fromWTSS} returns one set of time series provided by a WTSS server
#'
#' Given a location (lat/long), and start/end period, and the WTSS server information
#' retrieve a time series and include it on a stis table

.sits_fromWTSS <- function  (longitude  =         -55.51810,
                             latitude   =          -11.63884,
                             start_date =     start_date.gl,
                             end_date   =       end_date.gl,
                             label      =         "NoClass",
                             coverage   =       cov_name.gl,
                             bands      =          bands.gl) {

     # is the WTSS service running?
     sits_testWTSS()
     # get a time series from the WTSS server
     ts <- timeSeries (ts_server.gl,
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
     row.tb <- as_tibble (fortify.zoo (time_series))
     # clean the time series
     # create a list to store the zoo time series coming from the WTSS service
     ts.lst <- list()
     # transform the zoo list into a tibble to store in memory
     ts.lst[[1]] <- row.tb

     # create a table to store the WTSS data
     data.tb <- sits_table()
     # add one row to the table
     data.tb <- add_row (data.tb,
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
#------------------------------------------------------------------
#' Obtain timeSeries from WTSS server, based on a SHP file.
#'
#' \code{sits_fromSHP} reads a shapefile and retrieves a SITS table
#' containing time series from a coverage that are inside the SHP file.
#' The script uses the WTSS service, taking information about coverage, spatial and
#' temporal resolution from the WTSS configuration.
#'
#'
#'
#' @param shp_file   string  - name of a SHP file which provides the boundaries of a region of interest
#' @param crs        string  - the coordinate reference system used by the shapefile
#' @return table     tibble  - a SITS table
#' @keywords SITS
#' @family   SITS data input functions
#' @examples sits_fromSHP ("municipality.shp")
#'
.sits_fromSHP <- function (shp_file, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") {

     # is the WTSS service working?
     sits_testWTSS()

     create_polygon <- function (file, crs) {

          # read shapefile from a full path
          area_shp <- shapefile::read.shapefile(file)

          # create spatial polygons from polygon
          polygon <- sp::Polygon(area_shp$shp$shp[[1]]$points)
          polygons <- sp::Polygons(list(polygon), paste(1))
          spatial.polygons <- sp::SpatialPolygons(list(polygons))

          # define CRS of the spatial polygon
          proj4string(spatial.polygons) <- sp::CRS(crs)

          # return spatial.polygons
          return (spatial.polygons)

     }
     # What points are inside a given polygon?
     inside_polygon <- function(coord.df, plg, crs) {

          # create a spatial points data frame with the same CRS as the SHP polygon
          pts.df <- sp::SpatialPoints(coord.df, CRS(crs))

          # find the coordinates of the data frame that are inside the polygon
          coord.df <- subset(coord.df, !is.na(sp::over(pts.df, plg) == 1))
          # return the coordinates that are inside the polygon
          return (coord.df)

     }
     # create a point grid of coverage pixels that are inside the polygon
     create_grid <- function(plg) {

          # Para MOD13Q1 use pixelsize = 231.6564
          # center coordinates resolution
          resolution = 0.0019

          # get first point from the polygon bounding box boundaries
          minimum.bb = bbox(plg)
          first.point <- getTimeSeries(minimum.bb, dates, class)

          # define list of latitudes and longitudes by resolution based on the bounding box
          list.long <- seq(from=first.point$longitude, to=minimum.bb[3], by=resolution)
          list.lat <- seq(from=first.point$latitude, to=minimum.bb[4], by=resolution)

          # build the latitude and longitude
          df.coordinates <- data.frame(longitude = rep(list.long, each=length(list.lat)), latitude = rep(list.lat, length(list.long)))

          df.coordinates

     }

}
