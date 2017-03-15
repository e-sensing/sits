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
     if (tools::file_ext(file) == "json" || tools::file_ext(file) == "JSON"){
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
     # for each row of the input, retrieve the time series
     data.tb <- source %>%
          dplyr::rowwise() %>%
          dplyr::do (.sits_fromWTSS (.$longitude, .$latitude, .$start_date, .$end_date, .$label, wtss$wtss_obj, wtss$coverage, wtss$bands)) %>%
          dplyr::bind_rows (data.tb, .)
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

     # build grid points in Sinusoidal
     buildGridPoints <- function(points_Sinu.sp) {

          # pixel size Sinusoidal according to the gdalinfo
          pixel_size_Sinu <- 231.656358263958

          # bounding box of sinusoidal points
          bb_Sinu.num <- sp::bbox(points_Sinu.sp)

          # define coordinates by resolution according to the bounding box + one pixel size
          long.num <- seq(from=bb_Sinu.num[1],
                          to=bb_Sinu.num[3],
                          by=pixel_size_Sinu)

          lat.num <- seq(from=bb_Sinu.num[2],
                         to=bb_Sinu.num[4],
                         by=pixel_size_Sinu)

          # build the latitude and longitude
          coordinates.sp <- sp::SpatialPoints(data.frame(longitude = rep(long.num,
                                                                         each=length(lat.num)),
                                                         latitude = rep(lat.num,
                                                                        length(long.num))),
                                              proj4string=CRS(proj4string(points_Sinu.sp)))

          coordinates.sp

     }

     # spatial points to sits table
     sp2SitsTable <- function(coordinates.sp){

          # using the WTSS
          sits_points.tb <- sits_table()

          # fill rows with coordinates
          sits_points.tb <- tibble::add_row(sits_points.tb,
                                            longitude = coordinates.sp@coords[,1],
                                            latitude = coordinates.sp@coords[,2])

          sits_points.tb

     }

     # sitsTable longitude and latitude to crs shapefile
     sitsTable2sp <- function(points.tb){

          points.sp <-  sp::SpatialPoints(data.frame(points.tb$longitude,
                                                     points.tb$latitude),
                                          proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

          points.sp

     }

     # extract latitude and longitude values of sits points from polygon
     extractFromPolygon <- function(points.sp, roi.shp) {

          # subset of points.sp
          points.sp <- points.sp[!is.na(sp::over(points.sp, as(roi.shp, "SpatialPolygons")) == 1)]

          points.sp

     }

     # read shapefile and get first point time series
     roi.shp <- raster::shapefile(shp_file)
     roi.shp <- sp::spTransform(roi.shp, CRS("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"))
     pt.df <- data.frame(longitude = bbox(roi.shp)[1,], latitude = bbox(roi.shp)[2,])
     pt.sp <- sp::SpatialPoints(pt.df, CRS(proj4string(roi.shp)))
     pt.sp <- sp::spTransform(pt.sp, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
     pt.tb <- sp2SitsTable(pt.sp)
     pt_wtss.tb <- pt.tb %>%
          dplyr::rowwise() %>%
          dplyr::do (.sits_fromWTSS (.$longitude, .$latitude, wtss$start_date, wtss$end_date, "NoClass", wtss$wtss_obj, wtss$coverage, wtss$bands))

     # transform grid points in Sinusoidal into WGS
     pt_wtss.sp <- sitsTable2sp(pt_wtss.tb)
     pt_wtss.sp <- sp::spTransform(pt_wtss.sp, CRS("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"))
     grid_pts.sp <- buildGridPoints(pt_wtss.sp)

     # transform WGS sp into sits table to get data from server
     plg_pts.sp <- extractFromPolygon(grid_pts.sp, roi.shp)
     plg_pts.sp <- sp::spTransform(plg_pts.sp, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
     plg_pts.tb <- sp2SitsTable(plg_pts.sp)

     wtss_points.tb <- plg_pts.tb %>%
          dplyr::rowwise() %>%
          dplyr::do (.sits_fromWTSS (.$longitude, .$latitude, wtss$start_date, wtss$end_date, "NoClass", wtss$wtss_obj, wtss$coverage, wtss$bands))

     wtss_points.tb
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

