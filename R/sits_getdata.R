#' @title Obtain timeSeries from different sources
#' @name sits_getdata
#' @author Gilberto Camara
#'
#' @description reads descriptive information about a data source to retrive a set of
#' time series. The following options are available:
#' (a) The source is a SITS table - retrieves the metadata from the sits table and the data
#' from the WTSS service
#' (b) The source is a CSV file - retrieves the metadata from the CSV file and the data
#' from the WTSS service
#' (c) The source is a JSON file - retrieves the metadata and data from the JSON file.
#' (d) No source is given - it retrives the data based on <long, lat, wtss>
#' A sits table has the metadata and data for each time series
#' <longitude, latitude, start_date, end_date, label, coverage, time_series>
#'
#' A Web Time Series Service (WTSS) is a light-weight service that
#' retrieves one or more time series in JSON format from a data base.
#' @references
#' Lubia Vinhas, Gilberto Queiroz, Karine Ferreira, Gilberto Camara,
#' Web Services for Big Earth Observation Data.
#' In: XVII Brazilian Symposium on Geoinformatics, 2016, Campos do Jordao.
#' Proceedings of GeoInfo 2016. Sao Jose dos Campos: INPE/SBC, 2016. v.1. p.166-177.
#'
#' @param file            the name of a file with information on the data to be retrieved (options - CSV, JSON, SHP)
#' @param table           an R object ("sits_table")
#' @param longitude       double - the longitude of the chosen location
#' @param latitude        double - the latitude of the chosen location)
#' @param URL             string - the URL of WTSS (Web Time Series Service)
#' @param coverage        string - the name of the coverage to be retrieved
#' @param bands           string vector - the names of the bands to be retrieved
#' @param n_max           integer - the maximum number of samples to be read (optional)
#' @return data.tb        tibble  - a SITS table
#' @export
sits_getdata <- function (file        = NULL,
                          table       = NULL,
                          longitude   = NULL,
                          latitude    = NULL,
                          URL         = "http://www.dpi.inpe.br/tws/wtss",
                          coverage    = NULL,
                          bands       = NULL,
                          n_max       = Inf) {

     # a JSON file has all the data and metadata - no need to access the WTSS server
     if  (!purrr::is_null (file) && tolower(tools::file_ext(file)) == "json"){
          data.tb <- .sits_fromJSON (file)
          return (data.tb)
     }
     # Ensure that required inputs exist
     ensurer::ensure_that(coverage, !purrr::is_null (.), err_desc = "sits_getdata: Missing coverage name")
     ensurer::ensure_that(bands, !purrr::is_null (.), err_desc = "sits_getdata: Missing bands vector")
     ensurer::ensure_that(URL, !purrr::is_null (.), err_desc = "sits_getdata: Missing WTSS URL")

     # obtains an R object that represents the WTSS service
     wtss.obj <- wtss::WTSS(URL)

     #retrieve coverage information
     cov <- sits_getcovWTSS(URL, coverage)

     # get data based on latitude and longitude
     if (purrr::is_null (file) && purrr::is_null (table) && !purrr::is_null(latitude) && !purrr::is_null(longitude)) {
          data.tb <- .sits_fromlatlong (longitude, latitude, wtss.obj, cov, bands)
          return (data.tb)
     }
     # get data based on table
     if (!purrr::is_null (table)){
          data.tb <- .sits_fromtable (table, wtss.obj, cov, bands)
          return (data.tb)
     }

     # get data based on CSV file
     if (tolower(tools::file_ext(file)) == "csv") {
          data.tb <- .sits_fromCSV (file, wtss.obj, cov, bands, n_max)
          return (data.tb)
     }
     # get data based on SHP file
     if (tolower(tools::file_ext(file)) == "shp") {
          data.tb <- .sits_fromSHP (file, wtss.obj)
          return (data.tb)
     }
     message (paste ("No valid input to retrieve time series data!!","\n",sep=""))
     stop()
}

#' @title Obtain timeSeries from a JSON file.
#'
#' @name .sits_fromJSON
#'
#' @description reads a set of data and metadata for satellite image time series from a JSON file
#'
#' @param json_file  string  - name of a JSON file with sits data and metadata
#' @return data.tb    tibble  - a SITS table
.sits_fromJSON <- function (json_file) {
     # add the contents of the JSON file to a SITS table
     table <- tibble::as_tibble (jsonlite::fromJSON (json_file))
     # convert Indexes in time series to dates
     table1 <- sits_table()
     table %>%
          purrr::by_row(function (r) {
               tb <- tibble::as_tibble(r$time_series[[1]])
               tb$Index <- lubridate::as_date(tb$Index)
               r$time_series[[1]] <- tb
               r$start_date <- lubridate::as_date(r$start_date)
               r$end_date   <- lubridate::as_date(r$end_date)
               table1 <<- dplyr::bind_rows(table1, r)
          })
     return (table1)
}

#' @title Obtain timeSeries from WTSS server based on a lat/long information.
#' @name .sits_fromlatlong
#'
#' @description This function uses the lat/long location to retrive a time seriees
#' for a WTSS service. A Web Time Series Service is a light-weight service that
#' retrieves one or more time series in JSON format from a data base.
#' @references
#' Lubia Vinhas, Gilberto Queiroz, Karine Ferreira, Gilberto Camara,
#' Web Services for Big Earth Observation Data.
#' In: XVII Brazilian Symposium on Geoinformatics, 2016, Campos do Jordao.
#' Proceedings of GeoInfo 2016. Sao Jose dos Campos: INPE/SBC, 2016. v.1. p.166-177.

#' @param longitude       double - the longitude of the chosen location
#' @param latitude        double - the latitude of the chosen location)
#' @param wtss.obj       an R object that represents the WTSS server
#' @param cov            a list with the coverage parameters (retrived from the WTSS server)
#' @param bands          string vector - the names of the bands to be retrieved
#' @return data.tb       tibble  - a SITS table
.sits_fromlatlong <-  function (longitude, latitude, wtss.obj, cov, bands) {

     # set the start and end dates from the coverage
     start_date <- lubridate::as_date(cov$timeline[1])
     end_date <- lubridate::as_date(cov$timeline[length(cov$timeline)])
     # set the class of the time series
     label <-  "NoClass"
     # use the WTSS service to retrieve the time series
     data.tb <- .sits_fromWTSS (longitude, latitude, start_date, end_date, label, wtss.obj, cov, bands)
     return (data.tb)
}

#' @title Obtain timeSeries from WTSS server, based on a SITS table.
#' @name .sits_fromtable
#'
#' @description reads descriptive information about a set of
#' spatio-temporal locations from a SITS table. Then it uses the WTSS service to
#' obtain the required data. This function is useful when you have a sits table
#' but you want to get the time series from a different set of bands.
#'
#' @param table          a  sits_table
#' @param wtss.obj       an R object that represents the WTSS server
#' @param cov            a list with the coverage parameters (retrived from the WTSS server)
#' @param bands          string vector - the names of the bands to be retrieved
#' @return data.tb       tibble  - a SITS table
.sits_fromtable <-  function (table, wtss.obj, cov, bands) {
     # create the table to store
     data.tb <- sits_table()

     table %>%
          purrr::by_row( function (r){
               # does the lat/long information exist
               ensurer::ensure_that(r$longitude, !purrr::is_null(.) && !is.na(.), err_desc = "sits_getdata - no longitude information")
               ensurer::ensure_that(r$latitude,  !purrr::is_null(.) && !is.na(.), err_desc = "sits_getdata - no longitude information")

               # ajust the start and end dates and the label
               if (is.na(r$start_date)) { r$start_date <- lubridate::as_date(cov$timeline[1])}
               if (is.na(r$end_date))   { r$end_date   <- lubridate::as_date(cov$timeline[length(cov$timeline)])}
               if (is.na(r$label))      { r$label <- "NoClass"}

               # retrieve the data row
               t <- .sits_fromWTSS (r$longitude, r$latitude, r$start_date, r$end_date,
                                    r$label, wtss.obj, cov, bands)

               # add the row to the output
               data.tb <<- dplyr::bind_rows (data.tb, t)
          })
     return (data.tb)
}

#' @title Obtain timeSeries from WTSS server, based on a CSV file.
#' @name .sits_fromCSV
#'
#' @description reads descriptive information about a set of
#' spatio-temporal locations from a CSV file. Then, it uses the WTSS time series service
#' to retrieve the time series, and stores the time series on a SITS table for later use.
#' The CSV file should have the following column names:
#' "longitude", "latitude", "start_date", "end_date", "label"
#'
#' @param csv_file        string  - name of a CSV file with information <id, latitude, longitude, from, end, label>
#' @param wtss.obj        WTSS object - the WTSS object that describes the WTSS server
#' @param cov             list - a list with coverage information (retrieved from the WTSS)
#' @param bands           string vector - the names of the bands to be retrieved
#' @param n_max           integer - the maximum number of samples to be read
#' @return data.tb        tibble  - a SITS table

.sits_fromCSV <-  function (csv_file, wtss.obj, cov, bands, n_max = Inf){
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
     csv.tb %>%
          purrr::by_row( function (r){
               row <- .sits_fromWTSS (r$longitude, r$latitude, r$start_date, r$end_date, r$label, wtss.obj, cov, bands)
               data.tb <<- dplyr::bind_rows (data.tb, row)
          })
     return (data.tb)
}
#' @title Obtain timeSeries from WTSS server, based on a SHP file.
#' @name .sits_getdata_fromSHP
#'
#' @description reads a shapefile and retrieves a SITS table
#' containing time series from a coverage that are inside the SHP file.
#' The script uses the WTSS service, taking information about coverage, spatial and
#' temporal resolution from the WTSS configuration.
#'
#' @param shp_file   string  - name of a SHP file which provides the boundaries of a region of interest
#' @param wtss       an R object that represents the WTSS server
#' @return table     tibble  - a SITS table
.sits_fromSHP <- function (shp_file, wtss) {
     # read the shapefile
     area_shp <- raster::shapefile(shp_file)

     # build grid points in Sinusoidal
     buildGridPoints <- function(points_Sinu.sp) {

          # pixel size Sinusoidal according to the gdalinfo
          # this should be changed to use information from "describeCoverage" (ATTENTION)
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
                                              proj4string=sp::CRS(sp::proj4string(points_Sinu.sp)))

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
                                          proj4string=sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

          points.sp

     }

     # extract latitude and longitude values of sits points from polygon
     extractFromPolygon <- function(points.sp, roi.shp) {

          # subset of points.sp
          points.sp <- points.sp[!is.na(sp::over(points.sp, methods::as(roi.shp, "SpatialPolygons")) == 1)]

          points.sp

     }

     # read shapefile and get first point time series
     roi.shp <- raster::shapefile(shp_file)
     roi.shp <- sp::spTransform(roi.shp, sp::CRS("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"))
     pt.df <- data.frame(longitude = sp::bbox(roi.shp)[1,], latitude = sp::bbox(roi.shp)[2,])
     pt.sp <- sp::SpatialPoints(pt.df, sp::CRS(sp::proj4string(roi.shp)))
     pt.sp <- sp::spTransform(pt.sp, sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
     pt.tb <- sp2SitsTable(pt.sp)
     pt_wtss.tb <- pt.tb %>%
          dplyr::rowwise() %>%
          dplyr::do (.sits_fromWTSS (.$longitude, .$latitude, wtss$start_date, wtss$end_date, "NoClass", wtss$wtss_obj, wtss$coverage, wtss$bands))

     # transform grid points in Sinusoidal into WGS
     pt_wtss.sp <- sitsTable2sp(pt_wtss.tb)
     pt_wtss.sp <- sp::spTransform(pt_wtss.sp, sp::CRS("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"))
     grid_pts.sp <- buildGridPoints(pt_wtss.sp)

     # transform WGS sp into sits table to get data from server
     plg_pts.sp <- extractFromPolygon(grid_pts.sp, roi.shp)
     plg_pts.sp <- sp::spTransform(plg_pts.sp, sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
     plg_pts.tb <- sp2SitsTable(plg_pts.sp)

     wtss_points.tb <- plg_pts.tb %>%
          dplyr::rowwise() %>%
          dplyr::do (.sits_fromWTSS (.$longitude, .$latitude, wtss$start_date, wtss$end_date, "NoClass", wtss$wtss_obj, wtss$coverage, wtss$bands))

     return (wtss_points.tb)
}

#' @title Obtain one timeSeries from WTSS server and load it on a sits table
#' @name .sits_fromWTSS
#'
#' @description Returns one set of time series provided by a WTSS server
#' Given a location (lat/long), and start/end period, and the WTSS server information
#' retrieve a time series and include it on a stis table.
#' A Web Time Series Service (WTSS) is a light-weight service that
#' retrieves one or more time series in JSON format from a data base.
#' @references
#' Lubia Vinhas, Gilberto Queiroz, Karine Ferreira, Gilberto Camara,
#' Web Services for Big Earth Observation Data.
#' In: XVII Brazilian Symposium on Geoinformatics, 2016, Campos do Jordao.
#' Proceedings of GeoInfo 2016. Sao Jose dos Campos: INPE/SBC, 2016. v.1. p.166-177
#'
#' @param longitude       double - the longitude of the chosen location
#' @param latitude        double - the latitude of the chosen location
#' @param start_date      date - the start of the period
#' @param end_date        date - the end of the period
#' @param label           string - the label to attach to the time series (optional)
#' @param wtss.obj        an R object that represents the WTSS server
#' @param cov             a list containing information about the coverage from which data is to be recovered
#' @param bands           list of string - a list of the names of the bands of the coverage
#' @return data.tb        tibble  - a SITS table
.sits_fromWTSS <- function (longitude, latitude, start_date, end_date, label, wtss.obj, cov, bands) {

     # get a time series from the WTSS server
     ts <- wtss::timeSeries (wtss.obj, coverages = cov$name, attributes = bands,
                               longitude = longitude, latitude = latitude,
                               start = start_date, end = end_date)

     # retrieve the time series information
     time_series <- ts[[cov$name]]$attributes

     # retrieve information about the bands
     band_info <- cov$attributes

     # determine the missing value for each band
     miss_value <- function (band) {
          return (band_info[which(band == band_info[,"name"]),"missing_value"])
     }
     # update missing values to NA
     for (b in bands){
          time_series[,b][time_series[,b] == miss_value(b)] <- NA
     }

     # interpolate missing values
     time_series[,bands] <- zoo::na.spline(time_series[,bands])

     # calculate the scale factor for each band
     scale_factor <- function (band){
          return (band_info[which(band == band_info[,"name"]),"scale_factor"])
     }
     # scale the time series
     bands %>%
          purrr::map (function (b) {
               time_series[,b] <<- time_series[,b]*scale_factor(b)
          })

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
                         coverage     = cov$name,
                         time_series  = ts.lst
     )

     # return the table with the time series
     return (data.tb)
}
