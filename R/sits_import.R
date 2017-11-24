#' @title Obtain timeSeries from different sources
#' @name sits_getdata
#' @author Gilberto Camara
#'
#' @description Reads descriptive information about a data source to retrieve a set of
#' time series. The following options are available:
#' (a) The source is a SITS tibble - retrieves the metadata from the sits table and the data
#' from the WTSS service
#' (b) The source is a CSV file - retrieves the metadata from the CSV file and the data
#' from the WTSS service
#' (c) The source is a SHP file - retrives all points inside the shapefile from the WTSS service
#' (d) The source is a JSON file - retrieves the metadata and data from the JSON file.
#' (e) The source is a gz file (compressed JSON file) - retrieves the metadata and data from the compressed JSON file.
#' (f) The source is a RasterBrick - retrieves the point based on lat/long from the RasterBrick
#' (g) No source is given - it retrieves the data based on <long, lat, wtss>
#' A sits tibble has the metadata and data for each time series
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
#' @param raster.tb       an STRaster  object (tibble with raster information)
#' @param file            the name of a file with information on the data to be retrieved (options - CSV, JSON, SHP)
#' @param longitude       double - the longitude of the chosen location
#' @param latitude        double - the latitude of the chosen location)
#' @param start_date      date - the start of the period
#' @param end_date        date - the end of the period
#' @param label           string - the label to attach to the time series
#' @param URL             string - the URL of WTSS (Web Time Series Service)
#' @param coverage        string - the name of the coverage to be retrieved
#' @param bands           string vector - the names of the bands to be retrieved
#' @param n_max           integer - the maximum number of samples to be read (optional)
#' @param ignore_dates    use the start and end dates from the coverage instead of the time series
#' @return data.tb        a SITS tibble
#' @export
sits_getdata <- function (raster.tb   = NULL,
                          file        = NULL,
                          longitude   = NULL,
                          latitude    = NULL,
                          start_date  = NULL,
                          end_date    = NULL,
                          label       = NULL,
                          URL         = "http://www.dpi.inpe.br/tws/wtss",
                          coverage    = NULL,
                          bands       = NULL,
                          n_max       = Inf,
                          ignore_dates = FALSE) {



     # a JSON file has all the data and metadata - no need to access the WTSS server
    if  (!purrr::is_null (file) && tolower(tools::file_ext(file)) == "json"){
          data.tb <- sits_fromJSON (file)
          return (data.tb)
     }
    # get data based on gz (compressed JSON) file
    if (!purrr::is_null (file) && tolower(tools::file_ext(file)) == "gz") {
        data.tb <- sits_fromGZ(file)
        return (data.tb)
    }

    # get data based from ST Raster file
    if (!purrr::is_null (raster.tb)){
        if (!purrr::is_null (file) && tolower(tools::file_ext(file)) == "csv")
            data.tb <- sits_fromRaster(raster.tb, file = file)
        if (!purrr::is_null (longitude) && !purrr::is_null (latitude))
            data.tb <- sits_fromRaster(raster.tb, longitude = longitude, latitude = latitude)
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

     # get information about a specific coverage
     coverage.tb <- sits_coverageWTSS(URL, coverage)

     # get data based on latitude and longitude
     if (purrr::is_null (file) && purrr::is_null (raster.tb) && !purrr::is_null(latitude) && !purrr::is_null(longitude)) {
          data.tb <- sits_fromLatLong (longitude, latitude, start_date, end_date, wtss.obj, cov, bands)
          return (data.tb)
     }
     # get data based on CSV file
     if (!purrr::is_null (file) && tolower(tools::file_ext(file)) == "csv") {
          data.tb <- sits_fromCSV (file, wtss.obj, cov, bands, n_max, ignore_dates)
          return (data.tb)
     }
     # get data based on SHP file
     if (!purrr::is_null (file) && tolower(tools::file_ext(file)) == "shp") {
          data.tb <- sits_fromSHP (file, coverage.tb, bands, start_date, end_date, label)
          return (data.tb)
     }
     message (paste ("No valid input to retrieve time series data!!","\n",sep=""))
     stop()
}

#' @title Obtain timeSeries from a JSON file.
#'
#' @name sits_fromJSON
#'
#' @description reads a set of data and metadata for satellite image time series from a JSON file
#'
#' @param file      string  - name of a JSON file with sits data and metadata
#' @return data.tb   a SITS tibble
#' @export
sits_fromJSON <- function (file) {
     # add the contents of the JSON file to a SITS tibble
     table <- tibble::as_tibble (jsonlite::fromJSON (file))
     # convert Indexes in time series to dates
     table1 <- sits_tibble()
     table %>%
          purrrlyr::by_row(function (r) {
               tb <- tibble::as_tibble(r$time_series[[1]])
               tb$Index <- lubridate::as_date(tb$Index)
               r$time_series[[1]] <- tb
               r$start_date <- lubridate::as_date(r$start_date)
               r$end_date   <- lubridate::as_date(r$end_date)
               table1 <<- dplyr::bind_rows(table1, r)
          })
     return (table1)
}

#' @title Obtain timeSeries from a compressed JSON file.
#'
#' @name sits_fromGZ
#'
#' @description reads a set of data and metadata for satellite image time series from a compressed JSON file
#'
#' @param  file       string  - name of a compressed JSON file with sits data and metadata
#' @return data.tb    a SITS tibble
#' @export
sits_fromGZ <- function (file) {

    # uncompress the file
    json_file <- R.utils::gunzip (file, remove = FALSE)
    # retrieve the data
    data.tb <- sits_fromJSON (json_file)
    # remove the uncompressed file
    file.remove (json_file)

    # return the JSON file
    return (data.tb)
}

#' @title Obtain timeSeries from WTSS server based on a lat/long information.
#' @name sits_fromLatLong
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
#' @param start_date      date - the start of the period
#' @param end_date        date - the end of the period
#' @param wtss.obj       an R object that represents the WTSS server
#' @param cov            a list with the coverage parameters (retrived from the WTSS server)
#' @param bands          string vector - the names of the bands to be retrieved
#' @return data.tb       tibble  - a SITS table
#' @export
sits_fromLatLong <-  function (longitude, latitude, start_date = NULL, end_date = NULL, wtss.obj, cov, bands) {

     # set the class of the time series
     label <-  "NoClass"
     # use the WTSS service to retrieve the time series
     data.tb <- sits_fromWTSS (longitude, latitude, start_date, end_date, label, wtss.obj, cov, bands)
     return (data.tb)
}


#' @title Obtain timeSeries from WTSS server, based on a CSV file.
#' @name sits_fromCSV
#'
#' @description reads descriptive information about a set of
#' spatio-temporal locations from a CSV file. Then, it uses the WTSS time series service
#' to retrieve the time series, and stores the time series on a SITS tibble for later use.
#' The CSV file should have the following column names:
#' "longitude", "latitude", "start_date", "end_date", "label"
#'
#' @param csv_file        string  - name of a CSV file with information <id, latitude, longitude, from, end, label>
#' @param wtss.obj        WTSS object - the WTSS object that describes the WTSS server
#' @param cov             list - a list with coverage information (retrieved from the WTSS)
#' @param bands           string vector - the names of the bands to be retrieved
#' @param n_max           integer - the maximum number of samples to be read
#' @param ignore_dates    whether to use the dates of the coverage and not those specified in the file
#' @return data.tb        a SITS tibble
#' @export

sits_fromCSV <-  function (csv_file, wtss.obj, cov, bands, n_max = Inf, ignore_dates = FALSE){
     # configure the format of the CSV file to be read
     cols_csv <- readr::cols(id          = readr::col_integer(),
                             longitude   = readr::col_double(),
                             latitude    = readr::col_double(),
                             start_date  = readr::col_date(),
                             end_date    = readr::col_date(),
                             label       = readr::col_character())
     # read sample information from CSV file and put it in a tibble
     csv.tb <- readr::read_csv (csv_file, n_max = n_max, col_types = cols_csv)
     # create the tibble
     data.tb <- sits_tibble()
     # for each row of the input, retrieve the time series
     csv.tb %>%
          purrrlyr::by_row( function (r){
               row <- sits_fromWTSS (r$longitude, r$latitude, r$start_date, r$end_date, r$label, wtss.obj, cov, bands, ignore_dates)

               # ajust the start and end dates
               row$start_date <- lubridate::as_date(utils::head(row$time_series[[1]]$Index, 1))
               row$end_date   <- lubridate::as_date(utils::tail(row$time_series[[1]]$Index, 1))

               data.tb <<- dplyr::bind_rows (data.tb, row)
          })
     return (data.tb)
}
#' @title Extract a time series from a ST raster data set
#' @name sits_fromRaster
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Reads metadata about a raster data set to retrieve a set of
#' time series.
#'
#' @param raster.tb       A tibble with metadata describing a spatio-temporal data set
#' @param file            A CSV file with lat/long locations to be retrieve
#' @param longitude       double - the longitude of the chosen location
#' @param latitude        double - the latitude of the chosen location
#' @param xcoord          X coordinate of the point where the time series is to be obtained
#' @param ycoord          Y coordinate of the point where the time series is to be obtained
#' @param xmin            Minimum X coordinates of bounding box
#' @param xmax            Maximum X coordinates of bounding box
#' @param ymin            Minimum Y coordinates of bounding box
#' @param ymax            Maximum Y coordinates of bounding box
#' @param start_date      date - the start of the period
#' @param end_date        date - the end of the period
#' @param label           string - the label to attach to the time series
#' @param coverage        string - the name of the coverage to be retrieved
#' @return data.tb        a SITS tibble with the time series
#'
#' @description This function creates a tibble to store the information
#' about a raster time series
#'
#' @export
sits_fromRaster <- function (raster.tb, file = NULL, longitude = NULL, latitude = NULL,  xcoord = NULL, ycoord = NULL,
                             xmin = NULL, xmax = NULL, ymin = NULL, ymax = NULL,
                             start_date = NULL, end_date  = NULL, label = "NoClass", coverage    = NULL) {

    # ensure metadata tibble exists
    .sits_test_tibble (raster.tb)

    # get data based on CSV file
    if (!purrr::is_null (file) && tolower(tools::file_ext(file)) == "csv") {
        data.tb <- .sits_ts_fromRasterCSV (raster.tb, file)
    }

    if (!purrr::is_null (longitude) && !purrr::is_null (latitude)){
        xy <- .sits_latlong_to_proj(longitude, latitude, raster.tb[1,]$crs)
        data.tb <- .sits_ts_fromRasterXY (raster.tb, xy, longitude, latitude, label, coverage)
    }
    return (data.tb)
}
#' @title Obtain timeSeries from WTSS server, based on a SHP file.
#' @name sits_fromSHP
#'
#' @description reads a shapefile and retrieves a SITS tibble
#' containing time series from a coverage that are inside the SHP file.
#' The script uses the WTSS service, taking information about coverage, spatial and
#' temporal resolution from the WTSS configuration.
#'
#'
#' @param shp_file        string  - name of a SHP file which provides the boundaries of a region of interest
#' @param URL             string - the URL of WTSS (Web Time Series Service)
#' @param coverage.tb     metadata describing the coverage
#' @param bands           string vector - the names of the bands to be retrieved
#' @param start_date      date - the start of the period
#' @param end_date        date - the end of the period
#' @param label           string - the label to attach to the time series
#' @return table          a SITS tibble
#' @export
#'
sits_fromSHP <- function (shp_file, URL, coverage.tb, bands, start_date = NULL, end_date = NULL, label = NULL ) {

    # read the shapefile
    sf_shape <- sf::read_sf(shp_file)
    # get the bounding box
    bbox <- sf::st_bbox (sf_shape)

    shape.tb <- sits_tibble()
    label <- "NoClass"
    # retrieve the wtss.obj
    wtss.obj <- coverage.tb$wtss.obj[[1]]

    #retrieve coverage information
    cov <- sits_getcovWTSS(URL, coverage.tb$name)

    # adjust start and end dates
    if (purrr::is_null (start_date)) start_date <- as.Date (coverage.tb$start_date)
    if (purrr::is_null (end_date)) end_date <- as.Date (coverage.tb$end_date)

    # recover the bounding boxes
    longitude <- bbox["xmin"]
    while (longitude <= bbox["xmax"] ){
        latitude  <- bbox["ymin"]
        while (latitude <= bbox["ymax"]){
            ll <- sf::st_point(c(longitude, latitude))
            if (1 %in% as.logical (unlist(sf::st_within(ll, sf_shape)))) {
                row <- sits_fromWTSS (longitude, latitude, start_date, end_date, label, wtss.obj, cov, bands)
                shape.tb <- dplyr::bind_rows(shape.tb, row)
            }
            latitude <-  latitude + coverage.tb$yres
        }
        longitude <- longitude + coverage.tb$xres
    }
    return (shape.tb)
}



#' @title Obtain one timeSeries from WTSS server and load it on a sits tibble
#' @name sits_fromWTSS
#'
#' @description Returns one set of time series provided by a WTSS server
#' Given a location (lat/long), and start/end period, and the WTSS server information
#' retrieve a time series and include it on a stis tibble.
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
#' @param ignore_dates    whether to use the dates of the coverage and not those specified in the file
#' @return data.tb        a SITS tibble
#' @export
sits_fromWTSS <- function (longitude, latitude, start_date, end_date, label, wtss.obj, cov, bands, ignore_dates = FALSE) {

     # set the start and end dates from the coverage
     if (purrr::is_null (start_date) | ignore_dates ) start_date <- lubridate::as_date(cov$timeline[1])
     if (purrr::is_null (end_date)   | ignore_dates ) end_date <- lubridate::as_date(cov$timeline[length(cov$timeline)])

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

     # create a tibble to store the WTSS data
     data.tb <- sits_tibble()
     # add one row to the tibble
     data.tb <- tibble::add_row (data.tb,
                         longitude    = longitude,
                         latitude     = latitude,
                         start_date   = as.Date(start_date),
                         end_date     = as.Date(end_date),
                         label        = label,
                         coverage     = cov$name,
                         time_series  = ts.lst
     )

     # return the tibble with the time series
     return (data.tb)
}

#' @title Import time series in the zoo format to a SITS tibble
#' @name sits_fromZOO
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Converts data from an instance of a zoo series to a SITS tibble
#'
#' @param ts.zoo        A zoo time series
#' @param longitude     Longitude of the chosen location
#' @param latitude      Latitude of the chosen location
#' @param label         Label to attach to the time series (optional)
#' @param coverage      Name of the coverage where data comes from
#' @return data.tb      A time series in SITS tibble format
#' @export
sits_fromZOO <- function (ts.zoo, longitude = 0.00, latitude = 0.00, label = "NoLabel", coverage = "unknown"){

    # convert the data from the zoo format to the SITS format
    ts.tb <- tibble::as_tibble (zoo::fortify.zoo (time_series))

    # create a list to store the zoo time series coming from the WTSS service
    ts.lst <- list()
    # transform the zoo list into a tibble to store in memory
    ts.lst[[1]] <- ts.tb

    # get the start date
    start_date <- ts.tb[1,]$Index
    # get the end date
    end_date <- ts.tb[NROW(ts.tb),]$Index

    # create a tibble to store the WTSS data
    data.tb <- sits_tibble()
    # add one row to the tibble
    data.tb <- .sits_add_row (data.tb,
                                longitude    = longitude,
                                latitude     = latitude,
                                start_date   = as.Date(start_date),
                                end_date     = as.Date(end_date),
                                label        = label,
                                coverage     = coverage,
                                time_series  = ts.lst
    )

    return (data.tb)
}



