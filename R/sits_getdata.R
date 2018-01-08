#' @title Obtain time series from different sources
#' @name sits_getdata
#' @author Gilberto Camara
#'
#' @description Retrieve a set of time series. There are two main ways of retrieving time series:
#' using a time series service and from a Raster Brick. Two time series services are available:
#' (a) the Web Time Series Service (WTSS) by INPE; (b) the SATVEG service from EMBRAPA.
#' Please see \code{link[sits]{sits_infoWTSS}} for more information on thw WTSS service.
#' Please see \code{link[sits]{sits_fromSATVEG}} for more information on SATVEG.
#' The URL and other parameters for access to the time series services are defined in the package
#' configuration file. This file is called "config.yml". Please see the \code{link[sits]{sits_config}} for
#' more information.
#' The following options are available:
#' \enumerate{
#' \item No input file is given - it retrieves the data and metadata based on the latitude/longitude location
#' and on the information provided by the WTSS server.
#' \item The source is a CSV file - retrieves the metadata from the CSV file and the time series
#' from the WTSS service.
#' \item The source is a SHP file - retrives all points inside the shapefile from the WTSS service.
#' \item The source is a RasterBrick - retrieves the point based on lat/long from the RasterBrick.
#' \item The source is a ZOO time series - retrieves the time series from the ZOO file, and
#' the user should supply the metadata.
#' }
#  The results is a SITS tibble, which  has the metadata and data for each time series
#' <longitude, latitude, start_date, end_date, label, coverage, time_series>
#'
#' @references
#' Lubia Vinhas, Gilberto Queiroz, Karine Ferreira, Gilberto Camara,
#' Web Services for Big Earth Observation Data.
#' In: XVII Brazilian Symposium on Geoinformatics, 2016, Campos do Jordao.
#' Proceedings of GeoInfo 2016. Sao Jose dos Campos: INPE/SBC, 2016. v.1. p.166-177.
#'
#' @param raster.tb       (optional) an STRaster  object (tibble with raster information)
#' @param file            (optional) the name of a file with information on the data to be retrieved (options - CSV, JSON, SHP)
#' @param longitude       double - the longitude of the chosen location
#' @param latitude        double - the latitude of the chosen location)
#' @param start_date      (optional) date - the start of the period
#' @param end_date        (optional) date - the end of the period
#' @param service         string - name of the time series service (options are "WTSS" or "SATVEG")
#' @param coverage        string - the name of the coverage to be retrieved
#' @param bands           (optional) vector - the names of the bands to be retrieved
#' @param satellite       (optional) - the same of the satellite (options - "terra", "aqua", "comb")
#' @param prefilter       string ("0" - none, "1" - no data correction, "2" - cloud correction, "3" - no data and cloud correction)
#' @param label           (optional) string - the label to be assigned to the time series
#' @param n_max           (optional) integer - the maximum number of samples to be read
#' @return data.tb        a SITS tibble
#'
#' @examples
#' \donttest{
#' # Read a single lat long point from a WTSS server
#' point.tb <- sits_getdata (longitude = -55.50563, latitude = -11.71557,
#'          service = "WTSS", coverage    = "mod13q1_512")
#'
#' # show the point
#' show(point.tb)
#' # Read a set of points defined in a CSV file from a WTSS server
#' csv_file <- system.file ("extdata/samples/samples_import.csv", package = "sits")
#' points.tb <- sits_getdata (file = csv_file, service = "WTSS",
#'              coverage = "mod13q1_512")
#' show (points.tb)
#' # show the points retrieved for the WTSS server
#' }
#' # Read a point in a Raster Brick
#' # define the file that has the raster brick
#' files  <- c(system.file ("extdata/raster/mod13q1/sinop-crop-ndvi.tif", package = "sits"))
#' # define the timeline
#' data(timeline_mod13q1)
#' timeline <- lubridate::as_date (timeline_mod13q1$V1)
#' # create a raster metadata file based on the information about the files
#' raster.tb <- sits_STRaster (files, timeline, bands = c("ndvi"), scale_factors = c(0.0001))
#' # read the point from the raster
#' point.tb <- sits_getdata(raster.tb, longitude = -55.50563, latitude = -11.71557)
#'
#' @export
sits_getdata <- function(raster.tb   = NULL,
                         file        = NULL,
                         longitude   = NULL,
                         latitude    = NULL,
                         start_date  = NULL,
                         end_date    = NULL,
                         service     = "WTSS",
                         coverage    = "mod13q1_512",
                         bands       = NULL,
                         satellite   = "terra",
                         prefilter   = "1",
                         label       = "NoClass",
                         n_max       = Inf) {

    # load the configuration file
    if (purrr::is_null(sits.env$config))
        sits_config()

    # a JSON file has all the data and metadata
    if  (!purrr::is_null(file) && tolower(tools::file_ext(file)) == "json") {
          data.tb <- sits_fromJSON(file)
          return(data.tb)
     }
    # get data based on gz (compressed JSON) file
    if (!purrr::is_null(file) && tolower(tools::file_ext(file)) == "gz") {
        data.tb <- sits_fromGZ(file)
        return(data.tb)
    }

    # get data based from ST Raster file
    if (!purrr::is_null(raster.tb)) {
        if (!purrr::is_null(file) && tolower(tools::file_ext(file)) == "csv")
            data.tb <- sits_fromRaster(raster.tb, file = file)
        if (!purrr::is_null(longitude) && !purrr::is_null(latitude))
            data.tb <- sits_fromRaster(raster.tb,
                        longitude = longitude, latitude = latitude)
        return(data.tb)
    }

    # Ensure that the service is available
    ensurer::ensure_that(service, (.) %in% sits.env$config$ts_services,
                         err_desc = "sits_getdata: Invalid time series service")

    # get data based on latitude and longitude
    if (purrr::is_null(file) && purrr::is_null(raster.tb)
        && !purrr::is_null(latitude) && !purrr::is_null(longitude)) {
        data.tb <- sits_from_service(service, longitude, latitude, start_date, end_date,
                                     coverage, bands, satellite, prefilter, label)
        return(data.tb)
    }
    # get data based on CSV file
    if (!purrr::is_null(file) && tolower(tools::file_ext(file)) == "csv") {
        data.tb <- sits_fromCSV(file, service, coverage, bands, satellite, prefilter, n_max)
        return(data.tb)
    }
    # get data based on SHP file
    if (!purrr::is_null(file) && tolower(tools::file_ext(file)) == "shp") {
        data.tb <- sits_fromSHP(file, service, start_date, end_date, coverage, bands,
                                satellite, prefilter, label)
        return(data.tb)
    }
    message(paste("No valid input to retrieve time series data!!", "\n", sep = ""))
    stop()
}

#' @title Obtain timeSeries from time series service
#' @name sits_from_service
#'
#' @description obtains a time series from a time series service
#'
#' @param service         string - name of the time series service (options are "WTSS" or "SATVEG")
#' @param longitude       double - the longitude of the chosen location
#' @param latitude        double - the latitude of the chosen location)
#' @param start_date      (optional) date - the start of the period
#' @param end_date        (optional) date - the end of the period
#' @param coverage        name of the coverage (required for WTSS)
#' @param bands           (optional) string vector - the names of the bands to be retrieved
#' @param satellite       (optional) - the same of the satellite (options - "terra", "aqua", "comb")
#' @param prefilter       string ("0" - none, "1" - no data correction, "2" - cloud correction, "3" - no data and cloud correction)
#' @param label           string - the label to attach to the time series
#' @return table          a SITS tibble
#'
#' @examples
#' \donttest{
#' # define a shapefile and read from the points inside it from the WTSS service
#' shp_file <- system.file("extdata/shapefiles/anhanguera/anhanguera.shp", package = "sits")
#' munic.tb <- sits_fromSHP(shp_file)
#' }
#' @export
#'
sits_from_service <- function(service    = "WTSS",
                              latitude   = NULL,
                              longitude  = NULL,
                              start_date = NULL,
                              end_date   = NULL,
                              coverage   = "mod13q1_512",
                              bands      = NULL,
                              satellite  = "terra",
                              prefilter  = "1",
                              label      = "NoClass") {

    # load the configuration file
    if (!exists("config_sys"))
        config_sits <- sits_config()

    ensurer::ensure_that(service, (.) %in% sits.env$config$ts_services,
                         err_desc = "sits_from_ts_service: Invalid time series service")

    if (service == "WTSS")
        data.tb <- sits_fromWTSS(latitude, longitude, start_date, end_date, coverage, bands, label)
    if (service == "SATVEG")
        data.tb <- sits_fromSATVEG(latitude, longitude, start_date, end_date, satellite, prefilter, label)

    return(data.tb)
}



