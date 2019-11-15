#' @title Obtain time series from different sources
#' @name sits_get_data
#' @author Gilberto Camara
#'
#' @description Retrieve a set of time series and puts it in a "sits tibble".
#' Sits tibbles are the main structures of sits package.
#' They contain both the satellite image time series and their metadata.
#' A sits tibble is a tibble with pre-defined columns that
#' has the metadata and data for each time series. The columns are
#' <longitude, latitude, start_date, end_date, label, cube, time_series>.
#' There are two main ways of retrieving time series:
#' 1. Using a time series service and from a data cube defined based on a set of Raster Bricks. Two time series services are available:
#' (a) the Web Time Series Service (WTSS) by INPE; (b) the SATVEG service from EMBRAPA.
#' Please see \code{\link[sits]{sits_services}} for more information on the WTSS service.
#' The URL and other parameters for accessing the time series services are defined in the package
#' configuration file. This file is called "config.yml". Please see the \code{\link[sits]{sits_config}} for
#' more information.
#'
#' Before using this service, the user should create a valid description of a data cube ´
#' using the \code{\link[sits]{sits_cube}} function.
#'
#' The following options are available:
#' \enumerate{
#' \item No input file is given - it retrieves the data and metadata based on the latitude/longitude location
#' and on the information provided by the WTSS server.
#' \item The source is a CSV file - retrieves the metadata from the CSV file and the time series
#' from the WTSS service.
#' \item The source is a SHP file - retrives all points inside the shapefile from the WTSS service.
#' \item The source is a RasterBrick - retrieves the point based on lat/long from the RasterBrick.
#' }
#'  The result is a sits tibble, which  has the metadata and data for each time series
#' <longitude, latitude, start_date, end_date, label, cube, time_series>
#'
#' @references
#' Lubia Vinhas, Gilberto Queiroz, Karine Ferreira, Gilberto Camara,
#' Web Services for Big Earth Observation Data.
#' In: XVII Brazilian Symposium on Geoinformatics, 2016, Campos do Jordao.
#' Proceedings of GeoInfo 2016. Sao Jose dos Campos: INPE/SBC, 2016. v.1. p.166-177.
#'
#' @param cube            A tibble with information about the data cube from where data is to be retrived.
#' @param file            An optional name of a file with information on the data to be retrieved (options - CSV, SHP).
#' @param longitude       Longitude of the chosen location.
#' @param latitude        Latitude of the chosen location.
#' @param start_date      An optional start of the interval for the time series in Date format ("YYYY-MM-DD").
#' @param end_date        An optional end of the interval for the time series in Date format ("YYYY-MM-DD").
#' @param bands           An optional vector with the names of the bands to be retrieved.
#' @param label           An optional string with the label to be assigned to the time series.
#' @param shp_attr        An optional string to indicate the attribute in the shapefile to be used as a polygon label.
#' @param .n_shp_pol      Number of samples per polygon to be read (for POLYGON or MULTIPOLYGON shapes).
#' @param .n_shp_pts      Number of points to be read (for POINT shapes).
#' @param .prefilter      Prefilter for SATVEG cube("0" - none, "1" - no data correction, "2" - cloud correction, "3" - no data and cloud correction).
#' @param .n_start_csv    An optional integer with the row on the CSV file to start reading.
#' @param .n_max_csv      An optional integer with the maximum number of CSV samples to be read (set to Inf to read all).
#' @param .n_save         An optional number of samples to save as intermediate files (used for long reads).
#' @return A tibble with time series data and metadata.
#'
#' @examples
#' \donttest{
#' # Read a single lat long point from a WTSS server
#'
#' wtss_cube <- sits_cube(service = "WTSS", name = "MOD13Q1")
#' point.tb <- sits_get_data (wtss_cube, longitude = -55.50563, latitude = -11.71557)
#' plot(point.tb)
#'
#' # Read a set of points defined in a CSV file from a WTSS server
#' csv_file <- system.file ("extdata/samples/samples_matogrosso.csv", package = "sits")
#' points.tb <- sits_get_data (wtss_cube, file = csv_file)
#' # show the points retrieved for the WTSS server
#' plot(points.tb[1:3,])
#'
#' # Read a single lat long point from the SATVEG server
#' satveg_cube <- sits_cube(service = "SATVEG", name = "terra")
#' point_satveg.tb <- sits_get_data (satveg_cube, longitude = -55.50563, latitude = -11.71557)
#' plot(point_satveg.tb)
#'
#' # define a shapefile and read from the points inside it from the WTSS service
#' shp_file <- system.file("extdata/shapefiles/parcel_agriculture.shp", package = "sits")
#' parcel.tb <- sits_get_data(wtss_cube, file = shp_file, .n_shp_pol = 5)
#'
#' # Read a point in a Raster Brick
#' # define the file that has the raster brick
#' files  <- c(system.file ("extdata/raster/mod13q1/sinop-crop-ndvi.tif", package = "sits"))
#' # define the timeline
#' data(timeline_modis_392)
#' # create a data cube based on the information about the files
#' raster_cube <- sits_cube(name = "Sinop-crop", timeline = timeline_modis_392,
#'                          bands = c("ndvi"), files = files)
#' # read the time series of the point from the raster
#' point_ts <- sits_get_data(raster_cube, longitude = -55.554, latitude = -11.525)
#' plot(point_ts)
#'
#' #' # Read a CSV file in a Raster Brick
#' csv_file <- system.file ("extdata/samples/samples_sinop_crop.csv", package = "sits")
#' points.tb <- sits_get_data (raster_cube, file = csv_file)
#' # show the points retrieved for the RASTER images
#' plot(points.tb)
#' }
#' @export
sits_get_data <- function(cube,
                         file         = NULL,
                         longitude    = NULL,
                         latitude     = NULL,
                         start_date   = NULL,
                         end_date     = NULL,
                         bands        = NULL,
                         label        = "NoClass",
                         shp_attr     = NULL,
                         .n_shp_pol   = 20,
                         .n_shp_pts   = Inf,
                         .prefilter   = "1",
                         .n_start_csv = 1,
                         .n_max_csv   = Inf,
                         .n_save      = 0) {

    # Ensure that the cube is valid
    check <- .sits_cube_check_validity(cube)
    ensurer::ensure(check, (.) == TRUE,
                    err_desc = "sits_get_data: cube is not valid or not accessible")

    # No file is given - lat/long must be provided
    if (purrr::is_null(file)) {
        #precondition
        ensurer::ensure_that(latitude, !purrr::is_null(.) && !purrr::is_null(longitude),
                             err_desc = "sits_get_data - latitude/longitude must be provided")

        data    <- .sits_from_service(cube = cube, longitude = longitude, latitude = latitude,
                                      start_date = start_date, end_date = end_date,
                                      bands = bands, label = label, .prefilter = .prefilter)
    }
    # file is given - must be either CSV or SHP
    else {
        # precondition
        ensurer::ensure_that(file, tolower(tools::file_ext(.)) == "csv" || tolower(tools::file_ext(.)) == "shp",
                             err_desc = "sits_get_data - file must either be a CSV or a shapefile")

        # get data based on CSV file
        if (tolower(tools::file_ext(file)) == "csv")
            data  <- .sits_from_csv(csv_file = file, cube = cube, bands = bands,
                                    .prefilter = .prefilter, .n_start_csv = .n_start_csv,
                                    .n_max_csv = .n_max_csv, .n_save = .n_save)

        # get data based on SHP file
        if (tolower(tools::file_ext(file)) == "shp")
            data  <- .sits_from_shp(shp_file = file, cube = cube, start_date = start_date,
                                    end_date = end_date, bands = bands, label = label, shp_attr = shp_attr,
                                    .n_shp_pol = .n_shp_pol, .n_shp_pts = .n_shp_pts, .prefilter = .prefilter)
    }
    return(data)
}


#' @title Extract a time series from a ST raster data set
#' @name .sits_from_raster
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Retrieve a set of time series for data cube stored as a raster data set.
#'
#' @param cube            A tibble with metadata describing a data cube stored as a raster data set.
#' @param longitude       A double with the longitude of the chosen location.
#' @param latitude        A double with the latitude of the chosen location.
#' @param start_date      A date with the start of the period.
#' @param end_date        A date with the end of the period.
#' @param bands           A string vector with the names of the bands to be retrieved.
#' @param label           A string with the label to attach to the time series.
#' @return A sits tibble with the time series.
.sits_from_raster <- function(cube, longitude, latitude,
                              start_date, end_date, bands,
                              label = "NoClass"){

    # ensure metadata tibble exists
    ensurer::ensure_that(cube, NROW(.) >= 1,
                         err_desc = "sits_from_raster: need a valid metadata for data cube")

    timeline <- sits_timeline(cube)

    start_idx <- 1
    end_idx   <- length(timeline)

    if (!purrr::is_null(start_date)) {
        start_idx <- which.min(abs(lubridate::as_date(start_date) - timeline))
    }
    if (!purrr::is_null(end_date)) {
        end_idx <- which.min(abs(lubridate::as_date(end_date) - timeline))
    }
    timeline <- timeline[start_idx:end_idx]

    ts.tb <- tibble::tibble(Index = timeline)

    # get the bands, scale factors and missing values
    bands <- unlist(cube$bands)
    missing_values <- unlist(cube$missing_values)
    scale_factors  <- unlist(cube$scale_factors)
    nband <- 0

    # transform longitude and latitude to an sp Spatial Points* (understood by raster)
    st_point <- sf::st_point(c(longitude, latitude))
    ll_sfc   <- sf::st_sfc(st_point, crs = "+init=epsg:4326")
    ll_sp    <- sf::as_Spatial(ll_sfc)

    r_objs <- .sits_cube_all_robjs(cube)

    # An input raster brick contains several files, each corresponds to a band
    values.lst <- r_objs %>%
        purrr::map(function(r_brick) {
            # eack brick is a band
            nband <<- nband + 1
            # get the values of the time series
            values <- suppressWarnings(as.vector(raster::extract(r_brick, ll_sp)))
            # is the data valid?
            if (all(is.na(values))) {
                message("point outside the raster extent - NULL returned")
                return(NULL)
            }
            # create a tibble to store the values
            values.tb <- tibble::tibble(values[start_idx:end_idx])
            # find the names of the tibble column
            band <- bands[nband]
            names(values.tb) <- band
            # correct the values using the scale factor
            values.tb <- values.tb[,1]*scale_factors[band]
            return(values.tb)
        })

    ts.tb <- dplyr::bind_cols(ts.tb, values.lst)

    # create a list to store the time series coming from the set of Raster Layers
    ts.lst <- list()
    # transform the list into a tibble to store in memory
    ts.lst[[1]] <- ts.tb

    # create a tibble to store the WTSS data
    data <- .sits_tibble()
    # add one row to the tibble
    data  <- tibble::add_row(data,
                               longitude    = longitude,
                               latitude     = latitude,
                               start_date   = as.Date(timeline[1]),
                               end_date     = as.Date(timeline[length(timeline)]),
                               label        = label,
                               cube         = cube$name,
                               time_series  = ts.lst
    )
    return(data)
}
#' @title Obtain timeSeries from a web service associated to data cubes
#' @name .sits_from_service
#'
#' @description Obtains a time series from a time series service.
#'
#' @param cube            Data cube metadata.
#' @param longitude       Longitude of the chosen location.
#' @param latitude        Latitude of the chosen location).
#' @param start_date      Optional start date of the period.
#' @param end_date        Optional end date of the period.
#' @param bands           Optional string vector with the names of the bands to be retrieved.
#' @param label           String with the label to attach to the time series.
#' @param .prefilter      String (only for SATVEG) ("0" - none, "1" - no data correction, "2" - cloud correction, "3" - no data and cloud correction).
#' @return A sits tibble.
.sits_from_service <- function(cube,
                               longitude,
                               latitude,
                               start_date,
                               end_date,
                               bands,
                               label = "NoClass",
                               .prefilter  = "1") {

    # find out which is the service associate to the cube
    service <- .sits_cube_service(cube)

    if (service == "EOCUBES") {
        data  <- .sits_from_EOCubes(cube = cube, longitude = longitude, latitude = latitude,
                                    start_date = start_date, end_date = end_date, bands = bands, label = label)
        return(data)
    }
    if (service == "WTSS") {
        data <- .sits_from_wtss(cube = cube, longitude = longitude, latitude = latitude,
                                start_date = start_date, end_date = end_date, bands = bands, label = label)
        return(data)
    }
    if (service == "SATVEG") {
        data <- .sits_from_satveg(cube = cube, longitude = longitude, latitude = latitude,
                                  start_date = start_date, end_date = end_date, bands = bands,
                                  label = label, .prefilter = .prefilter)

        return(data)
    }
    if (service == "BRICK" || service == "STACK" || service == "AWS") {
        data <- .sits_from_raster(cube = cube, longitude = longitude, latitude = latitude,
                                  start_date = start_date, end_date = end_date, bands = bands, label = label)

        return(data)
    }
    return(NULL)
}

