#' @title Obtain time series from different sources
#' @name sits_getdata
#' @author Gilberto Camara
#'
#' @description Retrieve a set of time series. There are two main ways of retrieving time series:
#' using a time series service and from a Raster Brick. Two time series services are available:
#' (a) the Web Time Series Service (WTSS) by INPE; (b) the SATVEG service from EMBRAPA.
#' Please see \code{\link[sits]{sits_info_wtss}} for more information on thw WTSS service.
#' The URL and other parameters for access to the time series services are defined in the package
#' configuration file. This file is called "config.yml". Please see the \code{\link[sits]{sits_config}} for
#' more information.
#'
#' Before using this service, the user should create a valid coverage tibble
#' using the \code{\link[sits]{sits_coverage}} function.
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
#'  The results is a sits tibble, which  has the metadata and data for each time series
#' <longitude, latitude, start_date, end_date, label, coverage, time_series>
#'
#' @references
#' Lubia Vinhas, Gilberto Queiroz, Karine Ferreira, Gilberto Camara,
#' Web Services for Big Earth Observation Data.
#' In: XVII Brazilian Symposium on Geoinformatics, 2016, Campos do Jordao.
#' Proceedings of GeoInfo 2016. Sao Jose dos Campos: INPE/SBC, 2016. v.1. p.166-177.
#'
#' @param coverage        A mandatory tibble with information about the coverage.
#' @param file            An optional name of a file with information on the data to be retrieved (options - CSV, SHP).
#' @param longitude       Longitude of the chosen location.
#' @param latitude        Latitude of the chosen location.
#' @param start_date      An optional start of the interval for the time series in Date format ("YYYY-MM-DD").
#' @param end_date        An optional end of the interval for the time series in Date format ("YYYY-MM-DD").
#' @param bands           An optional vector with the names of the bands to be retrieved.
#' @param prefilter       An optional string ("0" - none, "1" - no data correction, "2" - cloud correction, "3" - no data and cloud correction).
#' @param label           An optional string with the label to be assigned to the time series.
#' @param .n_start        An optional integer with the row on the CSV file to start reading.
#' @param .n_max          An optional integer with the maximum number of CSV samples to be read (set to Inf to read all).
#' @param .n_save         An optional number of samples to save as intermediate files (used for long reads).
#' @return A tibble with time series data and metadata.
#'
#' @examples
#' \donttest{
#' # Read a single lat long point from a WTSS server
#' wtss_coverage <- sits_coverage(service = "WTSS-INPE", name = "MOD13Q1")
#' point.tb <- sits_getdata (wtss_coverage, longitude = -55.50563, latitude = -11.71557)
#' sits_plot(point.tb)
#'
#' # Read a set of points defined in a CSV file from a WTSS server
#' csv_file <- system.file ("extdata/samples/samples_matogrosso.csv", package = "sits")
#' points.tb <- sits_getdata (wtss_coverage, file = csv_file)
#' # show the points retrieved for the WTSS server
#' sits_plot (points.tb[1:3,])
#'
#' # Read a single lat long point from the SATVEG server
#' satveg_coverage <- sits_coverage(service = "SATVEG", name = "terra")
#' point_satveg.tb <- sits_getdata (satveg_coverage, longitude = -55.50563, latitude = -11.71557)
#' sits_plot(point_satveg.tb)
#'
#' # define a shapefile and read from the points inside it from the WTSS service
#' shp_file <- system.file("extdata/shapefiles/santa_cruz_minas.shp", package = "sits")
#' munic.tb <- sits_getdata(coverage = wtss_coverage, file = shp_file)
#'
#' # Read a point in a Raster Brick
#' # define the file that has the raster brick
#' files  <- c(system.file ("extdata/raster/mod13q1/sinop-crop-ndvi.tif", package = "sits"))
#' # define the timeline
#' data(timeline_modis_392)
#' # create a raster metadata file based on the information about the files
#' raster_cov <- sits_coverage(files = files, name = "Sinop-crop",
#'                             timeline = timeline_modis_392, bands = c("ndvi"))
#' # read the point from the raster
#' point_raster.tb <- sits_getdata(raster_cov, longitude = -55.554, latitude = -11.525)
#' sits_plot(point_raster.tb)
#'
#' #' # Read a CSV file in a Raster Brick
#' # define the file that has the raster brick
#' files  <- c(system.file ("extdata/raster/mod13q1/sinop-crop-ndvi.tif", package = "sits"))
#' # define the timeline
#' data(timeline_modis_392)
#' # create a raster metadata file based on the information about the files
#' raster_cov <- sits_coverage(files = files, name = "Sinop-crop",
#'                             timeline = timeline_modis_392, bands = c("ndvi"))
#' csv_raster_file <- system.file ("extdata/samples/samples_sinop_crop.csv", package = "sits")
#' points.tb <- sits_getdata (raster_cov, file = csv_raster_file)
#' # show the points retrieved for the RASTER images
#' sits_plot (points.tb)
#' }
#' @export
sits_getdata <- function(coverage    = NULL,
                         file        = NULL,
                         longitude   = NULL,
                         latitude    = NULL,
                         start_date  = NULL,
                         end_date    = NULL,
                         bands       = NULL,
                         prefilter   = "1",
                         label       = "NoClass",
                         .n_start    = 1,
                         .n_max      = Inf,
                         .n_save     = 0) {
    # Ensure that the service is available
    .sits_check_service(coverage[1,]$service)

    # get data based on latitude and longitude
    if (purrr::is_null(file) &&
        !purrr::is_null(latitude) && !purrr::is_null(longitude)) {
        data.tb <- .sits_from_service(coverage = coverage,
                                      longitude = longitude,
                                      latitude = latitude,
                                      start_date = start_date,
                                      end_date = end_date,
                                      bands = bands,
                                      prefilter = prefilter,
                                      label = label)
        return(data.tb)
    }
    # get data based on CSV file
    if (!purrr::is_null(file) && tolower(tools::file_ext(file)) == "csv") {
        data.tb <- .sits_from_csv(csv_file  = file,
                                 coverage  = coverage,
                                 bands     = bands,
                                 prefilter = prefilter,
                                .n_start   = .n_start,
                                .n_max     = .n_max,
                                .n_save    = .n_save)
        return(data.tb)
    }
    # get data based on SHP file
    if (!purrr::is_null(file) && tolower(tools::file_ext(file)) == "shp") {
        data.tb <- .sits_from_shp(file, coverage, start_date, end_date, bands, prefilter, label)
        return(data.tb)
    }
    message(paste("No valid input to retrieve time series data!!", "\n", sep = ""))
    stop()
}

#' @title Obtain timeSeries from time series service
#' @name .sits_from_service
#'
#' @description Obtains a time series from a time series service.
#'
#' @param coverage        Coverage metadata.
#' @param longitude       Longitude of the chosen location.
#' @param latitude        Latitude of the chosen location).
#' @param start_date      Optional start date of the period.
#' @param end_date        Optional end date of the period.
#' @param bands           Optional string vector with the names of the bands to be retrieved.
#' @param prefilter       String (only for SATVEG) ("0" - none, "1" - no data correction, "2" - cloud correction, "3" - no data and cloud correction).
#' @param label           String with the label to attach to the time series.
#' @return A sits tibble.
.sits_from_service <- function(coverage,
                               longitude,
                               latitude,
                               start_date,
                               end_date,
                               bands,
                               prefilter  = "1",
                               label = "NoClass") {
    protocol <- .sits_get_protocol(coverage[1,]$service)

    if (protocol == "WTSS") {
        data.tb <- .sits_from_wtss(coverage = coverage,
                                  longitude = longitude,
                                  latitude = latitude,
                                  start_date = start_date,
                                  end_date = end_date,
                                  bands = bands,
                                  label = label)
        return(data.tb)
    }
    if (protocol == "SATVEG") {
        data.tb <- .sits_from_satveg(coverage = coverage,
                                   longitude = longitude,
                                   latitude = latitude,
                                   start_date = start_date,
                                   end_date = end_date,
                                   prefilter = prefilter,
                                   label = label)

        return(data.tb)
    }
    if (protocol == "RASTER") {
        data.tb <- .sits_from_raster(coverage = coverage,
                                    longitude = longitude,
                                    latitude = latitude,
                                    start_date = start_date,
                                    end_date = end_date,
                                    label = label)

        return(data.tb)
    }
    return(NULL)
}

#' @title Obtain timeSeries from time series server, based on a CSV file.
#' @name .sits_from_csv
#'
#' @description reads descriptive information about a set of
#' spatio-temporal locations from a CSV file. Then, it uses the WTSS time series service
#' to retrieve the time series, and stores the time series on a sits tibble for later use.
#' The CSV file should have the following column names:
#' "longitude", "latitude", "start_date", "end_date", "label"
#'
#' @param csv_file        Name of a CSV file with information <id, latitude, longitude, from, end, label>.
#' @param coverage        A tibble with metadata about coverage which contains data to be retrieved.
#' @param bands           A string vector with the names of the bands to be retrieved.
#' @param prefilter       String ("0" - none, "1" - no data correction, "2" - cloud correction, "3" - no data and cloud correction).
#' @param .n_start        Row on the CSV file to start reading (optional).
#' @param .n_max          Maximum number of samples to be read.
#' @param .n_save         Number of samples to save as intermediate files (used for long reads).
#' @return A sits tibble.
.sits_from_csv <-  function(csv_file, coverage, bands, prefilter, .n_start, .n_max, .n_save) {
    # configure the format of the CSV file to be read
    cols_csv <- readr::cols(id          = readr::col_integer(),
                            longitude   = readr::col_double(),
                            latitude    = readr::col_double(),
                            start_date  = readr::col_date(),
                            end_date    = readr::col_date(),
                            label       = readr::col_character())
    # read sample information from CSV file and put it in a tibble
    csv.tb <- readr::read_csv(csv_file, n_max = Inf, col_types = cols_csv)

    # select a subset
    if (.n_max == Inf)
        .n_max = NROW(csv.tb)
    csv.tb <- csv.tb[.n_start:.n_max, ]

    # find how many samples are to be read
    n_rows_csv <- NROW(csv.tb)
    # create a variable to store the number of rows
    nrow <- 0
    # create the tibble
    data.tb <- sits_tibble()
    # create a file to store the unread rows
    csv_unread.tb <- .sits_tibble_csv()
    # for each row of the input, retrieve the time series
    purrr::pmap(list(csv.tb$longitude, csv.tb$latitude, csv.tb$start_date,
                csv.tb$end_date, csv.tb$label),
                function(longitude, latitude, start_date, end_date, label){

                    row <- .sits_from_service(coverage, longitude, latitude,
                                              lubridate::as_date(start_date),
                                              lubridate::as_date(end_date),
                                              bands, prefilter, label)
                    # did we get the data?
                    if (!purrr::is_null(row)) {
                        nrow <<-  nrow + 1

                        # add the new point to the sits tibble
                        data.tb <<- dplyr::bind_rows(data.tb, row)

                        # optional - save the results to an intermediate file
                        if (.n_save != 0 && !(nrow %% .n_save)) {
                            .sits_log_data(data.tb)
                        }
                    }
                    # the point could not be read - save it in the log file
                    else {
                        csv_unread_row.tb <- tibble::tibble(
                            longitude  = longitude,
                            latitude   = latitude,
                            start_date = lubridate::as_date(start_date),
                            end_date   = lubridate::as_date(end_date),
                            label      = label
                        )
                        csv_unread.tb <<- dplyr::bind_rows(csv_unread.tb, csv_unread_row.tb)
                    }
                })


    # Have all input rows being read?
    if (nrow != n_rows_csv) {
        message("Some points could not be retrieved - see log file and csv_unread_file")
        .sits_log_CSV(csv_unread.tb, "unread_samples.csv")
    }

    return(data.tb)
}

#' @title Obtain timeSeries from WTSS server, based on a SHP file.
#' @name .sits_from_shp
#'
#' @description reads a shapefile and retrieves a sits tibble
#' containing time series from a coverage that are inside the SHP file.
#' The script uses the WTSS service, taking information about coverage, spatial and
#' temporal resolution from the WTSS configuration.
#'
#' @param shp_file        Name of a SHP file which provides the boundaries of a region of interest.
#' @param coverage        A tibble with metadata about the coverage.
#' @param start_date      The start date of the period.
#' @param end_date        The end date of the period.
#' @param bands           A string vector with the names of the bands to be retrieved.
#' @param prefilter       A string related to data correction ("0" - none, "1" - no data correction, "2" - cloud correction, "3" - no data and cloud correction).
#' @param label           A string with the label to attach to the time series.
#' @return A sits tibble.
.sits_from_shp <- function(shp_file,
                          coverage,
                          start_date = NULL,
                          end_date   = NULL,
                          bands      = NULL,
                          prefilter  = "1",
                          label      = "NoClass") {
    # test parameters
    ensurer::ensure_that(shp_file, !purrr::is_null(.) && tolower(tools::file_ext(.)) == "shp",
                         err_desc = "sits_from_shp: please provide a valid SHP file")
    # Ensure that the service is available
    .sits_check_service(coverage$service)

    # read the shapefile
    sf_shape <- sf::read_sf(shp_file)
    # find out what is the projection of the shape file
    crs1 <- sf::st_crs(sf_shape)
    # if the shapefile is not in EPSG:4326 and WGS84, transform shape into WGS84
    if (crs1$epsg != 4326) {
        sf_shape <- sf::st_transform(sf_shape, crs = 4326)
    }
    # get the bounding box
    bbox <- sf::st_bbox(sf_shape)
    # create an empty sits tibble
    shape.tb <- sits_tibble()

    # if the resolution of the coverage is expressed in meters, convert it to lat long
    if (coverage$xres > 1) {
        res <- .sits_convert_resolution(coverage)
        xres <- res["xres"]
        yres <- res["yres"]
    }
    else {
        xres <- coverage$xres
        yres <- coverage$yres
    }

    # setup the sequence of latitudes and longitudes to be searched
    longitudes <- seq(from = bbox["xmin"], to = bbox["xmax"], by = xres)
    latitudes  <- seq(from = bbox["ymin"], to = bbox["ymax"], by = yres)

    longitudes %>%
        purrr::map(function(long){
            latitudes %>%
                purrr::map(function(lat){
                    ll <- sf::st_point(c(long, lat))
                    if (1 %in% as.logical(unlist(sf::st_within(ll, sf_shape)))) {
                        row <- .sits_from_service(coverage, long, lat, start_date, end_date,
                                                  bands, prefilter, label)
                        shape.tb <<- dplyr::bind_rows(shape.tb, row)
                    }
                })
        })
    return(shape.tb)
}
