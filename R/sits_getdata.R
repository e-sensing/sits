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
#' point_coverage <- sits_getdata(raster_cov, longitude = -55.554, latitude = -11.525)
#' sits_plot(point_coverage)
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
        .sits_log_csv(csv_unread.tb, "unread_samples.csv")
    }

    return(data.tb)
}

#' @title Extract a time series from a ST raster data set
#' @name .sits_from_raster
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Reads metadata about a raster data set to retrieve a set of
#' time series.
#'
#' @param coverage        A tibble with metadata describing a raster coverage.
#' @param longitude       A double with the longitude of the chosen location.
#' @param latitude        A double with the latitude of the chosen location.
#' @param start_date      A date with the start of the period.
#' @param end_date        A date with the end of the period.
#' @param label           A string with the label to attach to the time series.
#' @return A sits tibble with the time series.
.sits_from_raster <- function(coverage,
                              longitude,
                              latitude,
                              start_date = NULL,
                              end_date  = NULL,
                              label = "NoClass"){

    # ensure metadata tibble exists
    ensurer::ensure_that(coverage, NROW(.) >= 1,
                         err_desc = "sits_from_raster: need a valid metadata for coverage")

    timeline <- coverage$timeline[[1]][[1]]

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
    bands <- unlist(coverage$bands)
    missing_values <- unlist(coverage$missing_values)
    scale_factors  <- unlist(coverage$scale_factors)
    nband <- 0

    # transform longitude and latitude to an sp Spatial Points* (understood by raster)
    st_point <- sf::st_point(c(longitude, latitude))
    ll_sfc <- sf::st_sfc(st_point, crs = "+init=epsg:4326")
    ll_sp <- sf::as_Spatial(ll_sfc)

    # An input raster brick contains several files, each corresponds to a band
    values.lst <- coverage$r_objs[[1]] %>%
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
    data.tb <- sits_tibble()
    # add one row to the tibble
    data.tb <- tibble::add_row(data.tb,
                               longitude    = longitude,
                               latitude     = latitude,
                               start_date   = as.Date(timeline[1]),
                               end_date     = as.Date(timeline[length(timeline)]),
                               label        = label,
                               coverage     = coverage$name,
                               time_series  = ts.lst
    )
    return(data.tb)
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
                                     bands = bands,
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
