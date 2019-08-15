#' @title Obtain time series from different sources
#' @name sits_get_data
#' @author Gilberto Camara
#'
#' @description Retrieve a set of time series and puts it in a "sits tibble".
#' Sits tibbles are the main structures of sits package.
#' They contain both the satellite image time series and its metadata.
#' A sits tibble is a tibble with pre-defined columns that
#' has the metadata and data for each time series. The columns are
#' <longitude, latitude, start_date, end_date, label, cube, time_series>.
#' There are two main ways of retrieving time series:
#' 1. Using a time series service and from a data cube defined based on a set of Raster Bricks. Two time series services are available:
#' (a) the Web Time Series Service (WTSS) by INPE; (b) the SATVEG service from EMBRAPA.
#' Please see \code{\link[sits]{sits_services}} for more information on thw WTSS service.
#' The URL and other parameters for access to the time series services are defined in the package
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
#'  The results is a sits tibble, which  has the metadata and data for each time series
#' <longitude, latitude, start_date, end_date, label, cube, time_series>
#'
#' @references
#' Lubia Vinhas, Gilberto Queiroz, Karine Ferreira, Gilberto Camara,
#' Web Services for Big Earth Observation Data.
#' In: XVII Brazilian Symposium on Geoinformatics, 2016, Campos do Jordao.
#' Proceedings of GeoInfo 2016. Sao Jose dos Campos: INPE/SBC, 2016. v.1. p.166-177.
#'
#' @param cube            A tibble with information about the data cube where dta is to be retrived.
#' @param file            An optional name of a file with information on the data to be retrieved (options - CSV, SHP).
#' @param longitude       Longitude of the chosen location.
#' @param latitude        Latitude of the chosen location.
#' @param start_date      An optional start of the interval for the time series in Date format ("YYYY-MM-DD").
#' @param end_date        An optional end of the interval for the time series in Date format ("YYYY-MM-DD").
#' @param bands           An optional vector with the names of the bands to be retrieved.
#' @param prefilter       An optional string ("0" - none, "1" - no data correction, "2" - cloud correction, "3" - no data and cloud correction).
#' @param label           An optional string with the label to be assigned to the time series.
#' @param epsg            An optional code for the planar projection to be used (for reading data inside shapefiles)
#' @param .n_start        An optional integer with the row on the CSV file to start reading.
#' @param .n_max          An optional integer with the maximum number of CSV samples to be read (set to Inf to read all).
#' @param .n_save         An optional number of samples to save as intermediate files (used for long reads).
#' @return A tibble with time series data and metadata.
#'
#' @examples
#' \donttest{
#' # Read a single lat long point from a WTSS server
#' wtss_cube <- sits_cube(service = "WTSS", name = "MOD13Q1")
#' point.tb <- sits_get_data (wtss_cube, longitude = -55.50563, latitude = -11.71557)
#' sits_plot(point.tb)
#'
#' # Read a set of points defined in a CSV file from a WTSS server
#' csv_file <- system.file ("extdata/samples/samples_matogrosso.csv", package = "sits")
#' points.tb <- sits_get_data (wtss_cube, file = csv_file)
#' # show the points retrieved for the WTSS server
#' sits_plot (points.tb[1:3,])
#'
#' # Read a single lat long point from the SATVEG server
#' satveg_cube <- sits_cube(service = "SATVEG", name = "terra")
#' point_satveg.tb <- sits_get_data (satveg_cube, longitude = -55.50563, latitude = -11.71557)
#' sits_plot(point_satveg.tb)
#'
#' # define a shapefile and read from the points inside it from the WTSS service
#' shp_file <- system.file("extdata/shapefiles/santa_cruz_minas.shp", package = "sits")
#' munic.tb <- sits_get_data(wtss_cube, file = shp_file)
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
#' sits_plot(point_ts)
#'
#' #' # Read a CSV file in a Raster Brick
#' csv_file <- system.file ("extdata/samples/samples_sinop_crop.csv", package = "sits")
#' points.tb <- sits_get_data (raster_cube, file = csv_file)
#' # show the points retrieved for the RASTER images
#' sits_plot (points.tb)
#' }
#' @export
sits_get_data <- function(cube,
                         file        = NULL,
                         longitude   = NULL,
                         latitude    = NULL,
                         start_date  = NULL,
                         end_date    = NULL,
                         bands       = NULL,
                         prefilter   = "1",
                         label       = "NoClass",
                         epsg        = 6842,
                         .n_start    = 1,
                         .n_max      = Inf,
                         .n_save     = 0) {
    # Ensure that the service is available
    .sits_check_service(cube[1,]$service)

    # get data based on latitude and longitude
    if (purrr::is_null(file) &&
        !purrr::is_null(latitude) && !purrr::is_null(longitude)) {
        data.tb <- .sits_from_service(cube = cube,
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
        data.tb <- .sits_from_csv(file, cube, bands, prefilter, .n_start, .n_max, .n_save)
        return(data.tb)
    }
    # get data based on SHP file
    if (!purrr::is_null(file) && tolower(tools::file_ext(file)) == "shp") {
        data.tb <- .sits_from_shp(file, cube, start_date, end_date, bands, prefilter, label, epsg)
        return(data.tb)
    }
    message(paste("No valid input to retrieve time series data!!", "\n", sep = ""))
    stop()
}



#' @title Obtain timeSeries from a data cube, based on a CSV file.
#' @name .sits_from_csv
#'
#' @description reads descriptive information about a set of
#' spatio-temporal locations from a CSV file. Then, it retrieve the time series from a data cube,
#' and stores the time series on a sits tibble for later use.
#' The CSV file should have the following column names:
#' "longitude", "latitude", "start_date", "end_date", "label"
#'
#' @param csv_file        Name of a CSV file with information <id, latitude, longitude, from, end, label>.
#' @param cube            A tibble with metadata about the data cube which contains data to be retrieved.
#' @param bands           A string vector with the names of the bands to be retrieved.
#' @param prefilter       String ("0" - none, "1" - no data correction, "2" - cloud correction, "3" - no data and cloud correction).
#' @param .n_start        Row on the CSV file to start reading (optional).
#' @param .n_max          Maximum number of samples to be read.
#' @param .n_save         Number of samples to save as intermediate files (used for long reads).
#' @return A sits tibble.
.sits_from_csv <-  function(csv_file, cube, bands, prefilter, .n_start, .n_max, .n_save) {
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
    data.tb <- .sits_tibble()
    # create a file to store the unread rows
    csv_unread.tb <- .sits_tibble_csv()
    # for each row of the input, retrieve the time series
    purrr::pmap(list(csv.tb$longitude, csv.tb$latitude, csv.tb$start_date,
                csv.tb$end_date, csv.tb$label),
                function(longitude, latitude, start_date, end_date, label){
                    row <- .sits_from_service(cube, longitude, latitude,
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
    ll_sfc <- sf::st_sfc(st_point, crs = "+init=epsg:4326")
    ll_sp <- sf::as_Spatial(ll_sfc)

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
    data.tb <- .sits_tibble()
    # add one row to the tibble
    data.tb <- tibble::add_row(data.tb,
                               longitude    = longitude,
                               latitude     = latitude,
                               start_date   = as.Date(timeline[1]),
                               end_date     = as.Date(timeline[length(timeline)]),
                               label        = label,
                               cube         = cube$name,
                               time_series  = ts.lst
    )
    return(data.tb)
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
#' @param prefilter       String (only for SATVEG) ("0" - none, "1" - no data correction, "2" - cloud correction, "3" - no data and cloud correction).
#' @param label           String with the label to attach to the time series.
#' @return A sits tibble.
.sits_from_service <- function(cube,
                               longitude,
                               latitude,
                               start_date,
                               end_date,
                               bands,
                               prefilter  = "1",
                               label = "NoClass") {

    # find out which is the service associate to the cube
    service <- .sits_cube_service(cube)

    if (service == "EOCUBES") {
        data.tb <- .sits_from_EOCubes(cube, longitude, latitude, start_date, end_date, bands, label)
        return(data.tb)
    }
    if (service == "WTSS") {
        data.tb <- .sits_from_wtss(cube, longitude, latitude, start_date, end_date, bands, label)
        return(data.tb)
    }
    if (service == "SATVEG") {
        data.tb <- .sits_from_satveg(cube, longitude, latitude, start_date, end_date, bands, prefilter, label)

        return(data.tb)
    }
    if (service == "RASTER" || service == "LOCALHOST" || service == "STACK" || service == "AWS") {
        data.tb <- .sits_from_raster(cube, longitude, latitude, start_date, end_date, bands, label)

        return(data.tb)
    }
    return(NULL)
}
#' @title Obtain timeSeries from WTSS server, based on a SHP file.
#' @name .sits_from_shp
#'
#' @description reads a shapefile and retrieves a sits tibble
#' containing time series from a data cube that are inside the SHP file.
#' The script uses the WTSS service, taking information about spatial and
#' temporal resolution from the WTSS configuration.
#'
#' @param shp_file        Name of a SHP file which provides the boundaries of a region of interest.
#' @param cube            Data cube metadata.
#' @param start_date      The start date of the period.
#' @param end_date        The end date of the period.
#' @param bands           A string vector with the names of the bands to be retrieved.
#' @param prefilter       A string related to data correction ("0" - none, "1" - no data correction, "2" - cloud correction, "3" - no data and cloud correction).
#' @param label           A string with the label to attach to the time series.
#' @param epsg             An optional code for the planar projection to be used if the shapefile is in WGS84 (for reading data inside shapefiles)
#' @return A sits tibble.
.sits_from_shp <- function(shp_file, cube, start_date, end_date, bands,
                          prefilter, label, epsg) {
    # test parameters
    ensurer::ensure_that(shp_file, !purrr::is_null(.) && tolower(tools::file_ext(.)) == "shp",
                         err_desc = "sits_from_shp: please provide a valid SHP file")
    # Ensure that the service is available
    .sits_check_service(cube$service)

    # read the shapefile
    sf_shape <- sf::read_sf(shp_file)
    # find out what is the projection of the shape file
    epsg_shp <- sf::st_crs(sf_shape)$epsg
    # if the shapefile is not in planar coordinates, convert it
    if (epsg_shp == 4326) {
        sf_shape <- sf::st_transform(sf_shape, crs = epsg)
    }
    else # if shapefile not in lat/long, use the shapefile EPSG
        epsg <- epsg_shp
    # get the bounding box
    bbox <- sf::st_bbox(sf_shape)
    # create an empty sits tibble
    shape.tb <- .sits_tibble()

    # If the resolution of the cube is expressed in latlong, convert it to planar coordinates
    res <- .sits_convert_resolution(cube)

    # setup the sequence of latitudes and longitudes to be searched
    xs <- seq(from = bbox["xmin"], to = bbox["xmax"], by = res["xres"])
    ys  <- seq(from = bbox["ymin"], to = bbox["ymax"], by = res["yres"])

    xys <- tidyr::crossing(xs, ys)
    names(xys) <- c("x", "y")
    rows.lst <-
        xys %>%
            purrr::pmap(function (x, y) {
                xy <- sf::st_point(c(x,y))
                if (1 %in% as.logical(unlist(sf::st_contains(sf_shape, xy)))) {
                        ll <- .sits_proj_to_latlong(x, y, epsg)
                        row <- .sits_from_service(cube, ll[,"X"], ll[,"Y"], start_date, end_date,
                                bands, prefilter, label)
                        return(row)
                }
            })
    shape.tb <- dplyr::bind_rows(rows.lst)
    return(shape.tb)
}
