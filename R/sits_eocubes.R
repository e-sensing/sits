
#' @title Uses the EOCUBES service to provide information about a data cube
#' @name .sits_eocubes_cube
#'
#' @description Creates a tibble with metadata about a data cube.
#'
#' @param remote.obj Remote object.
#' @param service    Name of the data service.
#' @param URL        URL of the service provider.
#' @param name       Name of the cube
#' @param bands      Bands of the cube.
#' @param tiles      Filter tiles by prefix name.
#' @param geom       Geometry to filter tiles.
#' @param from       Start date to be filtered.
#' @param to         End date to be filtered.
.sits_eocubes_cube <- function(remote.obj, service, URL, name, bands, tiles, geom, from, to) {

    # obtains information about the available cubes
    cubes.vec    <- names(EOCubes::list_cubes(remote.obj))

    # is the cube in the list of cubes?
    ensurer::ensure_that(name, (.) %in% cubes.vec,
                         err_desc = ".sits_cube_EOCUBES: cube is not available in the EOCubes remote")

    # describe the cube
    cub.obj <- EOCubes::cube(name = name, remote = remote.obj)

    # filter cube
    cub.obj <- EOCubes::cube_filter(cube = cub.obj,
                                    tiles = EOCubes::tiles_which(cub.obj, prefix = tiles, geom = geom),
                                    from = from, to = to)

    # verify if the filter returned tiles
    ensurer::ensure_that(cub.obj, length(EOCubes::list_tiles(.)) > 0,
                         err_desc = ".sits_cube_EOCUBES: cube filter returned no tile.")

    # temporal extent
    timeline <- lubridate::as_date(c(EOCubes::cube_dates_info(cub.obj)$from,
                                     EOCubes::cube_dates_info(cub.obj)$to))

    # retrieve information about the bands
    attr <- EOCubes::cube_bands_info(cub.obj)

    bands.vec <- EOCubes::cube_bands(cub.obj)

    # verify if requested bands is in provided bands
    if (purrr::is_null(bands))
        bands <- bands.vec
    ensurer::ensure_that(bands.vec, all(bands %in% .),
                         err_desc = ".sits_cube_EOCUBES: requested band not provided by EOCubes remote.")

    b <- match(bands, bands.vec)
    bands.vec <- bands.vec[b]

    missing_values <- attr$fill[b]
    scale_factors  <- attr$scale[b]
    minimum_values <- attr$min[b]
    maximum_values <- attr$max[b]

    # Spatial extent
    cub_bbox <- EOCubes::cube_bbox(cub.obj)
    xmin <- cub_bbox[1] # xmin
    ymin <- cub_bbox[2] # ymin
    xmax <- cub_bbox[3] # xmax
    ymax <- cub_bbox[4] # ymax

    # Spatial resolution
    raster_info <- EOCubes::cube_raster_info(cub.obj)
    xres <- raster_info$resolution$x
    yres <- raster_info$resolution$y

    # Size (rows and cols)
    nrows <- raster_info$size$y
    ncols <- raster_info$size$x

    # Projection CRS
    crs <- EOCubes::cube_crs(cub.obj)

    #labels
    labels <- c("NoClass")

    # temporary fix until EOCubes is fully implemented
    satellite <- "TERRA"
    sensor    <- "MODIS"

    # create a tibble to store the metadata
    cube <- .sits_cube_create(service, URL, satellite, sensor, name, bands.vec, labels,
                              scale_factors, missing_values, minimum_values, maximum_values,
                              list(timeline), nrows, ncols, xmin, xmax, ymin, ymax,
                              xres, yres, crs)

    # return the tibble with cube info
    return(cube)
}

#' @title Obtain one timeSeries from EOCubes package and load it on a sits tibble
#' @name .sits_from_EOCubes
#'
#' @description Returns one set of time series provided by a EOCubes package
#' Given a location (lat/long), and start/end period, and the Cube name
#' retrieve a time series and include it on a sits tibble.
#'
#' @param cube            Metadata about the data cube to be retrived.
#' @param longitude       The longitude of the chosen location.
#' @param latitude        The latitude of the chosen location.
#' @param start_date      Date with the start of the period.
#' @param end_date        Date with the end of the period.
#' @param bands           A list of string with the names of the bands of the data cube.
#' @param label           A string with the label to attach to the time series (optional).
#' @return A sits tibble.
.sits_from_EOCubes <- function(cube,
                               longitude,
                               latitude,
                               start_date = NULL,
                               end_date   = NULL,
                               bands      = NULL,
                               label      = "NoClass") {
    # if bands are not provided, use all bands available in the cube
    # check the bands are available
    cube_bands <- .sits_cube_bands(cube)
    if (purrr::is_null(bands))
        bands <- cube_bands
    else
        ensurer::ensure_that(bands, all((.) %in% cube_bands),
                             err_desc = "sits_from_EOCubes: requested bands are not available in the cube")

    # get cube object
    cub.obj <- .sits_cube_robj(cube)

    # check start and end dates
    timeline <- sits_timeline(cube)
    if (purrr::is_null(start_date))
        start_date <- lubridate::as_date(timeline$from)
    if (purrr::is_null(end_date))
        end_date  <- lubridate::as_date(timeline$to)

    # try to get a time series from the EOCubes package
    tryCatch({

        # retrieve the time series from the package
        ts <- EOCubes::time_series(cube = cub.obj,
                                   longitude = longitude,
                                   latitude = latitude,
                                   bands = bands,
                                   from = start_date,
                                   to = end_date,
                                   crs = 4326)

        # retrieve the time series information
        ts <- ts[[1]][[1]]

        # determine the missing value for each band

        # retrieve information about the bands
        attr <- EOCubes::cube_bands_info(cub.obj)

        # update missing values to NA
        ts$bands[bands] <- lapply(bands, function(band) {

            values <- ts$bands[[band]]
            values[values == attr$fill[band]] <- NA
            return(values)
        })

        # interpolate missing values
        ts$bands[bands] <- as.list(as.data.frame(zoo::na.spline(do.call(data.frame,
                                                                        ts$bands[bands]), ts$timeline)))

        # scale the time series
        ts$bands[bands] <- lapply(bands, function(band) {

            values <- ts$bands[[band]] * attr$scale[band]
            return(values)
        })

        # convert the series to a tibble
        ts.tb <- dplyr::bind_cols(list(tibble::tibble(Index = ts$timeline),
                                       tibble::as_tibble(ts$bands)))

        # create a tibble to store the WTSS data
        data <- .sits_tibble()

        # add one row to the tibble
        data <- tibble::add_row(data,
                                   longitude   = longitude,
                                   latitude    = latitude,
                                   start_date  = start_date,
                                   end_date    = end_date,
                                   label       = label,
                                   cube        = cube$name,
                                   time_series = list(ts.tb))

        # return the tibble with the time series
        return(data)

    }, error = function(e){
        msg <- paste0("EOCubes - unable to retrieve point (", longitude, ", ", latitude, ", ", start_date," ,", end_date,")")
        .sits_log_error(msg)
        message("EOCubes - unable to retrieve point - see log file for details" )
        return(NULL)
    })
}

#' @title Check that the EOCUBES service parameter are valid
#' @name .sits_eocubes_check
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param URL           URL of the EOCUBES service.
#' @return eocubes.obj  R object for the EOCUBES service
.sits_eocubes_check <- function(URL) {
    check <- tryCatch({
        # can we connect to the EOCUBES service?
        eocubes.obj   <- EOCubes::remote(URL)

    }, error = function(e){
        msg <- paste0("EOCubes remote service not available at URL ", URL)
        .sits_log_error(msg)
        message(msg)
        return(FALSE)
    })
    # did we get an error message?
    if (!inherits(check, "error"))
        return(TRUE)
    else
        return(FALSE)
}
