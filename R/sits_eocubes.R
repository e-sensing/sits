#' @title Provides information about EOCubes service
#' @name sits_info_EOCubes
#' @author Rolf Simoes
#'
#' @description Obtains information about the EOCubes remotes
#' and about the cubes.
#'
#' The EOCubes package is a lightweight link data package that allows remote access to satellite
#'  image time series.
#'
#' @return An R object containing the information about the EOCubes object.
#'
#' @examples
#' \donttest{
#' # Obtain information about the coverages available
#' sits_info_EOCubes()
#' }
#' @export
sits_info_EOCubes <- function() {

    remote_name <- .sits_get_server("EOCUBES")
    remote <- EOCubes::remote(name = remote_name)

    cat(paste("-----------------------------------------------------------", "\n",sep = ""))
    cat(paste("The EOCubes remote name is '", EOCubes::remote_name(remote), "'\n", sep = ""))

    # obtains information about the coverages
    cubes.obj    <- EOCubes::list_cubes(remote = remote)
    cat(paste("Available Cubes: \n"))
    names(cubes.obj) %>%
        purrr::map(function(x) cat(paste(x, "\n", sep = "")))
    cat(paste("------------------------------------------------------------", "\n",sep = ""))
}


#' @title Obtain one timeSeries from EOCubes package and load it on a sits tibble
#' @name .sits_from_EOCubes
#'
#' @description Returns one set of time series provided by a EOCubes package
#' Given a location (lat/long), and start/end period, and the Cube name
#' retrieve a time series and include it on a sits tibble.
#'
#' @param coverage        Metadata about the coverage where the data is to be retrived.
#' @param longitude       The longitude of the chosen location.
#' @param latitude        The latitude of the chosen location.
#' @param start_date      Date with the start of the period.
#' @param end_date        Date with the end of the period.
#' @param bands           A list of string with the names of the bands of the coverage.
#' @param label           A string with the label to attach to the time series (optional).
#' @return A sits tibble.
.sits_from_EOCubes <- function(coverage,
                               longitude,
                               latitude,
                               start_date = NULL,
                               end_date   = NULL,
                               bands      = NULL,
                               label      = "NoClass") {
    # if bands are not provided, use all bands available in the coverage
    # check the bands are available
    cov_bands <- coverage$bands[[1]]
    if (purrr::is_null(bands))
        bands <- cov_bands
    else
        ensurer::ensure_that(bands, all((.) %in% cov_bands),
                             err_desc = "sits_from_EOCubes: requested bands are not available in the coverage")

    # get cube object
    cub.obj <- coverage$r_objs[[1]][[1]]

    # check start and end dates
    timeline <- coverage$timeline[[1]][[1]]
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
        ts$bands[bands] <- as.list(as.data.frame(zoo::na.spline(do.call(data.frame, ts$bands[bands]), ts$timeline)))

        # scale the time series
        ts$bands[bands] <- lapply(bands, function(band) {

            values <- ts$bands[[band]] * attr$scale[band]
            return(values)
        })

        # convert the series to a tibble
        ts.tb <- dplyr::bind_cols(list(tibble::tibble(Index = ts$timeline),
                                       tibble::as_tibble(ts$bands)))

        # create a tibble to store the WTSS data
        data.tb <- sits_tibble()

        # add one row to the tibble
        data.tb <- tibble::add_row(data.tb,
                                   longitude   = longitude,
                                   latitude    = latitude,
                                   start_date  = start_date,
                                   end_date    = end_date,
                                   label       = label,
                                   coverage    = coverage$name,
                                   time_series = list(ts.tb))

        # return the tibble with the time series
        return(data.tb)

    }, error = function(e){
        msg <- paste0("EOCubes - unable to retrieve point (", longitude, ", ", latitude, ", ", start_date," ,", end_date,")")
        .sits_log_error(msg)
        message("EOCubes - unable to retrieve point - see log file for details" )
        return(NULL)
    })
}
