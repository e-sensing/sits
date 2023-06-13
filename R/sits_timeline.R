#' @title Get timeline of a cube or a set of time series
#'
#' @name sits_timeline
#'
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function returns the timeline for a given data set, either
#'              a set of time series, a data cube, or a trained model.
#'
#' @param  data     either a sits tibble, a data cube, or a trained model.
#'
#' @return      Timeline of sample set or of data cube.
#'
#' @examples
#' sits_timeline(samples_modis_ndvi)
#'
#' @export
#'
sits_timeline <- function(data) {
    .check_set_caller("sits_timeline")
    # get the meta-type (sits or cube)
    data <- .conf_data_meta_type(data)

    UseMethod("sits_timeline", data)
}

#' @export
#'
sits_timeline.sits <- function(data) {
    return(data$time_series[[1]]$Index)
}

#' @export
#'
sits_timeline.sits_model <- function(data) {
    .check_is_sits_model(data)
    samples <- .ml_samples(data)
    return(samples$time_series[[1]]$Index)
}

#' @export
#'
sits_timeline.raster_cube <- function(data) {

    # pick the list of timelines
    timelines.lst <- slider::slide(data, function(tile) {
        timeline_tile <- .tile_timeline(tile)
        return(timeline_tile)
    })
    names(timelines.lst) <- data$tile
    timeline_unique <- unname(unique(timelines.lst))

    if (length(timeline_unique) == 1) {
        return(timeline_unique[[1]])
    } else {
        if (.check_warnings())
            warning("cube is not regular, returning all timelines",
                    call. = FALSE
            )
        return(timelines.lst)
    }
}

#' @export
#'
sits_timeline.derived_cube <- function(data) {
    # return the timeline of the cube
    timeline <- .tile_timeline(data)
    return(timeline)
}
