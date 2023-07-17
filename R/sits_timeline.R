#' @title Get timeline of a cube or a set of time series
#' @name sits_timeline
#'
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description This function returns the timeline for a given data set, either
#'              a set of time series, a data cube, or a trained model.
#'
#' @param  data  Sits time series or a data cube.
#' @return       Vector of class Date with timeline of samples or data cube.
#'
#' @examples
#' sits_timeline(samples_modis_ndvi)
#' @export
sits_timeline <- function(data) {
    UseMethod("sits_timeline", data)
}
#' @rdname sits_timeline
#' @export
#'
sits_timeline.sits <- function(data) {
    return(data$time_series[[1]]$Index)
}
#' @rdname sits_timeline
#' @export
#'
sits_timeline.sits_model <- function(data) {
    .check_is_sits_model(data)
    samples <- .ml_samples(data)
    return(samples$time_series[[1]]$Index)
}
#' @rdname sits_timeline
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
        if (.check_warnings()) {
            warning("cube is not regular, returning all timelines",
                call. = FALSE
            )
        }
        return(timelines.lst)
    }
}
#' @rdname sits_timeline
#' @export
#'
sits_timeline.derived_cube <- function(data) {
    # return the timeline of the cube
    timeline <- .tile_timeline(data)
    return(timeline)
}
#' @rdname sits_timeline
#' @export
sits_timeline.tbl_df <- function(data) {
    if (all(.conf("sits_cube_cols") %in% colnames(data))) {
        class(data) <- c("raster_cube", class(data))
    } else if (all(.conf("sits_tibble_cols") %in% colnames(data))) {
        class(data) <- c("sits", class(data))
    } else
        stop("Input should be a sits tibble or a data cube")
    timeline <- sits_timeline(data)
    return(timeline)
}
#' @rdname sits_timeline
#' @export
#'
sits_timeline.default <- function(data) {
    data <- tibble::as_tibble(data)
    timeline <- sits_timeline(data)
    return(timeline)
}
