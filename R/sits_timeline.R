#' @title Get timeline of a cube or a set of time series
#' @name sits_timeline
#'
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description This function returns the timeline for a given data set, either
#'              a set of time series, a data cube, or a trained model.
#'
#' @param  data  Tibble of class "sits" or class "raster_cube"
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
    .samples_timeline(data)
}
#' @rdname sits_timeline
#' @export
#'
sits_timeline.sits_model <- function(data) {
    .check_is_sits_model(data)
    samples <- .ml_samples(data)
    as.Date(samples[["time_series"]][[1]][["Index"]])
}
#' @rdname sits_timeline
#' @export
#'
sits_timeline.raster_cube <- function(data) {
    .check_set_caller("sits_timeline_raster_cube")
    # pick the list of timelines
    timelines_lst <- slider::slide(data, function(tile) {
        .tile_timeline(tile)
    })
    names(timelines_lst) <- data[["tile"]]
    timeline_unique <- unname(unique(timelines_lst))

    if (length(timeline_unique) == 1) {
        timeline_unique[[1]]
    } else {
        # warning if there is more than one timeline
        .message_warnings_timeline_cube()
        timelines_lst
    }
}
#' @rdname sits_timeline
#' @export
#'
sits_timeline.derived_cube <- function(data) {
    # return the timeline of the cube
    timeline <- .tile_timeline(data)
    timeline
}
#' @rdname sits_timeline
#' @export
sits_timeline.tbl_df <- function(data) {
    data <- tibble::as_tibble(data)
    if (all(.conf("sits_cube_cols") %in% colnames(data)))
        data <- .cube_find_class(data)
    else if (all(.conf("sits_tibble_cols") %in% colnames(data)))
        class(data) <- c("sits", class(data))
    else
        stop(.conf("messages", "sits_timeline_default"))
    timeline <- sits_timeline(data)
    timeline
}
#' @rdname sits_timeline
#' @export
#'
sits_timeline.default <- function(data) {
    data <- tibble::as_tibble(data)
    timeline <- sits_timeline(data)
    timeline

}
