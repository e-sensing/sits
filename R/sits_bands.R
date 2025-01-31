#' @title Get the names of the bands
#'
#' @name sits_bands
#'
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description
#' Finds the names of the bands of a set of time series or of a data cube
#'
#' @param x Valid sits tibble (time series or a cube)
#' @param value New value for the bands
#'
#' @returns
#' A vector with the names of the bands.
#'
#' @examples
#' if (sits_run_examples()) {
#'     # Create a data cube from local files
#'     data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#'     cube <- sits_cube(
#'         source = "BDC",
#'         collection = "MOD13Q1-6.1",
#'         data_dir = data_dir
#'     )
#'     # Get the bands from a daya cube
#'     bands <- sits_bands(cube)
#'     # Get the bands from a sits tibble
#'     bands <- sits_bands(samples_modis_ndvi)
#'     # Get the bands from patterns
#'     bands <- sits_bands(sits_patterns(samples_modis_ndvi))
#'     # Get the bands from ML model
#'     rf_model <- sits_train(samples_modis_ndvi, sits_rfor())
#'     bands <- sits_bands(rf_model)
#'     # Set the bands for a SITS time series
#'     sits_bands(samples_modis_ndvi) <- "NDVI2"
#'     # Set the bands for a SITS cube
#'     sits_bands(cube) <- "NDVI2"
#'}
#' @export
sits_bands <- function(x) {
    .check_set_caller("sits_bands")
    UseMethod("sits_bands", x)
}

#' @rdname sits_bands
#' @export
sits_bands.sits <- function(x) {
    return(setdiff(names(.tibble_time_series(x)), "Index"))
}
#' @rdname sits_bands
#' @export
sits_bands.raster_cube <- function(x) {
    # set caller to show in errors
    .check_set_caller("sits_bands")
    bands_lst <- slider::slide(x, function(tile) {
        bands_tile <- .tile_bands(tile)
        return(sort(bands_tile))
    })
    bands <- unique(bands_lst)
    .check_that(length(bands) == 1)
    return(unlist(bands))
}
#' @rdname sits_bands
#' @export
sits_bands.patterns <- function(x) {
    return(sits_bands.sits(x))
}
#' @rdname sits_bands
#' @export
sits_bands.sits_model <- function(x) {
    bands <- .ml_bands(x)
    return(bands)
}
#' @rdname sits_bands
#' @export
sits_bands.default <- function(x) {
    stop(.conf("messages", "sits_bands_default"))
}
#' @rdname sits_bands
#' @export
`sits_bands<-` <- function(x, value) {
    # set caller to show in errors
    .check_set_caller("sits_bands_assign")
    .check_chr(value, len_min = 1)
    value <- toupper(value)
    UseMethod("sits_bands<-", x)
}

#' @rdname sits_bands
#' @export
`sits_bands<-.sits` <- function(x, value) {
    bands <- .samples_bands(x)
    .check_that(length(bands) == length(value))
    x <- .apply(x, col = "time_series", fn = function(x) {
        names(x) <- c("Index", value, "#..")
        return(x)
    })
    return(x)
}
#' @rdname sits_bands
#' @export
`sits_bands<-.raster_cube` <- function(x, value) {
    bands <- .cube_bands(x)
    # precondition
    .check_that(length(bands) == length(value))
    x <- slider::slide_dfr(x, function(tile) {
        .tile_bands(tile) <- value
        tile
    })
    return(x)
}
#' @rdname sits_bands
#' @export
`sits_bands<-.default` <- function(x, value) {
    stop(.conf("messages", "sits_bands_assign_default"))
}
