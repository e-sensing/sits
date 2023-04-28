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
#'
#' @returns
#' A vector with the names of the bands.
#'
#' @examples
#' bands <- sits_bands(point_mt_6bands)
#'
#' @export
sits_bands <- function(x) {
    # Set caller to show in errors
    .check_set_caller("sits_bands")
    # Get the meta-type (sits or cube)
    x <- .conf_data_meta_type(x)
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
    bands_lst <- slider::slide(x, function(tile) {
        bands_tile <- .tile_bands(tile)
        return(sort(bands_tile))
    })
    bands <- unique(bands_lst)
    .check_that(length(bands) == 1,
        local_msg = "tiles have different bands",
        msg = "cube is inconsistent"
    )
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
    .check_is_sits_model(x)
    bands <- .ml_bands(x)
    return(bands)
}
