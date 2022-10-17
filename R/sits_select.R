#' @title Filter bands on a data set (tibble or cube)
#'
#' @name sits_select
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param data         A sits tibble or data cube.
#' @param bands        Character vector with the names of the bands.
#' @param tiles        Character vector with the names of the tiles.
#' @param ...          Additional parameters to be provided in the select
#'  function.
#'
#' @description Filter only the selected bands from a tibble or a data cube.
#'
#' @return
#' For sits tibble, returns a sits tibble with the selected bands.
#' For data cube, a data cube with the selected bands.
#'
#' @examples
#' # Retrieve a set of time series with 2 classes
#' data(cerrado_2classes)
#' # Print the original bands
#' sits_bands(cerrado_2classes)
#' # Select only the NDVI band
#' data <- sits_select(cerrado_2classes, bands = c("NDVI"))
#' # Print the labels of the resulting tibble
#' sits_bands(data)
#'
#' @export
#'
sits_select <- function(data, bands, ...) {

    # set caller to show in errors
    .check_set_caller("sits_select")
    # get the meta-type (sits or cube)
    data <- .conf_data_meta_type(data)
    UseMethod("sits_select", data)
}

#' @rdname sits_select
#'
#' @export
sits_select.sits <- function(data, bands, ...) {

    # bands names in SITS are uppercase
    bands <- toupper(bands)
    # pre-condition
    .check_chr_within(bands,
        within = sits_bands(data),
        msg = "Invalid bands values"
    )
    data <- .sits_select_bands(samples = data, bands = bands)
    return(data)
}

#' @rdname sits_select
#'
#' @export
sits_select.raster_cube <- function(data, bands, ..., tiles = NULL) {

    # filter tiles
    if (!is.null(tiles)) {
        data <- .cube_filter_tiles(cube = data, tiles = tiles)
    }

    # filter bands
    data <- .cube_filter_bands(cube = data, bands = bands)

    return(data)
}

#' @rdname sits_select
#'
#' @export
sits_select.patterns <- function(data, bands, ...) {
    return(sits_select.sits(data, bands))
}
