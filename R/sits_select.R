#' @title Filter bands on a data set (tibble or cube)
#'
#' @name sits_select
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param data         A sits tibble or data cube.
#' @param bands        Character vector with the names of the bands.
#'
#' @description Filter only the selected bands from a tibble or a data cube.
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
#' @return
#' For sits tibble, returns a sits tibble with the selected bands.
#' For data cube, a data cube with the selected bands.
#'
#' @export
#'
sits_select <- function(data, bands) {

    # set caller to show in errors
    .check_set_caller("sits_select")
    # get the meta-type (sits or cube)
    data <- .config_data_meta_type(data)
    UseMethod("sits_select", data)
}
#' @export
#'
sits_select.sits <- function(data, bands) {
    # bands names in SITS are uppercase
    bands <- toupper(bands)
    # pre-condition
    .check_chr_within(bands,
                      within = sits_bands(data),
                      msg = "Invalid bands values"
    )
    data <- .sits_fast_apply(data, col = "time_series", function(x) {
        dplyr::select(x, dplyr::all_of(c("#..", "Index", bands)))
    })
    return(data)
}

#' @export
#'
sits_select.sits_cube <- function(data, bands) {

    # pre-condition - cube
    .cube_check(data)

    # filter the file info
    data <- slider::slide_dfr(data, function(tile) {

        # default bands
        if (is.null(bands)) {
            bands <- .cube_bands(tile)
        }

        # pre-condition - check bands
        .cube_bands_check(tile, bands = bands)

        db_info <- .file_info(tile)
        db_info <- dplyr::filter(db_info, .data[["band"]] %in% bands)
        tile$file_info[[1]] <- db_info
        return(tile)
    })

    return(data)
}

#' @export
#'
sits_select.patterns <- function(data, bands) {
    return(sits_select.sits(data, bands))
}
