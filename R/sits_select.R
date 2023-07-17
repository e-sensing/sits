#' @title Filter bands on a data set (tibble or cube)
#' @name sits_select
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param data       Sits tibble or data cube.
#' @param bands      Character vector with the names of the bands.
#' @param start_date Date in YYYY-MM-DD format: start date to be filtered.
#' @param end_date   Date in YYYY-MM-DD format: end date to be filtered.
#' @param tiles      Character vector with the names of the tiles.
#' @param ...        Additional parameters to be provided in the select
#'                   function.
#'
#' @description Filter only the selected bands from a tibble or a data cube.
#'
#' @return           Data.frame with a filtered sits tibble or data cube.
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
#' # select start and end date
#' point_2010 <- sits_select(point_mt_6bands,
#'               start_date = "2010-01-01",
#'               end_date = "2010-12-31")
#'
#' @export
sits_select <- function(data,
                        bands = NULL,
                        start_date = NULL,
                        end_date = NULL, ...) {
    # set caller to show in errors
    .check_set_caller("sits_select")
    # check data
    .check_valid(data)
    if (!is.null(bands)) {
        # check bands parameter
        .check_chr(bands,
                   allow_na = FALSE,
                   allow_empty = TRUE,
                   allow_duplicate = FALSE,
                   len_min = 1,
                   len_max = length(sits_bands(data)),
                   msg = "Invalid bands parameter")
        # pre-condition
        .check_chr_within(bands,
                          within = sits_bands(data),
                          msg = "Invalid bands values"
        )
    }
    # check validity of start and end dates
    if (!is.null(start_date))
        .check_date_parameter(start_date)
    if (!is.null(end_date))
        .check_date_parameter(end_date)
    # get the meta-type (sits or cube)
    UseMethod("sits_select", data)
}
#' @rdname sits_select
#'
#' @export
sits_select.sits <- function(data,
                             bands = NULL,
                             start_date = NULL,
                             end_date = NULL, ...) {
    # Pre-condition
    .check_samples_ts(data)
    # Filter bands
    if (!is.null(bands)) {
        # bands names in SITS are uppercase
        bands <- .band_samples(bands)
        data <- .samples_select_bands(data, bands = bands)
    }
    # Filter dates
    if (!is.null(start_date) || !is.null(end_date)) {
        data <- .samples_filter_interval(
            data,
            start_date = start_date, end_date = end_date
        )
    }
    return(data)
}

#' @rdname sits_select
#'
#' @export
sits_select.raster_cube <- function(data,
                                    bands = NULL,
                                    start_date = NULL,
                                    end_date = NULL, ...,
                                    tiles = NULL) {
    # Pre-condition
    .check_is_raster_cube(data)
    # Filter bands
    if (!is.null(bands)) {
        # bands names in SITS are uppercase
        bands <- .band_samples(bands)
        data <- .cube_filter_bands(cube = data, bands = bands)
    }
    # Filter dates
    if (!is.null(start_date) || !is.null(end_date)) {
        start_date <- .default(start_date, .cube_start_date(data))
        end_date <- .default(end_date, .cube_end_date(data))

        data <- .cube_filter_interval(
            cube = data, start_date = start_date, end_date = end_date
        )
    }
    # Filter tiles
    if (!is.null(tiles)) {
        .check_chr_type(tiles)
        data <- .cube_filter_tiles(cube = data, tiles = tiles)
    }
    return(data)
}
#' @rdname sits_select
#'
#' @export
sits_select.patterns <- function(data, bands, ...) {
    return(sits_select.sits(data, bands))
}
#' @rdname sits_bands
#' @export
sits_select.tbl_df <- function(data, ...) {
    if (all(.conf("sits_cube_cols") %in% colnames(data))) {
        class(data) <- c("raster_cube", class(data))
    } else if (all(.conf("sits_tibble_cols") %in% colnames(data))) {
        class(data) <- c("sits", class(data))
    } else
        stop("Input should be a sits tibble, data cube, patterns, or model")
    data <- sits_select(data, ...)
    return(data)
}
#' @rdname sits_bands
#' @export
sits_select.default <- function(data, ...){
    data <- tibble::as_tibble(data)
    data <- sits_select(data, ...)
    return(data)
}
