#' @title Filter bands on a data set (tibble or cube)
#' @name sits_select
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param data       Tibble (class "sits" or class "raster_cube").
#' @param bands      Character vector with the names of the bands.
#' @param start_date Date in YYYY-MM-DD format: start date to be filtered.
#' @param end_date   Date in YYYY-MM-DD format: end date to be filtered.
#' @param ...        Additional parameters to be provided
#' @param tiles      Character vector with the names of the tiles.
#' @param dates      Character vector with sparse dates to select.
#'
#' @description      Filter only the selected bands and dates
#'                   from a set of time series or froam a data cube.
#'
#' @return           Tibble of class "sits" or class "raster_cube".
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
#'               start_date = "2000-01-01",
#'               end_date = "2030-12-31")
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
    if (!purrr::is_null(bands) && !any(is.na(bands))) {
        # sits tibble only works with non-processed cubes
        # all bands are uppercase
        bands <- toupper(bands)
        # check bands parameter
        .check_chr(bands,
                   allow_empty = FALSE,
                   allow_duplicate = FALSE,
                   len_min = 1,
                   len_max = length(sits_bands(data)),
                   msg = "Invalid bands parameter")

        # select bands from the time series
        data <- .samples_select_bands(data, bands = bands)
    }
    if (!purrr::is_null(start_date) && !purrr::is_null(end_date)) {
        # Filter dates
        start_date <- .timeline_format(start_date)
        end_date   <- .timeline_format(end_date)
        data <- .samples_filter_interval(
            data,
            start_date = start_date,
            end_date = end_date
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
                                    dates = NULL,
                                    tiles = NULL) {
    # Pre-condition
    .check_cube_files(data)
    # Filter bands
    if (!purrr::is_null(bands) && !any(is.na(bands))) {
        bands <- .band_set_case(bands)
        bands <- .default(bands, .band_samples(sits_bands(data)))
        # check bands parameter
        .check_chr(bands,
                   allow_empty = FALSE,
                   allow_duplicate = FALSE,
                   len_min = 1,
                   len_max = length(sits_bands(data)),
                   msg = "Invalid bands parameter")

        # filter the selected bands
        data <- .cube_filter_bands(cube = data, bands = bands)
    }
    # Filter by dates
    if (!purrr::is_null(dates)) {
        dates <- .timeline_format(dates)
        data <- .cube_filter_dates(cube = data, dates = dates)
    }
    # Filter by period
    if (!purrr::is_null(start_date) && !purrr::is_null(end_date)
        && !is.na(start_date) && !is.na(end_date)) {
        start_date <- .timeline_format(start_date)
        end_date   <- .timeline_format(end_date)
        data <- .cube_filter_interval(
            cube = data, start_date = start_date, end_date = end_date
        )
    }
    # Filter tiles
    if (!purrr::is_null(tiles) && !any(is.na(tiles))) {
        .check_chr_type(tiles)
        data <- .cube_filter_tiles(cube = data, tiles = tiles)
    }
    return(data)
}
#' @rdname sits_select
#'
#' @export
sits_select.patterns <- function(data, bands = NULL,
                                 start_date = NULL, end_date = NULL, ...) {
    return(sits_select.sits(data, bands, start_date, end_date))
}
#' @rdname sits_select
#' @export
sits_select.tbl_df <- function(data,...) {
    data <- tibble::as_tibble(data)
    if (all(.conf("sits_cube_cols") %in% colnames(data))) {
        data <- .cube_find_class(data)
    } else if (all(.conf("sits_tibble_cols") %in% colnames(data))) {
        class(data) <- c("sits", class(data))
    } else
        stop("Input should be a sits tibble or a data cube")
    data <- sits_select(data, ...)
    return(data)
}
#' @rdname sits_select
#' @export
sits_select.default <- function(data, ...){
    stop("input should an object of class cube or class sits")
}
