#' @title Filter a data set (tibble or cube) for bands, tiles, and dates
#' @name sits_select
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#'
#' @param data       Tibble with time series or data cube.
#' @param bands      Character vector with the names of the bands.
#' @param start_date Date in YYYY-MM-DD format: start date to be filtered.
#' @param end_date   Date in YYYY-MM-DD format: end date to be filtered.
#' @param ...        Additional parameters to be provided
#' @param tiles      Character vector with the names of the tiles.
#' @param dates      Character vector with sparse dates to select.
#'
#' @description      Filter only the selected bands and dates
#'                   from a set of time series or from a data cube.
#'
#' @return           Tibble with time series or data cube.
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
sits_select <- function(data, ...) {
    # set caller to show in errors
    .check_set_caller("sits_select")
    # check data
    .check_na_null_parameter(data)
    # get the meta-type (sits or cube)
    UseMethod("sits_select", data)
}
#' @rdname sits_select
#'
#' @export
sits_select.sits <- function(data, ...,
                             bands = NULL,
                             start_date = NULL,
                             end_date = NULL) {
    # Pre-condition
    .check_samples_ts(data)
    # Filter bands
    if (.has(bands) && !anyNA(bands)) {
        # sits tibble only works with non-processed cubes
        # all bands are uppercase
        bands <- toupper(bands)
        # check bands parameter
        .check_chr_parameter(bands,
                   allow_empty = FALSE,
                   allow_duplicate = FALSE,
                   len_min = 1L,
                   len_max = length(.samples_bands(data))
        )

        # select bands from the time series
        data <- .samples_select_bands(data, bands = bands)
    }
    if (.has(start_date) && .has(end_date)) {
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
sits_select.raster_cube <- function(data, ...,
                                    bands = NULL,
                                    start_date = NULL,
                                    end_date = NULL,
                                    dates = NULL,
                                    tiles = NULL) {
    # Call internal function
    data <- .select_raster_cube(
        data = data,
        bands = bands,
        start_date = start_date,
        end_date = end_date,
        dates = dates,
        tiles = tiles
    )
    return(data)
}
#' @rdname sits_select
#' @export
sits_select.default <- function(data, ...) {
    data <- tibble::as_tibble(data)
    if (all(.conf("sits_cube_cols") %in% colnames(data)))
        data <- .cube_find_class(data)
    else if (all(.conf("sits_tibble_cols") %in% colnames(data)))
        class(data) <- c("sits", class(data))
    else
        stop(.conf("messages", "sits_select"))
    sits_select(data, ...)
}
