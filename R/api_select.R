#' @title Select bands from cube
#' @noRd
#' @param data    Raster data cube
#' @param bands   Required bands
#' @return cube with selected bands
.select_raster_bands <- function(data, bands) {
    if (.has(bands) && !anyNA(bands)) {
        bands <- .band_set_case(bands)
        bands <- .default(bands, .band_samples(.cube_bands(data)))
        # check bands parameter
        .check_chr_parameter(
            bands,
            allow_empty = FALSE,
            allow_duplicate = FALSE,
            len_min = 1L,
            len_max = length(.cube_bands(data))
        )

        # filter the selected bands
        data <- .cube_filter_bands(cube = data, bands = bands)
    }
    data
}
#' @title Select dates from cube
#' @noRd
#' @param data    Raster data cube
#' @param dates   Required bands
#' @return cube with selected dates
.select_raster_dates <- function(data, dates) {
    if (.has(dates)) {
        dates <- .timeline_format(dates)
        data <- .cube_filter_dates(cube = data, dates = dates)
    }
    data
}
#' @title Select period from cube
#' @noRd
#' @param data         Raster data cube
#' @param start_date   Start date
#' @param end_date     End date
#' @return cube with selected period
.select_raster_interval <- function(data, start_date, end_date) {
    if (.has(start_date) && .has(end_date) &&
        !is.na(start_date) && !is.na(end_date)) {
        start_date <- .timeline_format(start_date)
        end_date <- .timeline_format(end_date)
        data <- .cube_filter_interval(
            cube = data, start_date = start_date, end_date = end_date
        )
    }
    data
}
#' @title Select tiles from cube
#' @noRd
#' @param data         Raster data cube
#' @param tiles        Tiles to be selected
#' @return cube with selected tiles
.select_raster_tiles <- function(data, tiles) {
    if (.has(tiles) && !anyNA(tiles)) {
        .check_chr_parameter(tiles)
        data <- .cube_filter_tiles(cube = data, tiles = tiles)
    }
    data
}
#' @title Select tiles from cube
#' @noRd
#' @param data       Data cube.
#' @param bands      Character vector with the names of the bands.
#' @param start_date Date in YYYY-MM-DD format: start date to be filtered.
#' @param end_date   Date in YYYY-MM-DD format: end date to be filtered.
#' @param dates      Character vector with sparse dates to select.
#' @param tiles      Character vector with the names of the tiles.
#'
#' @return Selected lines of the data cube tibble
.select_raster_cube <- function(data,
                                bands = NULL,
                                start_date = NULL,
                                end_date = NULL,
                                dates = NULL,
                                tiles = NULL) {
    # Filter bands
    data <- .select_raster_bands(data, bands)
    # Filter by dates
    data <- .select_raster_dates(data, dates)
    # Filter by interval
    data <- .select_raster_interval(data, start_date, end_date)
    # Filter tiles
    data <- .select_raster_tiles(data, tiles)
    return(data)
}

#' @title Select samples from sits
#' @noRd
#' @param samples    Tibble with time series.
#' @param bands      Character vector with the names of the bands.
#' @param start_date Date in YYYY-MM-DD format: start date to be filtered.
#' @param end_date   Date in YYYY-MM-DD format: end date to be filtered.
#' @param dates      Character vector with sparse dates to select.
#' @param labels     Character vector with the names of the labels.
#'
#' @return Selected lines of the data cube tibble
.select_sits <- function(samples,
                         bands = NULL,
                         start_date = NULL,
                         end_date = NULL,
                         dates = NULL,
                         labels = NULL) {
    # Select by bands
    if (.has(bands)) {
        bands <- toupper(bands)
        .check_samples_bands(samples, bands)
        samples <- .samples_select_bands(samples, bands)
    }
    # Select by start and end dates
    if (.has(start_date) && .has(end_date)) {
        start_date <- .timeline_format(start_date)
        end_date <- .timeline_format(end_date)
        .check_samples_dates_range(samples, c(start_date, end_date))
        samples <- .samples_select_interval(
            samples = samples,
            start_date = start_date,
            end_date = end_date
        )
    }
    # Select by dates
    if (.has(dates)) {
        dates <- .as_date(dates)
        .check_samples_dates(samples, dates)
        samples <- .samples_select_dates(samples, dates)
    }
    # Select by labels
    if (.has(labels)) {
        .check_chr_within(
            x = labels,
            within = .samples_labels(samples)
        )
        samples <- .samples_select_labels(samples, labels)
    }
    return(samples)
}
