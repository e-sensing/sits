#' @title Create chunk file
#' @name .create_chunk_file
#' @keywords internal
#
#' @description Create a temp file name.
#'
#' @param output_dir  A character with the output directory to be concatenated.
#' @param pattern     File name pattern.
#' @param ext         File extension.
#'
#' @return A character with the file name.
.create_chunk_file <- function(output_dir,
                               pattern,
                               ext = ".tif") {

    # Try create '.sits' directory in output_dir
    path <- file.path(output_dir, ".sits")
    if (!dir.exists(path))
        dir.create(path)

    # Pre-condition
    .check_file(
        x = path,
        msg = "Cannot create '.sits' temp directory"
    )

    filename <- tempfile(tmpdir = path,
                         pattern = pattern,
                         fileext = ext)
    return(filename)
}

#' @title Informs if sits tests should run
#'
#' @name sits_run_tests
#'
#' @description
#' This function informs if sits test should run.
#' Useful to avoid running slow tests in CRAN environment.
#' Behaviour controlled by environmental variable R_CONFIG_ACTIVE_TESTS
#' @return TRUE/FALSE
#' @examples
#' if (sits_run_examples()) {
#' # recover config state
#' config_tests <- sits_run_tests()
#' # set active tests to FALSE
#' sits_config(run_tests = FALSE)
#' isFALSE(sits_run_tests())
#' # recover config state
#' # set active tests
#' sits_config(run_tests = TRUE)
#' # result should be true
#' isTRUE(sits_run_tests())
#' # restore previous state
#' sits_config(run_tests = config_tests)
#' }
#'
#' @export
sits_run_tests <- function() {
    return(.config_get("run_tests", default = FALSE))
}

#' @title Informs if sits examples should run
#'
#' @name sits_run_examples
#'
#' @description
#' This function informs if sits examples should run.
#' This is useful to avoid running slow examples in CRAN environment.
#'
#' @return A logical value
#' @examples
#' if (sits_run_examples()) {
#' # set examples to FALSE
#' sits_config(run_examples = FALSE)
#' isFALSE(sits_run_examples())
#' # recover config state
#' sits_config(run_examples = TRUE)
#' }
#'
#'

#' @export
sits_run_examples <- function() {
    return(.config_get("run_examples", default = FALSE))
}

#' @title Transform samples to wgs84
#' @name .sits_transform_samples
#' @keywords internal
#
#' @description Transforming samples points to wgs84 and replace the longitude
#' and latitude values.
#'
#' @param samples         Samples to be retrieved.
#' @param crs             A coordinate reference system of samples.
#'                        The provided crs could be a character
#'                        (e.g, "EPSG:4326" or "WGS84" or a proj4string), or a
#'                        a numeric with the EPSG code (e.g. 4326).
#'                        This parameter only works for 'csv' or data.frame'
#'                        samples. Default is 4326.
#'
#' @return A tibble with tranformed points.
.sits_transform_samples <- function(samples, crs) {

    .check_chr_within(
        x = .config_get("df_sample_columns"),
        within = colnames(samples),
        msg = "data input is not valid"
    )

    samples <- suppressWarnings(
        sf::st_transform(
            x = sits_as_sf(data = samples, crs = crs),
            crs = 4326
        )
    )
    pts_repr <- tibble::as_tibble(sf::st_coordinates(samples))
    samples[, c("longitude", "latitude")] <- pts_repr[, c("X", "Y")]

    samples <- sf::st_drop_geometry(samples)

    return(samples)
}

#' @title Filter tiles that intersects with samples
#' @name .sits_filter_intersecting_tiles
#' @keywords internal
#
#' @description Filter tiles that intersects with samples.
#'
#' @param cube     Data cube from where data is to be retrieved.
#' @param samples  Samples to be retrieved.
#'
#' @return A cube with filtered tiles.
.sits_filter_intersecting_tiles <- function(cube, samples) {
    samples_sf <- sits_as_sf(data = samples)

    are_samples_in_tiles <- slider::slide_lgl(cube, function(tile) {
        .sits_raster_sub_image_intersects(
            cube = tile,
            roi = samples_sf
        )
    })
    .check_that(
        any(are_samples_in_tiles),
        msg = "The provided tile(s) does not intersects with samples."
    )
    # filter only tiles that intersects with samples
    cube <- cube[are_samples_in_tiles, ]

    return(cube)
}
