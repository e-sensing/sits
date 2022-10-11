

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
    return(.try(
        .conf("run_tests"),
        .default = FALSE
        )
    )
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
    return(.try(
        .conf("run_tests"),
        .default = FALSE
    )
    )
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
        .raster_sub_image_intersects(
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
