

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
#' @export
sits_run_examples <- function() {
    return(.try(
        .conf("run_examples"),
        .default = FALSE
    )
    )
}
