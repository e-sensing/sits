

#' @title Informs if sits tests should run
#'
#' @name sits_run_tests
#'
#' @description
#' This function informs if sits examples should run.
#' To run the examples, set "SITS_RUN_TESTS" environment
#' variable to "YES" using
#' Sys.setenv("SITS_RUN_TESTS" = "YES")
#' To come back to the default behaviour, please unset
#' the enviroment variable
#' Sys.unsetenv("SITS_RUN_TESTS")
#' @return TRUE/FALSE
#'
#' @export
sits_run_tests <- function() {
    return(!Sys.getenv("SITS_RUN_TESTS") %in% c("", "NO", "FALSE", "OFF"))
}

#' @title Informs if sits examples should run
#'
#' @name sits_run_examples
#'
#' @description
#' This function informs if sits examples should run.
#' To run the examples, set "SITS_RUN_EXAMPLES" environment
#' variable to "YES" using
#' Sys.setenv("SITS_RUN_EXAMPLES" = "YES")
#' To come back to the default behaviour, please unset
#' the enviroment variable
#' Sys.unsetenv("SITS_RUN_EXAMPLES")
#'
#' @return A logical value
#' @export
sits_run_examples <- function() {
    return(!Sys.getenv("SITS_RUN_EXAMPLES") %in% c("", "NO", "FALSE", "OFF"))
}
