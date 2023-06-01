

#' @title Informs if sits tests should run
#'
#' @name sits_run_tests
#'
#' @description
#' To run the tests, set "SITS_RUN_TESTS" environment to "YES" using
#' Sys.setenv("SITS_RUN_TESTS" = "YES")
#' To come back to the default behaviour, please set
#' Sys.setenv("SITS_RUN_TESTS" = "NO")
#' @return TRUE/FALSE
#' @examples
#' if (sits_run_tests()){
#'     message("Tests will be run")
#' } else{
#'     message(paste("Set the environmental variable SITS_RUN_TESTS to YES",
#'     "to run the tests or use devtools::run_tests()"))
#' }
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
#' To run the examples, set "SITS_RUN_EXAMPLES" to "YES" using
#' Sys.setenv("SITS_RUN_EXAMPLES" = "YES")
#' To come back to the default behaviour, please set
#' Sys.setenv("SITS_RUN_EXAMPLES" = "NO")
#'
#' @return A logical value
#' @examples
#' if (sits_run_examples()){
#'     message("use devtools::run_examples() to run the examples")
#' } else{
#'     message(paste0("Set the envvar SITS_RUN_EXAMPLES to YES",
#'     "to run the examples"))
#' }
#'
#'
#' @export
sits_run_examples <- function() {
    return(!Sys.getenv("SITS_RUN_EXAMPLES") %in% c("", "NO", "FALSE", "OFF"))
}
