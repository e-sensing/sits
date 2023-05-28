

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
#' @examples
#' #' if (sits_run_tests()){
#'     message("use devtools::run_tests()")
#' }
#' else{
#'     message("Set the environmental variable "SITS_RUN_TESTS" to "YES"
#'     to run the tests or use devtools::run_tests()")
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
#' To run the examples, set "SITS_RUN_EXAMPLES" environment
#' variable to "YES" using
#' Sys.setenv("SITS_RUN_EXAMPLES" = "YES")
#' To come back to the default behaviour, please unset
#' the enviroment variable
#' Sys.unsetenv("SITS_RUN_EXAMPLES")
#'
#' @return A logical value
#' @examples
#' if (sits_run_examples()){
#'     message("use devtools::run_examples() to run the examples")
#' }
#' else{
#'     message("Set the environmental variable "SITS_RUN_EXAMPLES" to "YES"
#'     to run the examples")
#' }
#'
#'
#' @export
sits_run_examples <- function() {
    return(!Sys.getenv("SITS_RUN_EXAMPLES") %in% c("", "NO", "FALSE", "OFF"))
}
