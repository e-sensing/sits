#' @title Create a closure for calling functions with and without data
#' @name .sits_function_factory
#' @keywords internal
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This internal function implements the factory method pattern for the sits package
#' Its role is to create a generic interface to closures in R so that the functions
#' in the sits package can be called in two different ways:
#' 1. They can be called directly, passing input data and parameters.
#' 2. They can be called as second-order values (parameters of another function).
#'    In the second case, the call will pass no data values and only pass the parameters for execution
#'
#' The factory pattern is used in many situations in the sits package, to allow different alternatives
#' for filtering, pattern creation, training, and cross-validation
#'
#' @param data      Tibble with time series data and metadata.
#' @param fun       The function that performs some calculation on the input data.
.sits_factory_function <- function(data, fun) {
    # if no data is given, we prepare a
    # function to be called as a parameter of other functions
    if (purrr::is_null(data))
        result <- fun
    # ...otherwise compute the result on the input data
    else
        result <- fun(data)
    return(result)
}
