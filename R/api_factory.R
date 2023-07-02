#' @title Create a closure for calling functions with and without data
#' @name .factory_function
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @keywords internal
#' @noRd
#'
#' @description This function implements the factory method pattern.
#' Its creates a generic interface to closures in R so that the functions
#' in the sits package can be called in two different ways:
#' 1. Called directly, passing input data and parameters.
#' 2. Called as second-order values (parameters of another function).
#'    In the second case, the call will pass no data values
#'    and only pass the parameters for execution
#'
#' The factory pattern is used in many situations in the sits package,
#' to allow different alternatives
#' for filtering, pattern creation, training, and cross-validation
#'
#' Please see the chapter "Technical Annex" in the sits book for details.
#'
#' @param data      Input data.
#' @param fun       Function that performs calculation on the input data.
#' @return          A closure that encapsulates the function applied to data.
#'
.factory_function <- function(data, fun) {
    # if no data is given, we prepare a
    # function to be called as a parameter of other functions
    if (purrr::is_null(data)) {
        result <- fun
    } else {
        # ...otherwise compute the result on the input data
        result <- fun(data)
    }
    return(result)
}
