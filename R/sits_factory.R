#' @title Create a closure for calling functions with and without data
#' @name .sits_function_factory
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This internal function implements the factory method pattern for the SITS package
#' Its role is to create a generic interface to closures in R so that the functions
#' in the SITS package can be called in two different ways:
#' 1. They can be called directly, passing input data and parameters.
#' 2. They can be called as second-order values (parameters of another function).
#'    In the second case, the call will pass no data values and only pass the parameters for execution
#'
#' The factory pattern is used in many situations in the SITS package, to allow different alternatives
#' for filtering, pattern creation, training, and cross-validation
#'
#' @param data.tb   a SITS tibble time series with the data input to the function
#' @param fun       the function that performs some calculation on the input data
.sits_factory_function <- function (data.tb = NULL, fun) {

    # if no data is given, we prepare a function to be called as a parameter of other functions
    if (is.null(data.tb))
        result <- fun
    # ...otherwise compute the result on the input data
    else
        result <- fun(data.tb)
    return(result)
}

#' @title Create a closure for calling functions with and without data (two parameters)
#' @name .sits_function_factory2
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This internal function implements the factory method pattern for the SITS package
#' in the case of two data inputs
#' Its role is to create a generic interface to closures in R so that the functions
#' in the SITS package can be called in two different ways:
#' 1. They can be called directly, passing input data and parameters.
#' 2. They can be called as second-order values (parameters of another function).
#'    In the second case, the call will pass no data values and only pass the parameters for execution
#'
#' The factory pattern is used in many situations in the SITS package, to allow different alternatives
#' for filtering, pattern creation, training, and cross-validation
#'
#' @param data.tb   a SITS tibble time series with the first data input to the function
#' @param data2.tb   a SITS tibble time series with the second data input to the function
#' @param fun       the function that performs some calculation on the input data
.sits_factory_function2 <- function (data.tb, data2.tb, fun) {

    # if no data is given, we prepare a function to be called as a parameter of other functions
    if (is.null(data.tb) && is.null (data2.tb))
        result <- fun
    # ...otherwise compute the result on the input data sets
    else
        result <- fun(data.tb, data2.tb)

    return(result)
}

#' @title Set function arguments
#' @name .set_fun_args
#' @author Victor Maus, \email{vwmaus1@@gmail.com}
#'
#' @description Given a function and a list of arguments,
#' returns the function with new default parameters
#'
#' @param fun a function
#' @param args a list of arguments
#' @param ... arguments
#'
#' @noRd
#'
.set_fun_args = function(fun, ..., args = list(...)){
    base_formals = formals(fun)
    base_formals_names = names(base_formals)
    given_formals = args[names(args) %in% base_formals_names]
    missing_formals_names = setdiff(base_formals_names, names(args))
    new_formals = c(base_formals[missing_formals_names], given_formals)
    new_formals = new_formals[base_formals_names]
    formals(fun) = new_formals
    fun
}
