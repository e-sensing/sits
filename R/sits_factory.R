#' @title Create a closure for calling functions with and without data
#' @name .sits_function_factory
#' @keywords internal
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This internal function implements the factory method pattern.
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
#' @param data      Tibble with time series data and metadata.
#' @param fun       The function that performs calculation on the input data.
.sits_factory_function <- function(data, fun) {
    # if no data is given, we prepare a
    # function to be called as a parameter of other functions
    if (purrr::is_null(data)) {
        result <- fun
    } # ...otherwise compute the result on the input data
    else {
        result <- fun(data)
    }
    return(result)
}
#' @title Create a closure to generate new data from data and expressions
#' @name .sits_expr_new
#' @keywords internal
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description This internal function implements the factory expression
#' method.
#' Its creates a function that process data using expressions
#'
#' This factory can be used in many situations in the sits package,
#' to allow user defined expressions to process satellite data.
#'
#' @param ...      Named parameters containing any valid expression to be
#'                 applied later on actual data.
#'
.sits_expr_new <- function(...) {

    # convert expressions to language symbols
    .dots <- substitute(list(...))

    # precondition 1: check if all expressions are named
    assertthat::assert_that(
        all(names(.dots[-1]) != ""),
        msg = ".sits_expr_closure: all expressions must be named"
    )

    # prepare closure result
    fn <- function(data, old_fn = NULL) {

        # evaluate the expression
        if (!is.null(old_fn)) {
            data <- utils::modifyList(old_fn(data))
        }

        return(eval(.dots, envir = data))
    }

    return(fn)
}

#
# sits_compute_bands <- function(cube, ...) {
#
#     user_function <- .sits_expr_new(...)
#
#     # test
#     data <- .sits_get_data_test(cube)
#
#     tryCatch(user_function(data, attr(cube, ".eval_fn")),
#              erro = function(e) {
#
#              })
#
#     attr(cube, ".eval_fn") <- function(data) {
#         user_function(data, attr(cube, ".eval_fn"))
#     }
#
#     return(data)
# }
#
# ndvi_function <- .sits_expr_new(NDVI = (nir - red) / (nir + red) * 10000,
#                                 EVI  = (mir - red + nir) * 2 / (mir + nir) * 10000)
#
# data <- list(mir = c(7000, 3000, 4000, 8000), red = c(800, 300, 400, 500),
#              nir = c(4000, 5000, 3000, 8000))
#
# data <- modifyList(data, ndvi_function(data))
#
# data
#


