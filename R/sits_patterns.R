#' @title Create time series patterns for classification
#' @name sits_patterns
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#'
#' @description This function allows the user to select different alternatives to define a set of
#' patterns, given his samples. The alternatives are:
#' "gam" - uses a generalised additive model to approximate a smooth spline for each pattern
#' "dendogram" - uses a herarchical clustering method to group the patterns
#' "centroids" - uses a positional clustering method to group the patterns
#'
#' @param data.tb          a SITS tibble time series with an alignment column
#' @param pt_method        a pattern fitting method
#' @return result          a model fitted into input data given by train_method parameter
#' @export
#'
sits_patterns <- function(data.tb, tr_method = sits_gam(data.tb = NULL, bands = NULL, from = NULL, to = NULL, freq = 8, formula = y ~ s(x))) {

    # is the input data the result of a TWDTW matching function?
    ensurer::ensure_that (data.tb, !purrr::is_null(.),
                          err_desc = "sits_patterns: input data not provided")
    # is the train method a function?
    ensurer::ensure_that(tr_method, class(.) == "function", err_desc = "sits_train: train_method is not a valid function")

    # compute the training method by the given data
    result <- tr_method(data.tb)
    return(result)

}
