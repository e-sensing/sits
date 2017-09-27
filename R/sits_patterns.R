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
sits_patterns <- function(data.tb, pt_method = sits_gam(data.tb = NULL, from = NULL, to = NULL, freq = 8, formula = y ~ s(x))) {

    # does the input data exist?
    .sits_test_tibble (data.tb)
    # is the train method a function?
    ensurer::ensure_that(pt_method, class(.) == "function", err_desc = "sits_train: train_method is not a valid function")

    # compute the training method by the given data
    result <- pt_method(data.tb)
    return(result)

}
#' @title Do not create patterns for classification
#' @name sits_patterns_from_data
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#'
#' @description This function is to be used when oen does not want to create
#' a set of idealised patterns from the input data, but prefers to use the
#' input series themselves as training data from estimating a prediction model.
#' It should be used in connection with sits_distances_from_data.
#'
#' @param  data.tb           a SITS tibble time series
#' @return patterns.tb       a SITS tibble time series used as reference for traning the model
#' @export
sits_patterns_from_data <- function (data.tb = NULL){


    # function that is used to be called as a value from another function
    result_fun <- function(tb){

        # create a reference tibble that will align all training data to the same
        # dates as the first entry in the input data
        # the reference tibble has one sample of each label
        ref.tb  <-  sits_sample (tb, n = 1)
        return (ref.tb)
    }
    result <- .sits_factory_function (data.tb, result_fun)
}
