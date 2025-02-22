#' @title Train classification models
#' @name sits_train
#'
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Alexandre Ywata de Carvalho, \email{alexandre.ywata@@ipea.gov.br}
#'
#' @description Given a tibble with a set of distance measures,
#'    returns trained models. Currently, sits supports the following models:
#' 'svm' (see \code{\link[sits]{sits_svm}}),
#' random forests (see \code{\link[sits]{sits_rfor}}),
#' extreme gradient boosting (see \code{\link[sits]{sits_xgboost}}),
#' and different deep learning functions, including multi-layer perceptrons
#' (see \code{\link[sits]{sits_mlp}}), 1D convolution neural
#' networks \code{\link[sits]{sits_tempcnn}},
#' self-attention encoders \code{\link[sits]{sits_lighttae}}
#'
#' @param  samples          Time series with the training samples.
#' @param  ml_method        Machine learning method.
#' @return                  Model fitted to input data
#'                          to be passed to \code{\link[sits]{sits_classify}}
#'
#' @examples
#' if (sits_run_examples()) {
#'     # Retrieve the set of samples for Mato Grosso
#'     # fit a training model (rfor model)
#'     ml_model <- sits_train(samples_modis_ndvi, sits_rfor(num_trees = 50))
#'     # get a point and classify the point with the ml_model
#'     point_ndvi <- sits_select(point_mt_6bands, bands = "NDVI")
#'     class <- sits_classify(
#'         data = point_ndvi, ml_model = ml_model
#'     )
#' }
#' @export
#'
sits_train <- function(samples, ml_method = sits_svm()) {
    # set caller to show in errors
    .check_set_caller("sits_train")
    # check if samples are valid
    .check_samples_train(samples)
    # is the train method a function?
    .check_that(inherits(ml_method, "function"),
        msg = .conf("messages", "sits_train_method")
    )
    # compute the training method by the given data
    result <- ml_method(samples)
    # return a valid machine learning method
    return(result)
}
