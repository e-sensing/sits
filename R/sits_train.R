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
#' @note
#' The main \code{sits} classification workflow has the following steps:
#' \enumerate{
#'      \item{\code{\link[sits]{sits_cube}}: selects a ARD image collection from
#'          a cloud provider.}
#'      \item{\code{\link[sits]{sits_cube_copy}}: copies an ARD image collection
#'          from a cloud provider to a local directory for faster processing.}
#'      \item{\code{\link[sits]{sits_regularize}}: create a regular data cube
#'          from an ARD image collection.}
#'      \item{\code{\link[sits]{sits_apply}}: create new indices by combining
#'          bands of a  regular data cube (optional).}
#'      \item{\code{\link[sits]{sits_get_data}}: extract time series
#'          from a regular data cube based on user-provided labelled samples.}
#'      \item{\code{\link[sits]{sits_train}}: train a machine learning
#'          model based on image time series.}
#'      \item{\code{\link[sits]{sits_classify}}: classify a data cube
#'          using a machine learning model and obtain a probability cube.}
#'      \item{\code{\link[sits]{sits_smooth}}: post-process a probability cube
#'          using a spatial smoother to remove outliers and
#'          increase spatial consistency.}
#'      \item{\code{\link[sits]{sits_label_classification}}: produce a
#'          classified map by selecting the label with the highest probability
#'          from a smoothed cube.}
#' }
#'
#' \code{sits_train}  provides a standard interface to machine learning models.
#' It takes two mandatory parameters: the training data (\code{samples})
#' and the ML algorithm (\code{ml_method}). The output is a model that
#' can be used to classify individual time series or data cubes
#' with \code{\link[sits]{sits_classify}}.
#'
#' \code{sits} provides a set of default values for all classification models.
#' These settings have been chosen based on testing by the authors.
#' Nevertheless, users can control all parameters for each model.
#' Novice users can rely on the default values,
#' while experienced ones can fine-tune deep learning models
#' using \code{\link[sits]{sits_tuning}}.
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
