#' @title Train classification models
#' @name sits_train
#'
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Alexandre Ywata de Carvalho, \email{alexandre.ywata@@ipea.gov.br}
#'
#' @description Given a tibble with a set of distance measures,
#'    returns trained models. Currently, sits supports the following models:
#' 'svm' (see \code{\link[sits]{sits_svm}}),
#' random forests (see \code{\link[sits]{sits_rfor}}),
#' multinomial logit (see \code{\link[sits]{sits_mlr}}) and its variants
#' 'lasso' (see \code{\link[sits]{sits_mlr}}) and
#' 'ridge' (see \code{\link[sits]{sits_mlr}}),
#' extreme gradient boosting (see \code{\link[sits]{sits_xgboost}}),
#' light gradient boosting machine (see \code{\link[sits]{sits_lightgbm}}),
#' and different deep learning functions, including multi-layer perceptrons
#' (see \code{\link[sits]{sits_mlp}}), 1D convolution neural
#' networks \code{\link[sits]{sits_tempcnn}},
#' deep residual networks \code{\link[sits]{sits_resnet}} and
#' self-attention encoders \code{\link[sits]{sits_lighttae}}
#'
#' @param  samples          Time series with the training samples.
#' @param  ml_method        Machine learning method.
#' @return                  Model fitted to input data
#'                          to be passed to \code{\link[sits]{sits_classify}}
#'
#' @examples
#' # Retrieve the set of samples for Mato Grosso (provided by EMBRAPA)
#' # fit a training model (RFOR model)
#' samples <- sits_select(samples_modis_4bands, bands = c("NDVI"))
#' ml_model <- sits_train(samples, sits_rfor(num_trees = 50))
#' # get a point and classify the point with the ml_model
#' point_ndvi <- sits_select(point_mt_6bands, bands = "NDVI")
#' class <- sits_classify(point_ndvi, ml_model)
#' @export
#'
sits_train <- function(samples, ml_method = sits_svm()) {

    # set caller to show in errors
    .check_set_caller("sits_train")

    # is the input data a valid sits tibble?
    .check_chr_within(
        x = "label",
        within = names(samples),
        discriminator = "any_of",
        msg = "input data does not contain a valid sits tibble"
    )

    # is the train method a function?
    .check_that(
        x = inherits(ml_method, "function"),
        msg = "ml_method is not a valid function"
    )

    .check_that(
        x = .sits_timeline_check(samples) == TRUE,
        msg = paste0(
            "Samples have different timeline lengths", "\n",
            "Use.sits_tibble_prune or sits_fix_timeline"
        )
    )

    # compute the training method by the given data
    result <- ml_method(samples)

    # return a valid machine learning method
    return(result)
}
