#' @title Classify a set of time series or a data cube using machine learning models
#' @name sits_classify
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function classifies a set of time series or data cube given
#' a set of training samples, an inference model, and an interval.
#' To perform the classification, users should provide a set of
#' labelled samples. Each samples should be associated to one spatial location
#' (latitude/longitude), one time interval and a label.
#'
#' After defining the training samples, the users need to provide a machine learning model.
#' Currenly, sits supports the following models:
#' 'svm' (see \code{\link[sits]{sits_svm}}), 'random forest' (see \code{\link[sits]{sits_rfor}}),
#' 'lda' (see \code{\link[sits]{sits_lda}}),
#' 'qda' (see \code{\link[sits]{sits_qda}}), multinomial logit' (see \code{\link[sits]{sits_mlr}}),
#' 'lasso' (see \code{\link[sits]{sits_mlr}}), and 'ridge' (see \code{\link[sits]{sits_mlr}}).
#'
#' The model should be precomputed by the user. This model should be
#' passed to the function using the parameter "ml_model".
#'
#' @param  data.tb           Tibble with time series metadata and data.
#' @param  ml_model          Pre-built machine learning model (see \code{\link[sits]{sits_train}}).
#' @param  interval          Interval used for classification (in months).
#' @param  filter            Smoothing filter to be applied (if desired).
#' @param  memsize           Memory available for classification (in GB).
#' @param  multicores        Number of cores to be used for classification.
#' @param  out_prefix        Prefix of the output files. For each time interval, one file will be created.
#' @return A tibble with the predicted labels for each input segment.
#' @examples
#' \donttest{
#' # Retrive the samples for Mato Grosso
#' devtools::install_github("e-sensing/inSitu")
#' library(inSitu)
#'
#' samples <- inSitu::br_mt_1_8K_9classes_6bands
#'
#' # select the bands "ndvi", "evi", "nir", and "mir"
#' samples_4bands <- sits_select_bands(samples, ndvi, evi, nir, mir)
#'
#' #select a random forest model
#'
#' rfor_model <- sits_train(samples_4bands, ml_method = sits_rfor())
#'
#' # Retrieve a time series (17 years)
#' data(point_mt_6bands)
#'
#' # select the bands "ndvi", "evi", "nir", and "mir"
#' point.tb <- sits_select_bands(point_mt_6bands, ndvi, evi, nir, mir)
#'
#' # classify the point
#'
#' class.tb <- sits_classify(point.tb, rfor_model)
#'
#' # plot the classification
#'
#' sits_plot(class.tb)
#'
#' # Classify a raster file with 23 instances for one year
#'
#' # select the bands "ndvi", "evi" from the "inSitu" package
#' evi_file <- system.file("extdata/Sinop", "Sinop_evi_2014.tif", package = "inSitu")
#' ndvi_file <- system.file("extdata/Sinop", "Sinop_ndvi_2014.tif", package = "inSitu")
#'
#' files <- c(ndvi_file, evi_file)
#'
#' # define the timeline for the files
#' time_file <- system.file("extdata/Sinop", "timeline_2014.txt", package = "inSitu")
#' timeline_2013_2014 <- scan(time_file, character())
#'
#' # create a data cube based on the information about the files
#' raster.tb <- sits_cube(service = "RASTER", name = "Sinop",
#'              timeline = timeline_2013_2014, bands = c("ndvi", "evi"), files = files)
#'
#' # classify the raster image
#' raster_class.tb <- sits_classify(raster.tb, ml_model = rfor_model, memsize = 4, multicores = 2,
#'                    out_prefix = "./Sinop-class")
#'
#' # plot the raster image
#' sits_plot_raster(raster_class.tb, time = 1, title = "Sinop-2013-2014")
#'
#' # smooth the result with a bayesian filter
#' raster_class_bayes.tb <- sits::sits_bayes_postprocess(raster_class.tb, file = "./smooth")
#'
#' # plot the smoothened image
#' sits_plot_raster(raster_class_bayes.tb, time = 1, title = "Sinop-smooth")
#' }
#' @export
sits_classify <- function(data.tb     = NULL,
                          ml_model    = NULL,
                          interval    = "12 month",
                          filter      = NULL,
                          multicores  = 2,
                          memsize     = 4,
                          out_prefix  = "cube-class")
{
    if ("time_series" %in% names(data.tb))
        result.tb <- .sits_classify_ts(data.tb, ml_model, interval, multicores)
    else
        result.tb <- .sits_classify_cube(data.tb, ml_model, interval, filter, multicores, memsize, out_prefix)

    return(result.tb)
}
