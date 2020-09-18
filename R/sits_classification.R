#' @title Classify time series or data cube using machine learning models
#' @name sits_classify
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function classifies a set of time series or data cube given
#' a set of training samples, an inference model, and an interval.
#' To perform the classification, users should provide a set of
#' labelled samples. Each samples should be associated to one spatial location
#' (latitude/longitude), one time interval and a label.
#' This is a generic function. The following specific functions are available:
#' \itemize{
#'  \item{"sits tibble": }{Classify a set of time series - see \code{\link{sits_classify.sits}}}
#'  \item{"stack cube": }{ Classify a stack cube - see \code{\link{sits_classify.stack_cube}}}
#'  \item{"brick cube": }{Classify a brick cube - see \code{\link{sits_classify.brick_cube}}}
#' }
#' SITS supports the following models:
#' \itemize{
#'  \item{support vector machines: }{See \code{\link[sits]{sits_svm}}}
#'  \item{random forests: }{see \code{\link[sits]{sits_rfor}}}
#'  \item{linear discriminant analysis: }{see \code{\link[sits]{sits_lda}}}
#'  \item{quadratic discriminant analysis: }{see \code{\link[sits]{sits_qda}}}
#'  \item{multinomial logit, and its variants 'lasso' and ridge': }{see \code{\link[sits]{sits_mlr}}}
#'  \item{extreme gradient boosting: }{see \code{\link[sits]{sits_xgboost}}}
#'  \item{multi-layer perceptrons: }{see \code{\link[sits]{sits_deeplearning}}}
#'  \item{1D convolutional neural networks: see \code{\link[sits]{sits_FCN}}}
#'  \item{mixed 1D and MLP networks: }{ see \code{\link[sits]{sits_TempCNN}}}
#'  \item{1D version of ResNet: }{see \code{\link[sits]{sits_ResNet}}}
#'  \item{combined LSTM-FCN model: }{see \code{\link[sits]{sits_LSTM_FCN}}}
#'  }
#' The model should be precomputed by the user using the function \code{\link[sits]{sits_train}}
#' and then passed to the "sits_classify" function using parameter "ml_model".
#'
#' @param  data              Tibble with time series metadata and data.
#' @param  ml_model          Pre-built machine learning model
#'                             (see \code{\link[sits]{sits_train}}).
#' @param  ...               Other parameters to be passed to specific functions
#' @return                   Predicted data (either a sits tibble or a probs data cube)
#'
#' @export
sits_classify <- function(data, ml_model, ...) {

    # is the data a sits tibble? If not, it must be a cube
    if (!("sits" %in% class(data))){
      # find out the generic cube class it belongs to
      class_data <- .sits_config_cube_class_generic(data[1,]$type)
      class(data) <- c(class_data, class(data))
    }

    # Dispatch
    UseMethod("sits_classify", data)
}

#' @title Classify a set of time series using machine learning models
#' @name sits_classify.sits
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function classifies a set of time series, given
#' a set of training samples, an inference model, and an interval.
#' To perform the classification, users should provide a set of
#' labelled samples. Each samples should be associated to one spatial location
#' (latitude/longitude), one time interval and a label.
#'
#' The model should be precomputed by the user. This model should be
#' passed to the function using the parameter "ml_model".
#'
#' @param  data              Tibble with time series metadata and data.
#' @param  ml_model          Pre-built machine learning model
#'                             (see \code{\link[sits]{sits_train}}).
#' @param  ...               Other parameters to be passed to specific functions
#' @param  filter            Smoothing filter to be applied (if desired).
#' @param  multicores        Number of cores to be used for classification.
#' @return A tibble with the predicted labels for each input segment.
#'
#' @examples
#' \donttest{
#' # Retrieve the samples for Mato Grosso
#' # select band "NDVI"
#' samples_ndvi <- sits_select(samples_mt_4bands, bands = "NDVI")
#'
#' #select a random forest model
#' rfor_model <- sits_train(samples_ndvi, ml_method = sits_rfor())
#'
#' # classify the point
#' class.tb <- sits_classify(point_ndvi, rfor_model)
#'
#' # plot the classification
#' plot(class.tb)
#' }
#' @export
#'
sits_classify.sits <- function(data, ml_model, ...,
                               filter = NULL,
                               multicores = 2) {

		# check if we are running in Windows
		if (.Platform$OS.type != "unix")
			multicores <-  1

		# backward compatibility
		data <- .sits_tibble_rename(data)

		# precondition - verify that the data is correct
		.sits_test_tibble(data)

		# precondition -ensure the machine learning model has been built
		assertthat::assert_that(!purrr::is_null(ml_model),
							msg = "sits_classify_ts: please provide a trained ML model")

		# Precondition
		# only savitsky-golay and whittaker filters are supported
		if (!purrr::is_null(filter)) {
				call_names <-  deparse(sys.call())
				assertthat::assert_that(any(grepl("sgolay", (call_names))) ||
										any(grepl("whittaker", (call_names))),
					 msg = "sits_classify_cube: only savitsky-golay and whittaker filters
                  are supported")
				data <- filter(data)
		}

		# precondition - are the samples valid?
		samples <- environment(ml_model)$data
		assertthat::assert_that(NROW(samples) > 0,
								msg = "sits_classify_ts: missing original samples")
		# how many samples per interval?
		num_samples <- nrow(samples[1,]$time_series[[1]])

		# get normalization params
		stats   <- environment(ml_model)$stats
		# Has the data been normalised?
		if (!purrr::is_null(stats))
				# obtain the distances after normalizing data by band
				distances_DT <- .sits_distances(.sits_normalize_data(data = data,
																stats = stats,
																multicores = multicores))
		else
		    # data has been already normalised
		    distances_DT <- .sits_distances(data)

	  # postcondition - valid result
	  assertthat::assert_that(NROW(distances_DT) > 0,
					msg = "sits_classify,sits: problem with normalization")

	  # calculate the breaks in the time for multi-year classification
	  class_info.tb <- .sits_timeline_class_info(data     = data,
	                                             samples  = samples)

	  # retrieve the the predicted results
	  predict.mtx <- .sits_distances_classify(distances_DT = distances_DT,
	                                          class_info.tb = class_info.tb,
	                                          ml_model = ml_model,
	                                          multicores = multicores)

	  # Store the result in the input data
	  data <- .sits_tibble_prediction(data = data,
	                                  class_info.tb = class_info.tb,
	                                  pred.mtx = predict.mtx)
	  return(data)
}


#' @title Classify a data cube using multicore machines
#' @name sits_classify.brick_cube
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Takes a data cube, whose metadata is
#'    described by tibble (created by \code{\link[sits]{sits_cube}}),
#'    a set of samples used for training a classification model,
#'    a prediction model (created by \code{\link[sits]{sits_train}}),
#'    and produces a classified set of RasterLayers. These
#'    parameters are "memsize" and "multicores".
#'    The "multicores" parameter defines the
#'    number of cores used for processing. The "memsize" parameter  controls
#'    the amount of memory available for classification.
#'
#' @param  data            data cube
#' @param  ml_model        R model trained by \code{\link[sits]{sits_train}}.
#' @param  ...             other parameters to be passed to specific functions
#' @param  sf_region       an sf object with the region of interest
#' @param  filter          smoothing filter to be applied (if desired).
#' @param  memsize         memory available for classification (in GB).
#' @param  multicores      number of cores to be used for classification.
#' @param  output_dir      directory for output file
#' @param  version         version of the output (for multiple classifications)
#' @return                 cube with the metadata of a brick of probabilities.
#'
#' @examples
#' \donttest{
#' # Classify a raster file with 23 instances for one year
#' files <- c(system.file("extdata/raster/mod13q1/sinop-crop-ndvi.tif",
#'                        package = "sits"))
#' # create a data cube based on the information about the files
#' sinop <- sits_cube(type = "BRICK", satellite = "TERRA",
#'                    sensor = "MODIS", name = "Sinop-crop",
#'                    timeline = timeline_modis_392,
#'                    bands = c("NDVI"), files = files)
#'
#' # classify the raster image
#' sinop_probs <- sits_classify(sinop, ml_model = rfor_model,
#'                              memsize = 2, multicores = 1)
#'
#' # label the classified image
#' sinop_label <- sits_label_classification(sinop_probs)
#'
#' # plot the raster image
#' plot(sinop_label, time = 1, title = "Sinop-2013-2014")
#'
#' # smooth the result with a bayesian filter
#' sinop_bayes <- sits_label_classification(sinop_probs,
#'                                smoothing = "bayesian")
#'
#' # plot the smoothened image
#' plot(sinop_bayes, time = 1, title = "Sinop-smooth")
#'
#' # remove the files (cleanup)
#' file.remove(unlist(sinop_probs$files))
#' file.remove(unlist(sinop_label$files))
#' file.remove(unlist(sinop_bayes$files))
#' }
#' @export
sits_classify.brick_cube <- function(data, ml_model, ...,
                                     sf_region  = NULL,
                                     filter     = NULL,
                                     memsize    = 8,
                                     multicores = 2,
                                     output_dir = "./",
                                     version    = "v1") {

    # precondition - checks if the cube and ml_model are valid
    .sits_classify_check_params(data, ml_model)

    # find the number of cores
    if (purrr::is_null(multicores))
    multicores <- max(parallel::detectCores(logical = FALSE) - 1, 1)

    # CRAN limits the number of cores to 2
    chk <- Sys.getenv("_R_CHECK_LIMIT_CORES_", "")
    # if running on check mode, multicores must be 2
    if (nzchar(chk) && chk == "TRUE") {
        # use 2 cores in CRAN/Travis/AppVeyor
        multicores <- 2L
    }

    if (!purrr::is_null(sf_region)) {
      assertthat::assert_that("sf" %in% class(sf_region),
                              msg = "region of interest must be an sf object")
    }
    # retrieve the samples from the model
    samples  <- environment(ml_model)$data
    # precondition - are the samples correct?
    assertthat::assert_that(NROW(samples) > 0,
        msg = "sits_classify: original samples not saved")

    # precondition - are the cube bands the same as the sample bands?
    cube_bands   <- .sits_cube_bands(data)
    sample_bands <- sits_bands(samples)
    assertthat::assert_that(
        all(sample_bands %in% cube_bands),
        msg = "sits_classify: bands in samples different from cube bands")

    # classify the data
    cube_probs <- .sits_classify_multicores(cube        = data,
                                            samples     = samples,
                                            ml_model    = ml_model,
                                            sf_region   = sf_region,
                                            filter      = filter,
                                            memsize     = memsize,
                                            multicores  = multicores,
                                            output_dir  = output_dir,
                                            version = version)

    return(cube_probs)
}

#' @title Classify a stack data cube using multicore machines
#' @name sits_classify.stack_cube
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Takes a data cube, whose metadata is
#'    described by tibble (created by \code{\link[sits]{sits_cube}}),
#'    a set of samples used for training a classification model,
#'    a prediction model (created by \code{\link[sits]{sits_train}}),
#'    and produces a classified set of RasterLayers. These
#'    parameters are "memsize" and "multicores".
#'    The "multicores" parameter defines the
#'    number of cores used for processing. The "memsize" parameter  controls
#'    the amount of memory available for classification.
#'
#' @param  data            data cube
#' @param  ml_model        R model trained by \code{\link[sits]{sits_train}}.
#' @param  ...             other parameters to be passed to specific functions
#' @param  sf_region       an sf object with the region of interest
#' @param  filter          smoothing filter to be applied (if desired).
#' @param  memsize         memory available for classification (in GB).
#' @param  multicores      number of cores to be used for classification.
#' @param  output_dir      directory for output file
#' @param  version         version of the output (for multiple classifications)
#' @return                 cube with the metadata of a brick of probabilities.
#'
#' @export
sits_classify.stack_cube <- function(data, ml_model, ...,
                                     sf_region  = NULL,
                                     filter     = NULL,
                                     memsize    = 8,
                                     multicores = 2,
                                     output_dir = "./",
                                     version    = "v1") {


    cube_probs <-  sits_classify.brick_cube(data = data,
                                            ml_model = ml_model,
                                            ...,
                                            sf_region  = sf_region,
                                            filter     = filter,
                                            memsize    = memsize,
                                            multicores = multicores,
                                            output_dir = output_dir,
                                            version    = version)
  return(cube_probs)
}







