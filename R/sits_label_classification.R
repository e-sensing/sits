#' @title Post-process a classified data raster probs to obtain a labelled image
#'
#' @name  sits_label_classification
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Takes a set of classified raster layers with probabilities,
#'              whose metadata is]created by \code{\link[sits]{sits_cube}},
#'              and label them, with an optional bayesian smoothing process.
#'
#' @param  cube              Classified image data cube.
#' @param  smoothing         (optional) smoothing method to be applied
#'                           ("none", "bayesian", "majority")
#' @param  window            A matrix with the neighborhood window
#'                           to compute bayesian smooth.
#'                           The central element index (i, j) is given by
#'                           i = floor(nrows(window)/2)+1 and
#'                           j = floor(ncols(window)/2)+1.
#'                           Elements '0' are excluded from window.
#' @param  variance          Estimated variance of logit of class_probs
#'                           (Bayesian smoothing parameter).
#' @param  output_dir        Output directory where to out the file
#' @param  version           Version of resulting image
#'                           (in the case of multiple tests)
#' @return A tibble with metadata about the output RasterLayer objects.
#' @examples
#' \donttest{
#' # Retrieve the samples for Mato Grosso
#' # select band "ndvi"
#'
#' samples_ndvi <- sits_select(samples_mt_4bands, bands = "NDVI")
#'
#' #select a random forest model
#' rfor_model <- sits_train(samples_ndvi, ml_method = sits_rfor())
#'
#' # Classify a raster file with 23 instances for one year
#' files <- c(system.file("extdata/raster/mod13q1/sinop-crop-ndvi.tif",
#'                        package = "sits"))
#'
#' # create a data cube based on the information about the files
#' sinop <- sits_cube(type = "RASTER", satellite = "TERRA",
#'                    sensor = "MODIS", name = "Sinop-crop",
#'                    timeline = timeline_modis_392,
#'                    bands = c("NDVI"), files = files)
#'
#' # classify the raster image
#' sinop_probs <- sits_classify(sinop, ml_model = rfor_model,
#'                                     memsize = 2, multicores = 1)
#'
#' # label the classified image
#' sinop_label <- sits_label_classification(sinop_probs)
#'
#' # plot the raster image
#' plot(sinop_label, time = 1, title = "Sinop-2013-2014")
#'
#' # smooth the result with a bayesian filter
#' sinop_bayes <- sits_label_classification(sinop_probs, smoothing = "bayesian")
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
sits_label_classification <- function(cube,
									  smoothing    = "none",
									  window       = matrix(1,
									  					  nrow = 3,
									  					  ncol = 3,
									  					  byrow = TRUE),
									  variance     = 20,
									  output_dir   = "./",
									  version      = "v1") {

	# precondition 1 - check if cube has probability data
	file_name <- .sits_cube_file(cube)
	assertthat::assert_that(as.logical(grep("probs",(file_name))),
							msg = "sits_label_classification: input is not probability cube")

	# precondition 2 - test smoothing parameters
	assertthat::assert_that(smoothing %in% c("none", "bayesian",
											 "majority", "bayesian+majority"),
							msg = "sits_label_classification: unknown smoothing method")

	# precondition 3 - test window size
	assertthat::assert_that(nrow(window) == ncol(window),
							msg = "sits_label_classification: window must have equal sizes")

	# prediction 4 - test variance
	if (smoothing == "bayesian" || smoothing == "bayesian+majority")
		assertthat::assert_that(variance > 1,
								msg = "sits_label_classification: variance must be more than 1")


	# find out how many labels exist
	n_labels  <- length(.sits_cube_labels(cube))

	# allocate matrix of probabilities
	cube_size <-  cube$nrows*cube$ncols
	values <- matrix(NA, nrow = cube_size, ncol = n_labels)

	# create metadata for labeled raster cube
	cube_labels <- .sits_label_cube(cube_probs = cube,
	                                smoothing = smoothing,
	                                output_dir = output_dir,
	                                version = version)
	# retrieve the files to be read and written
	in_files  <- .sits_cube_files(cube)
	out_files <- .sits_cube_files(cube_labels)

	purrr::map2(in_files, out_files, function(in_file, out_file) {

	    for (b in 1:n_labels){
	        # read band values from file using GDAL
	        data <- matrix(as.matrix(
	            suppressWarnings(rgdal::readGDAL(
	                fname = in_file,
	                band = b, silent = TRUE)@data)),
	            nrow = cube$nrows, byrow = TRUE)

	        # avoid extreme values
	        data[data < 1] <- 1
	        data[data > 9999] <- 9999

	        # for each class, compute the smooth estimator (if required)
	        if (smoothing == "bayesian" || smoothing == "bayesian+majority")
	            # get smoothed values
	            values[ ,b] <- bayes_estimator_class(data, window, variance)
	        else
	            values[, b] <- t(data)

		}
		# create a raster object to write
	    layer <- terra::rast(nrows = cube_labels$nrows,
	                         ncols = cube_labels$ncols,
	                         xmin  = cube_labels$xmin,
	                         xmax  = cube_labels$xmax,
	                         ymin  = cube_labels$ymin,
	                         ymax  = cube_labels$ymax,
	                         crs   = cube_labels$crs)

		# select the best class by choosing the maximum value
		layer[] <- apply(values, 1, which.max)

		# apply majority filter
		if (smoothing == "majority" || smoothing == "bayesian+majority") {
			layer <- terra::focal(x = layer, w = 3,
			                      na.rm = TRUE, fun = terra::modal)
		}
		# save raster output to file
		terra::writeRaster(layer,
		                   filename = out_file,
		                   wopt     = list(filetype  = "GTiff",
		                                   datatype = "INT1U"),
		                   overwrite = TRUE)
	})
	return(cube_labels)
}

#' @title Create a set of RasterLayer objects
#'        to store data cube classification results (labelled classes)
#' @name .sits_label_cube
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Takes a tibble containing metadata about a data cube wuth
#' classification probabilites and and creates a
#' set of RasterLayers to store the classification result. Each RasterLayer is
#' to one time step. The time steps are specified in a list of dates.
#'
#' @param  cube_probs        Metadata about the input data cube (probability).
#' @param  smoothing         (optional) smoothing method to be applied
#'                           ("none", "bayesian", "majority")
#' @param  output_dir        Output directory where to put the files
#' @param  version           Name of the version of the result
#' @return                   Metadata about the output RasterLayer objects.
.sits_label_cube <- function(cube_probs, smoothing, output_dir, version) {

	# labels come from the input cube
	labels <- .sits_cube_labels(cube_probs)

	# how many objects are to be created?
	n_objs <- length(.sits_cube_files(cube_probs))

	# lists that store the content of the raster layers (classified values))
	bands     <- vector(length = n_objs)
	files     <- vector(length = n_objs)
	timelines <- vector("list", length = n_objs)
	# generate a set of timelines for the file_info
	times_probs <- vector(length = n_objs)

	# set scale factors, missing values, minimum and maximum values
	scale_factors   <- rep(1, n_objs)
	missing_values  <- rep(-9999, n_objs)
	minimum_values  <- rep(1, n_objs)
	maximum_values  <- rep(length(labels), n_objs)

	# get the type of the cube
	type  <- paste0("class_", smoothing)
	# name of the cube
	name  <- paste0(cube_probs[1,]$name, "_", type)

	# loop through the list of dates and create list of raster layers
	for (i in 1:n_objs) {

		# define the timeline for the raster data sets
		timeline       <- cube_probs$timeline[[1]][[i]]
		start_date     <- timeline[1]
		end_date       <- timeline[length(timeline)]

		# # define the filename for the classified image
		bands[i] <- .sits_cube_class_band_name(name = cube_probs[1,]$name,
											   type = type,
											   start_date = start_date,
											   end_date = end_date)
		files[i] <- .sits_raster_filename(output_dir = output_dir,
										  version = version,
										  name = cube_probs[1,]$name,
										  type = type,
										  start_date = start_date,
										  end_date = end_date)

		times_probs[i] <- start_date

		timelines[[i]] <- timeline
	}

	# get the file information
	file_info <- .sits_raster_file_info(bands, times_probs, files)

	# create a new RasterLayer for a defined period and generate metadata
	cube_labels <- .sits_cube_create(type           = "CLASSIFIED",
									 satellite      = cube_probs$satellite,
									 sensor         = cube_probs$sensor,
									 name           = name,
									 bands          = bands,
									 labels         = labels,
									 scale_factors  = scale_factors,
									 missing_values = missing_values,
									 minimum_values = minimum_values,
									 maximum_values = maximum_values,
									 timelines      = timelines,
									 nrows          = cube_probs$nrows,
									 ncols          = cube_probs$ncols,
									 xmin           = cube_probs$xmin,
									 xmax           = cube_probs$xmax,
									 ymin           = cube_probs$ymin,
									 ymax           = cube_probs$ymax,
									 xres           = cube_probs$xres,
									 yres           = cube_probs$yres,
									 crs            = cube_probs$crs,
									 file_info      = file_info)


	class(cube_labels) <- c("classified_image", class(cube_labels))
	return(cube_labels)
}
