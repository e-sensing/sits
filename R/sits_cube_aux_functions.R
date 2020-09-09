#' @title Creates the description of a data cube
#' @name .sits_cube_create
#' @keywords internal
#'
#' @description Print information and save metadata about a data cube.
#'
#' @param type               Type of cube
#' @param URL                URL of the provider (optional)
#' @param satellite          Name of satellite
#' @param sensor             Name of sensor
#' @param name               Name of the data cube (mandatory)
#' @param cube               Name of the input data cube (optional)
#' @param tile               Name of the input data tile (optional)
#' @param bands              Vector with the names of the bands.
#' @param labels             Vector with labels (only for classified data).
#' @param scale_factors      Vector with scale factor for each band.
#' @param missing_values     Vector with missing values for each band.
#' @param minimum_values     Vector with minimum values for each band.
#' @param maximum_values     Vector with maximum values for each band.
#' @param timelines          List with vectors of valid timelines for each band.
#' @param nrows              Number of rows in the cube.
#' @param ncols              Number of columns in the cube.
#' @param xmin               Spatial extent (xmin).
#' @param ymin               Spatial extent (ymin).
#' @param xmax               Spatial extent (xmax).
#' @param ymax               Spatial extent (ymin).
#' @param xres               Spatial resolution (x dimension).
#' @param yres               Spatial resolution (y dimension).
#' @param crs                CRS for cube (EPSG code or PROJ4 string).
#' @param file_info          Tibble with information about stacks (for stacks)
#'
.sits_cube_create <- function(type,
							  URL = NA,
							  satellite,
							  sensor,
							  name,
							  cube = NA,
							  tile = NA,
							  bands,
							  labels = NA,
							  scale_factors,
							  missing_values,
							  minimum_values,
							  maximum_values,
							  timelines,
							  nrows,
							  ncols,
							  xmin,
							  xmax,
							  ymin,
							  ymax,
							  xres,
							  yres,
							  crs,
							  file_info = NULL) {


	# create a tibble to store the metadata (mandatory parameters)
	cube.tb <- tibble::tibble(type           = type,
							  URL            = URL,
							  satellite      = satellite,
							  sensor         = sensor,
							  name           = name,
							  cube           = cube,
							  tile           = tile,
							  bands          = list(bands),
							  labels         = list(labels),
							  scale_factors  = list(scale_factors),
							  missing_values = list(missing_values),
							  minimum_values = list(minimum_values),
							  maximum_values = list(maximum_values),
							  timeline       = list(timelines),
							  nrows          = nrows,
							  ncols          = ncols,
							  xmin           = xmin,
							  xmax           = xmax,
							  ymin           = ymin,
							  ymax           = ymax,
							  xres           = xres,
							  yres           = yres,
							  crs            = crs)

	if (!purrr::is_null(file_info))
		cube.tb <- tibble::add_column(cube.tb, file_info = list(file_info))

	return(cube.tb)
}



#' @title Create a set of RasterLayer objects to store
#' data cube classification results (only the probs)
#' @name .sits_cube_classified
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Take a tibble containing metadata about a data cube
#' containing time series (each Brick has information for one band) and create a
#' set of RasterLayers to store the classification result.
#' Each RasterLayer corresponds to one time step.
#' The time steps are specified in a list of dates.
#'
#' @param  cube              input data cube.
#' @param  samples           samples used for training the classification model.
#' @param  sub_image         bounding box of the ROI
#' @param  output_dir        prefix of the output files.
#' @param  version           version of the output files
#' @return                   output data cube
.sits_cube_classified <- function(cube, samples, sub_image,
								  output_dir, version) {
	# ensure metadata tibble exists
	assertthat::assert_that(NROW(cube) > 0,
							msg = ".sits_classify_cube: need a valid metadata for cube")

	# get the timeline of of the data cube
	timeline <- lubridate::as_date(sits_timeline(cube))

	# Get the reference start date and end date from the samples
	ref_start_date <- lubridate::as_date(samples[1,]$start_date)
	ref_end_date   <- lubridate::as_date(samples[1,]$end_date)

	# number of samples
	num_samples <- nrow(samples[1,]$time_series[[1]])

	# produce the breaks used to generate the output rasters
	subset_dates <- .sits_timeline_match(timeline      = timeline,
										ref_start_date = ref_start_date,
										ref_end_date   = ref_end_date,
										num_samples    = num_samples)

	# how many objects are to be created?
	n_objs <- length(subset_dates)

	# labels come from samples.tb
	labels <- sits_labels(samples)$label

	# create vectors and lists to store the content of the probabilities
	rasters    <- vector("list", length = n_objs)
	bands      <- vector(length = n_objs)
	files      <- vector(length = n_objs)
	n_layers   <- length(labels)
	timelines  <- vector("list", length = n_objs)

	# set scale factors, missing values, minimum and maximum values for probs
	scale_factors   <- rep(0.001,  n_objs)
	missing_values  <- rep(-9999,  n_objs)
	minimum_values  <- rep(0.0,    n_objs)
	maximum_values  <- rep(1.0,    n_objs)

	# loop through the list of dates and create list of raster layers
	for (i in 1:n_objs) {

		# define the timeline for the raster data sets
		start_date     <- subset_dates[[i]][1]
		end_date       <- subset_dates[[i]][2]
		timelines[[i]] <- timeline[lubridate::as_date(timeline) >= start_date &
								   	lubridate::as_date(timeline) <= end_date]

		# define the filename for the classified image
		files[i] <- .sits_raster_filename(output_dir = output_dir,
										  version = version,
										  name = cube$name,
										  type = "probs",
										  start_date = start_date,
										  end_date = end_date)
		bands[i] <- .sits_cube_class_band_name(name = cube$name, type = "probs",
											   start_date = start_date,
											   end_date = end_date)
	}
	# get the name of the cube
	name   <-  paste0(cube[1,]$name, "_probs")

	# generate a set of timelines for the file_info
	times_probs <- vector(length = n_objs)
	for (i in 1:n_objs) {
		times_probs[i] <- timelines[[i]][1]
	}

	# get the file information
	file_info <- .sits_raster_file_info(bands, times_probs, files)

	# set the metadata for the probability cube
	cube_probs <- .sits_cube_create(type            = "PROBS",
									satellite       = cube$satellite,
									sensor          = cube$sensor,
									name            = name,
									bands           = bands,
									labels          = labels,
									scale_factors   = scale_factors,
									missing_values  = missing_values,
									minimum_values  = minimum_values,
									maximum_values  = maximum_values,
									timelines       = timelines,
									nrows           = unname(sub_image["nrows"]),
									ncols           = unname(sub_image["ncols"]),
									xmin            = unname(sub_image["xmin"]),
									xmax            = unname(sub_image["xmax"]),
									ymin            = unname(sub_image["ymin"]),
									ymax            = unname(sub_image["ymax"]),
									xres            = cube$xres,
									yres            = cube$yres,
									crs             = cube$crs,
									file_info       = file_info)

	class(cube_probs) <- c("probs_cube", class(cube_probs))
	return(cube_probs)
}
#' @title Define a name for classified band
#' @name .sits_cube_class_band_name
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description    Creates a name for a raster layer based on timeline
#'
#' @param name           original cube name (without temporal information).
#' @param type           type of output
#' @param start_date     starting date of the time series classification.
#' @param end_date       end date of the time series classification.
#' @return               name of the classification file for the required interval.
.sits_cube_class_band_name <- function(name, type, start_date, end_date){
	y1 <- lubridate::year(start_date)
	m1 <- lubridate::month(start_date)
	y2 <- lubridate::year(end_date)
	m2 <- lubridate::month(end_date)

	band_name <- paste0(name, "_", type, "_", y1, "_", m1, "_", y2, "_", m2)

	return(band_name)
}

#' @title Find the bands associated to a cube
#' @name .sits_cube_bands
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description    Given a data cube, retrieves the bands
#'
#' @param cube          Metadata about a data cube
#' @return  Vector of bands available in the data cube
.sits_cube_bands <- function(cube) {
	return(cube$bands[[1]])
}
#' @title Set the bands associated to a cube
#' @name .sits_cube_bands_set
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description    Given a data cube and a set of bands, sets the bands
#'
#' @param cube          Metadata about a data cube
#' @param bands         Bands to be assigned to the cube
#' @return  Vector of bands available in the data cube
.sits_cube_bands_set <- function(cube, bands) {
	cube$bands[[1]] <- bands
	return(cube)
}

#' @title Check that the cube is valid
#' @name .sits_cube_check_validity
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description     Given a data cube, retrieve the scale factors
#' @param cube      Metadata about a data cube
#' @return          Boolean value
.sits_cube_check_validity <- function(cube){

	# check that the service is valid
	.sits_config_check_type(cube[1,]$type)

	check <- FALSE

	# check is WTSS service is working
	if (cube$type == "WTSS")
		check <- .sits_wtss_check(cube$URL, cube$name)
	# check is SATVEG service is working
	else if (cube$type == "SATVEG")
		check <- .sits_satveg_check()
	# raster cubes have been checked before
	else
		check <- TRUE

	return(invisible(check))
}

#' @title Return a file associated to a data cube, given an index
#' @name .sits_cube_file
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description     Given a data cube and an index, retrieve the file
#' @param cube      Metadata about a data cube
#' @param index     Index for file to be retrived
#' @return          Name of file
.sits_cube_file <- function(cube, index = 1) {
	assertthat::assert_that(index <= length(cube$file_info[[1]]$path),
							msg = ".sits_cube_file: index is out of range")
	return(cube$file_info[[1]]$path[index])
}

#' @title Return all file associated to a data cube
#' @name .sits_cube_files
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description    Given a data cube and an index, retrieve the files
#' @param cube     Metadata about a data cube
#' @return         Vector of files
.sits_cube_files <- function(cube) {
	return(cube$file_info[[1]]$path)
}


#' @title Return all labels associated to a data cube
#' @name .sits_cube_labels
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description    Given a data cube, retrieve the ;abels
#' @param cube     Metadata about a data cube
#' @return         Vector of labels
.sits_cube_labels <- function(cube){
	return(cube$labels[[1]])
}

#' @title Return the timeline associated to a data cube, given an index
#' @name sits_cube_timeline
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description    Given a data cube, retrieve the timeline
#' @param cube     Metadata about a data cube
#' @param index    Index of timeline list
#' @return         Vector of times for an index
#' @export
sits_cube_timeline <- function(cube, index = 1){
	assertthat::assert_that(index <= length(cube$timeline[[1]]),
							msg = ".sits_cube_timeline: index out of range")
	return(cube$timeline[[1]][[index]])
}

#' @title Given a band, return the associated Raster object for the cube
#' @name .sits_cube_robj_band
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description          Given a data cube, retrieve the timeline
#' @param cube           Metadata about a data cube
#' @param band_cube      Name of the band to the retrieved
#' @return               Raster object associated to the indexed file
#'
.sits_cube_robj_band <- function(cube, band_cube){

	bands <- unlist(cube$bands)
	index <- grep(band_cube, bands)

	band.tb <- dplyr::filter(cube$file_info[[1]], band == band_cube)
	# Get the robjs for faster access
	if (cube$type == "BRICK")
		r_obj <- suppressWarnings(raster::brick(band.tb$path))
	else
		r_obj <- suppressWarnings(raster::stack(band.tb$path))
	return(r_obj)
}

#' @title Given a band, return the associated Raster object for the cube
#' @name .sits_cube_terra_obj_band
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description          Given a data cube, retrieve the timeline
#' @param cube           Metadata about a data cube
#' @param band_cube      Name of the band to the retrieved
#' @return               Raster object associated to the indexed file
#'
.sits_cube_terra_obj_band <- function(cube, band_cube){

    band.tb <- dplyr::filter(cube$file_info[[1]], band == band_cube)
    t_obj <- suppressWarnings(terra::rast(band.tb$path))
    return(t_obj)
}

#' @title Retrieve the missing values for a data cube
#' @name .sits_cube_missing_values
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description     Given a data cube, retrieve the missing values
#' @param cube      Metadata about a data cube
#' @return          Vector of missing values.
.sits_cube_missing_values <- function(cube){
	return(cube$missing_values[[1]])
}

#' @title Retrieve the minimum values for a data cube
#' @name .sits_cube_minimum_values
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description     Given a data cube, retrieve the minimum values
#' @param cube      Metadata about a data cube
#' @return          Vector of minimum values.
.sits_cube_minimum_values <- function(cube){
	return(cube$minimum_values[[1]])
}
#' @title Retrieve the minimum values for a data cube
#' @name .sits_cube_maximum_values
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description     Given a data cube, retrieve the maximum values
#' @param cube      Metadata about a data cube
#' @return          Vector of maximum values.
.sits_cube_maximum_values <- function(cube){
	return(cube$maximum_value[[1]])
}

#' @title Retrieve the scale factors for a data cube
#' @name .sits_cube_scale_factors
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description     Given a data cube, retrieve the scale factors
#' @param cube      Metadata about a data cube
#' @return          Vector of scale factors
.sits_cube_scale_factors <- function(cube){
	return(cube$scale_factors[[1]])
}

#' @title Align the bands of the cube with those of the samples
#' @name .sits_cube_align_bands
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description         Given a data cube, retrieve the scale factors
#' @param cube          Metadata about a data cube
#' @param sample_bands  Bands of the data sample
#' @return              Updated cube
.sits_cube_align_bands <- function(cube, sample_bands){

	# retrieve the cube bands
	cube_bands <- .sits_cube_bands(cube)

	# align the indexes
	m <- match(sample_bands, cube_bands)

	# reorganize the bands and the files in the cube
	# they should be aligned with the bands in the samples
	cube_bands <- cube_bands[m]
	cube$bands <- list(cube_bands)

	# adjust the object list
	if (cube$type == "BRICK") {
		cube$files <- cube$files %>%
			unlist() %>%
			.[m]     %>%
			list()
	}
	cube$r_objs_list <- cube$r_objs_list %>%
		unlist() %>%
		.[m]     %>%
		list()

	# need to include the case of BDC_TILE
	return(cube)
}
#' @title Check that the requested bands exist in the cube
#' @name .sits_cube_bands_check
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param cube          Metadata about a data cube
#' @param bands         Requested bands of the data sample
#' @return              Checked bands (cube bands if bands are NULL)
#'
.sits_cube_bands_check <- function(cube, bands = NULL) {
	# check the bands are available
	cb_bands <- sits_bands(cube)
	if (purrr::is_null(bands))
		bands <- cb_bands
	else {
		bands <- toupper(bands)
		assertthat::assert_that(all(bands %in% cb_bands),
								msg = "sits_from_wtss: bands are not available in the cube")
	}
	return(bands)
}
