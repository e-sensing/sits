#' @title Classify a chunk of raster data  using multicores
#' @name .sits_classify_multicores
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Classifies a block of data using multicores. It breaks
#' the data into horizontal blocks and divides them between the available cores.
#'
#' Reads data using Rgdal, cleans the data for NAs and missing values.
#' The clean data is stored in a data table with the time instances
#' for all pixels of the block. The algorithm then classifies data on
#' an year by year basis. For each year, extracts the sub-blocks for each band.
#'
#' After all cores process their blocks, it joins the result and then writes it
#' in the classified images for each corresponding year.
#'
#' @param  cube            Metadata cube derived from a raster brick.
#' @param  samples         Samples used for training the classification model.
#' @param  ml_model        A model trained by \code{\link[sits]{sits_train}}.
#' @param  interval        Classification interval.
#' @param  filter          Smoothing filter to be applied to the data.
#' @param  memsize         Memory available for classification (in GB).
#' @param  multicores      Number of cores.
#' @param  output_dir      Output directory
#' @param  version         Version of result
#' @return List of the classified raster layers.
.sits_classify_multicores <-  function(cube,
									   samples,
									   ml_model,
									   interval,
									   filter,
									   memsize,
									   multicores,
									   output_dir,
									   version) {



	# create the metadata for the classified cube
	cube_class <- .sits_cube_classified(cube = cube,
										samples = samples,
										interval = interval,
										output_dir = output_dir,
										version = version)
	# find out how many layers per brick
	n_layers  <- length(sits_labels(samples)$label)

	# create the Raster objects
	n_objs <- length(.sits_cube_files(cube_class))
	bricks <- vector("list", n_objs)

	# create the outbut bricks
	bricks <- purrr::map(bricks, function(brick){
		brick <- suppressWarnings(raster::brick(nl    = n_layers,
												nrows = cube_class$nrows,
												ncols = cube_class$ncols,
												xmn   = cube_class$xmin,
												xmx   = cube_class$xmax,
												ymn   = cube_class$ymin,
												ymx   = cube_class$ymax,
												crs   = cube_class$crs))
		return(brick)
	})

	# initiate writing
	bricks <- purrr::map2(bricks, c(1:n_objs), function(brick, i){
		brick <- suppressWarnings(raster::writeStart(brick,
													 filename = .sits_cube_file(cube_class, i),
													 format   = "GTiff",
													 datatype = "INT2U",
													 overwrite = TRUE))
	})
	# retrieve the normalization stats
	stats     <- environment(ml_model)$stats

	# divide the input data in blocks
	bs <- .sits_raster_blocks(cube, ml_model, interval, memsize, multicores)

	# build a list with columns of data table to be processed for each interval
	select.lst <- .sits_timeline_raster_indexes(cube, samples, interval)

	# get the attribute names
	attr_names <- names(.sits_distances(environment(ml_model)$data[1,]))
	assertthat::assert_that(length(attr_names) > 0,
							msg = "sits_classify_distances: training data not available")
	# get initial time for classification
	start_time <- lubridate::now()
	message(sprintf("Starting classification at %s", start_time))

	# read the blocks
	for (block in 1:bs$n) {
		# read the data
		data_DT <- .sits_raster_read_data(cube = cube,
										  samples = samples,
										  ml_model = ml_model,
										  first_row = bs$row[block],
										  n_rows_block = bs$nrows[block],
										  stats = stats,
										  filter = filter,
										  multicores = multicores)
		# process one temporal instance at a time
		n_bricks <- length(bricks)

		bricks <- purrr::pmap(list(select.lst, bricks, c(1:n_bricks)),
							  function(time, brick, iter) {
							  	# retrieve the values used for classification
							  	if (all(time))
							  		dist_DT <- data_DT
							  	else {
							  		dist_DT <- data_DT[, time, with = FALSE]
							  		# set column names for DT
							  	}
							  	colnames(dist_DT) <- attr_names
							  	# predict the classification values
							  	prediction_DT <- .sits_classify_interval(DT          = dist_DT,
							  											 ml_model   = ml_model,
							  											 multicores = multicores)


							  	# convert probabilities matrix to INT2U
							  	scale_factor_save <- 10000
							  	probs  <- .sits_raster_scale_matrix_integer(
							  		values.mx    = as.matrix(prediction_DT),
							  		scale_factor = scale_factor_save,
							  		multicores   = multicores)

							  	# write the probabilities
							  	brick <- suppressWarnings(raster::writeValues(brick, probs,
							  												  bs$row[block]))

							  	# memory management
							  	rm(prediction_DT)
							  	gc()
							  	.sits_log_debug(paste0("Memory used after processing block ",
							  						   block, " of interval ", iter, " - ",
							  						   .sits_mem_used(), " GB"))

							  	# estimate processing time
							  	.sits_classify_estimate_processing_time(start_time = start_time,
							  											select.lst = select.lst,
							  											bs = bs, block = block,
							  											time = iter)
							  	return(brick)
							  })

		# save information about memory use for debugging later
		.sits_log_debug(paste0("Processed block starting from ",
							   bs$row[block], " to ", (bs$row[block] + bs$nrows[block] - 1)))
		.sits_log_debug(paste0("Memory used after processing block ",
							   block,  " - ", .sits_mem_used(), " GB"))

	}

	# finish writing
	bricks <- purrr::map(bricks, function(brick){
		brick <- suppressWarnings(raster::writeStop(brick))
	})
	return(cube_class)
}




#' @title Check clasification parameters
#' @name .sits_classify_check_params
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description Verify that required parameters are correct.
#'
#' @param  cube            Tibble with information about a data cube.
#' @param  ml_model        An R model trained by \code{\link[sits]{sits_train}}.
#' @return Tests succeeded?
.sits_classify_check_params <- function(cube, ml_model){
	# ensure metadata tibble exists
	assertthat::assert_that(NROW(cube) > 0,
							msg = "sits_classify: invalid metadata for the cube")

	# ensure the machine learning model has been built
	assertthat::assert_that(!purrr::is_null(ml_model),
							msg = "sits-classify: trained ML model not available")

	return(invisible(TRUE))
}

#' @title Estimate the processing time
#' @name .sits_classify_estimate_processing_time
#' @keywords internal
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description This function normalizes values read from a raster brick.
#'
#' @param  start_time     Initial processing time.
#' @param  select.lst     List of time intervals.
#' @param  bs             Raster block parameters.
#' @param  block          Current block.
#' @param  time           Current interval.
#' @return Scaled matrix.
.sits_classify_estimate_processing_time <- function(start_time,
													select.lst,
													bs,
													block,
													time) {
	# compute current time
	current_time <- lubridate::now()

	# compute elapsed time and estimates remaining time
	elapsed_time <- lubridate::time_length(current_time - start_time,
										   unit = "minute")
	elapsed_intervals <- (block - 1) * length(select.lst) + time
	total_intervals   <- bs$n * length(select.lst)
	if (elapsed_intervals < total_intervals) {
		message(sprintf(
			"Elapsed time %s minute(s).
         Estimated total process time %s minute(s)...",
			round(as.numeric(elapsed_time), 1),
			round(as.numeric((total_intervals/elapsed_intervals) * elapsed_time),
				  1)))
	} else {
		message(sprintf(
			"Classification finished at %s. Total elapsed time: %s minute(s).",
			current_time,
			round(as.numeric(elapsed_time), 1)))
	}
	return(invisible(TRUE))
}

#' @title Classify one interval of data
#' @name  .sits_classify_interval
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param  DT                A data.table with distance values.
#' @param  ml_model          Machine learning model to be applied.
#' @param  multicores        Number of cores to process the time series.
#' @return                   A data table with predicted values of probs
.sits_classify_interval <- function(DT, ml_model, multicores) {
	nrows_DT <- nrow(DT)
	proc_cores <- multicores
	if (!(purrr::is_null(environment(ml_model)$model.keras)) ||
		!(purrr::is_null(environment(ml_model)$result_ranger)) ) {
		proc_cores <- 1
		.sits_log_debug(
			paste0("keras and ranger run on multiple CPUs -
                   setting multicores to 1"))
	}

	# classify a block of data (with data split)
	classify_block <- function(block) {
		# predict the values for each time interval
		pred_block <- ml_model(block)
		return(pred_block)
	}
	# set up multicore processing
	if (proc_cores > 1) {
		# estimate the list for breaking a block
		.sits_log_debug(
			paste0("Memory used before split data - ", .sits_mem_used(), " GB"))
		block.lst <- .sits_raster_split_data(DT, proc_cores)
		# memory management
		rm(DT)
		gc()

		.sits_log_debug(
			paste0("Memory used before mcapply - ", .sits_mem_used(), " GB"))
		# apply parallel processing to the split data
		# (return the results in a list inside a prototype)
		predictions.lst <- parallel::mclapply(block.lst,
											  classify_block,
											  mc.cores = proc_cores)

		#memory management
		rm(block.lst)
		gc()
		.sits_log_debug(
			paste0("Memory used after mclapply - ", .sits_mem_used(), " GB"))
		# compose result based on output from different cores
		prediction_DT <- data.table::as.data.table(do.call(rbind,predictions.lst))
		# memory management
		rm(predictions.lst)
		gc()
		.sits_log_debug(
			paste0("Memory after removing predictions - ",
				   .sits_mem_used(), " GB"))
	}
	else {
		# memory management
		.sits_log_debug(
			paste0("Memory used before prediction - ", .sits_mem_used(), " GB"))

		# estimate the prediction vector
		prediction_DT <- ml_model(DT)
		# memory management
		rm(DT)
		gc()
	}

	# are the results consistent with the data input?
	assertthat::assert_that(nrow(prediction_DT) == nrows_DT,
							msg = ".sits_classify_cube -
                    number of rows of probability matrix is different
                    from number of input pixels")

	return(prediction_DT)
}
