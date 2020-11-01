#' @title Classify a chunk of raster data  using multicores
#' @name .sits_classify_multicores
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Classifies a block of data using multicores. It breaks
#' the data into horizontal blocks and divides them between the available cores.
#'
#' Reads data using terra, cleans the data for NAs and missing values.
#' The clean data is stored in a data table with the time instances
#' for all pixels of the block. The algorithm then classifies data on
#' an year by year basis. For each year, extracts the sub-blocks for each band.
#'
#' After all cores process their blocks, it joins the result and then writes it
#' in the classified images for each corresponding year.
#'
#' @param  cube            data cube.
#' @param  ml_model        model trained by \code{\link[sits]{sits_train}}.
#' @param  name            name of the output data cube
#' @param  roi             region of interest
#' @param  filter          smoothing filter to be applied to the data.
#' @param  impute_fn       impute function to replace NA
#' @param  memsize         memory available for classification (in GB).
#' @param  multicores      number of cores.
#' @param  output_dir      output directory
#' @param  version         version of result
#' @param  .verbose        print information about processing steps
#' @return List of the classified raster layers.
.sits_classify_multicores <-  function(cube,
                                       ml_model,
                                       name,
                                       roi,
                                       filter,
                                       impute_fn,
                                       memsize,
                                       multicores,
                                       output_dir,
                                       version,
                                       .verbose) {

    # retrieve the samples from the model
    samples  <- environment(ml_model)$data
    # precondition - are the samples correct?
    assertthat::assert_that(NROW(samples) > 0,
                            msg = "sits_classify: original samples not saved")

    # precondition - are the cube bands the same as the sample bands?
    cube_bands   <- .sits_cube_bands(cube)
    bands <- sits_bands(samples)
    assertthat::assert_that(
        all(bands %in% cube_bands),
        msg = "sits_classify: bands in samples different from cube bands")

    # is there a region of interest?
    if (purrr::is_null(roi))
        sub_image <- .sits_raster_sub_image_default(cube)
    else
        # define the sub_image
        sub_image <- .sits_raster_sub_image(cube = cube, roi = roi)

    if (purrr::is_null(sub_image)){
        message("region of interest outside of cube")
        return(NULL)
    }

    # divide the input data in blocks
    block_info <- .sits_raster_blocks(cube       = cube,
                                      ml_model   = ml_model,
                                      sub_image  = sub_image,
                                      memsize    = memsize,
                                      multicores = multicores)

    if (.verbose) {
        message(paste0("Using ", block_info$n,
                       " blocks of size (", block_info$nrows[1],
                       " x ", block_info$ncols[1]),")")
    }

    # create the metadata for the classified cube
    cube_class <- .sits_cube_classified(cube       = cube,
                                        samples    = samples,
                                        name       = name,
                                        sub_image  = sub_image,
                                        output_dir = output_dir,
                                        version    = version)

    # number of output raster objects
    n_objs <- length(.sits_cube_files(cube_class))

    # retrieve the normalization stats
    stats   <- environment(ml_model)$stats

    # build a list with columns of data table to be processed for each interval
    select.lst <- .sits_timeline_raster_indexes(cube, samples)

    # get the attribute names
    attr_names <- names(.sits_distances(environment(ml_model)$data[1,]))
    assertthat::assert_that(length(attr_names) > 0,
                            msg = "sits_classify_distances: training data not available")

    # create the input raster objects
    t_obj.lst <- purrr::map(bands, function(b){
        t_obj <- .sits_cube_terra_obj_band(cube, b)
    })
    # # does the cube have a cloud band?
    cld_band <- .sits_config_cloud_band(cube)
    if (cld_band %in% sits_bands(cube))
         t_obj_cld <- .sits_cube_terra_obj_band(cube, cld_band)
    else
         t_obj_cld <- NULL

    # get initial time for classification
    start_time <- lubridate::now()
    message(sprintf("Starting classification at %s", start_time))

    # read the blocks
    blocks.lst <- purrr::map(c(1:block_info$n), function (b) {
        # define the extent
        extent <- c(block_info$row[b], block_info$nrows[b],
                    block_info$col, block_info$ncols)
        names(extent) <- (c("row", "nrows", "col", "ncols"))
        # read the data
        if (.verbose) {
            message(paste0("Read and preprocess block ", b))
            read_data_start_time <- lubridate::now()
        }
        data_DT <- .sits_raster_read_data(cube         = cube,
                                          samples      = samples,
                                          obj.lst      = t_obj.lst,
                                          obj_cld      = t_obj_cld,
                                          extent       = extent,
                                          stats        = stats,
                                          filter       = filter,
                                          impute_fn    = impute_fn,
                                          multicores   = multicores,
                                          .verbose     = .verbose)
        if (.verbose)
            .sits_processing_estimate_task_time("Read block",
                                                read_data_start_time)

        if (.verbose) classify_start_time <- lubridate::now()

        # process one temporal instance at a time
        probs.lst <- purrr::pmap(list(select.lst, c(1:n_objs)),
                    function(time, iter) {
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

                        # memory management
                        # rm(prediction_DT)
                        # gc()

                        # estimate processing time
                        .sits_processing_estimate_classification_time(
                            start_time = start_time,
                            n_intervals = length(select.lst),
                            bs = block_info,
                            block = b,
                            time = iter)
                        return(probs)
                    })

        return(probs.lst)
    })
    # now we have to untangle the probabilities
    n_blocks <- length(blocks.lst)
    n_times  <- length(blocks.lst[[1]])

    # find out how many layers per brick
    n_layers  <- length(sits_labels(samples)$label)

    purrr::map(c(1:n_times), function (t) {
        b <- 1
        probs <- blocks.lst[[b]][[t]]
        while (b < n_blocks)  {
            b <- b + 1
            probs <- rbind(probs, blocks.lst[[b]][[t]])
        }
        t_obj <- terra::rast(nrows  = cube_class$nrows,
                             ncols  = cube_class$ncols,
                             nlyrs  = n_layers,
                             xmin   = cube_class$xmin,
                             xmax   = cube_class$xmax,
                             ymin   = cube_class$ymin,
                             ymax   = cube_class$ymax,
                             crs    = cube_class$crs)

        terra::values(t_obj) <- probs

        terra::writeRaster(t_obj,
                           filename = .sits_cube_file(cube_class, t),
                           wopt     = list(filetype  = "GTiff",
                                           datatype = "INT2U"),
                           overwrite = TRUE)
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
	if ( "keras_model"   %in% class(ml_model)
	    | "ranger_model" %in% class(ml_model)
	    | "xgb_model"    %in% class(ml_model))
		proc_cores <- 1

	# classify a block of data (with data split)
	classify_block <- function(block) {
		# predict the values for each time interval
		pred_block <- ml_model(block)
		return(pred_block)
	}
	# set up multicore processing
	if (proc_cores > 1) {
		# estimate the list for breaking a block
		block.lst <- .sits_raster_split_data(DT, proc_cores)
		# memory management
		# rm(DT)
		# gc()
		# apply parallel processing to the split data
		# (return the results in a list inside a prototype)
		predictions.lst <- parallel::mclapply(block.lst,
											  classify_block,
											  mc.cores = proc_cores)

		#memory management
		# rm(block.lst)
		# gc()
		# compose result based on output from different cores
		prediction_DT <- data.table::as.data.table(do.call(rbind,predictions.lst))
		# memory management
		# rm(predictions.lst)
		# gc()
	}
	else {

		# estimate the prediction vector
		prediction_DT <- ml_model(DT)
		# memory management
		# rm(DT)
		# gc()
	}

	# are the results consistent with the data input?
	assertthat::assert_that(nrow(prediction_DT) == nrows_DT,
							msg = ".sits_classify_cube -
                    number of rows of probability matrix is different
                    from number of input pixels")

    return(prediction_DT)
}

