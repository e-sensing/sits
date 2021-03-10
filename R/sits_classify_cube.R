#' @title Classify a chunk of raster data  using multicores
#' @name .sits_classify_multicores
#' @keywords internal
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
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
#' @param  tile            a single tile of a data cube.
#' @param  ml_model        model trained by \code{\link[sits]{sits_train}}.
#' @param  roi             region of interest
#' @param  filter_fn       smoothing filter function to be applied to the data.
#' @param  impute_fn       impute function to replace NA
#' @param  interp_fn       function to interpolate points from cube to match samples
#' @param  compose_fn      function to compose points from cube to match samples
#' @param  memsize         memory available for classification (in GB).
#' @param  multicores      number of cores.
#' @param  output_dir      output directory
#' @param  version         version of result
#' @return List of the classified raster layers.
.sits_classify_multicores <- function(tile,
                                      ml_model,
                                      roi,
                                      filter_fn,
                                      impute_fn,
                                      interp_fn,
                                      compose_fn,
                                      memsize,
                                      multicores,
                                      output_dir,
                                      version) {

    # retrieve the samples from the model
    samples <- environment(ml_model)$data
    # retrieve the labels
    labels <- sits_labels(samples)$label

    # precondition - are the samples empty?
    assertthat::assert_that(nrow(samples) > 0,
        msg = "sits_classify: original samples not saved"
    )
    # precondition - are the sample bands contained in the cube bands?
    tile_bands <- sits_bands(tile)
    bands <- sits_bands(samples)
    assertthat::assert_that(
      all(bands %in% tile_bands),
      msg = "sits_classify: some bands in samples are not in cube"
    )

    # retrieve the normalization stats from the model
    stats <- environment(ml_model)$stats

    # is there a region of interest?
    if (purrr::is_null(roi)) {
          sub_image <- .sits_raster_sub_image_default(tile)
      } else {
          # define the sub_image
          sub_image <- .sits_raster_sub_image(cube = tile, roi = roi)
      }

    # divide the input data in blocks
    block_info <- .sits_raster_blocks(
        cube = tile,
        ml_model = ml_model,
        sub_image = sub_image,
        memsize = memsize,
        multicores = multicores
    )

    message(paste0(
            "Using ", block_info$n,
            " blocks of size (", block_info$nrows[1],
            " x ", block_info$ncols[1]), ")"
    )
    # create the metadata for the probability cube
    probs_cube <- .sits_cube_probs(
        tile = tile,
        samples = samples,
        sub_image = sub_image,
        output_dir = output_dir,
        version = version
    )

    # get initial time for classification
    start_time <- lubridate::now()
    message(sprintf("Starting classification at %s", start_time))

    # read the blocks and compute the probabilities
    probs_blocks <- purrr::map(c(1:block_info$n), function(b) {
        # define the extent for each block
        extent <- c(
            block_info$row[b], block_info$nrows[b],
            block_info$col, block_info$ncols
        )
        names(extent) <- (c("row", "nrows", "col", "ncols"))

        # read the data
        distances <- .sits_raster_data_read(
            cube = tile,
            samples = samples,
            extent = extent,
            stats = stats,
            filter_fn = filter_fn,
            impute_fn = impute_fn,
            interp_fn = interp_fn,
            compose_fn = compose_fn,
            multicores = multicores
        )

        # get the attribute names
        attr_names <- names(.sits_distances(samples[1, ]))
        assertthat::assert_that(length(attr_names) > 0,
                      msg = "sits_classify: training data not available"
        )
        # set column names for DT
        colnames(distances) <- attr_names
        # predict the classification values
        prediction <- .sits_classify_interval(
            data = distances,
            ml_model = ml_model,
            multicores = multicores
        )
        # convert probabilities matrix to INT2U
        scale_factor_save <- round(1/.sits_config_probs_scale_factor())
        prediction <- round(scale_factor_save * prediction, digits = 0)

        # estimate processing time
        .sits_est_class_time(
            start_time = start_time,
            n_blocks = block_info$n,
            block = b
        )

        return(prediction)
    })
    # combine the image to make a probability cube
    probs <- do.call(rbind, probs_blocks)

    # define the file name of the raster file to be written
    filename <- probs_cube$file_info[[1]]$path

    # write the probabilities to a raster file
    .sits_raster_api_write(
        params = .sits_raster_api_params_cube(probs_cube),
        num_layers = length(labels),
        values = probs,
        filename = filename,
        datatype = "INT2U"
    )
    return(probs_cube)
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
.sits_classify_check_params <- function(cube, ml_model) {
    # ensure metadata tibble exists
    assertthat::assert_that(NROW(cube) > 0,
        msg = "sits_classify: invalid metadata for the cube"
    )

    # ensure the machine learning model has been built
    assertthat::assert_that(!purrr::is_null(ml_model),
        msg = "sits-classify: trained ML model not available"
    )

    return(invisible(TRUE))
}


#' @title Classify one interval of data
#' @name  .sits_classify_interval
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param  data              A data.table with distance values.
#' @param  ml_model          Machine learning model to be applied.
#' @param  multicores        Number of cores to process the time series.
#' @return                   A data table with predicted values of probs
.sits_classify_interval <- function(data, ml_model, multicores) {

    # keras, ranger and xgb models do internal parallel processing
    if ("keras_model" %in% class(ml_model)
    | "ranger_model" %in% class(ml_model)
    | "xgb_model" %in% class(ml_model)) {
      multicores <- 1
      }

    # classify a block of data (with data split)
    classify_block <- function(block) {
        # predict the values for each time interval
        pred_block <- ml_model(block)
        return(pred_block)
    }
    # set up multicore processing
    if (multicores > 1) {
        # estimate the list for breaking a block
        data_blocks <- .sits_raster_data_split(data, multicores)
        # apply parallel processing to the split data
        # (return the results in a list inside a prototype)
        prediction_blocks <- parallel::mclapply(data_blocks,
            classify_block,
            mc.cores = multicores
        )

        # compose result based on output from different cores
        prediction <- data.table::as.data.table(
          do.call(rbind, prediction_blocks)
          )
    }
    else {
        # single core
        # estimate the prediction vector
        prediction <- ml_model(data)
    }

    # are the results consistent with the data input?
    assertthat::assert_that(nrow(prediction) == nrow(data),
        msg = ".sits_classify_cube -
                    number of rows of probability matrix is different
                    from number of input pixels"
    )

    return(prediction)
}
