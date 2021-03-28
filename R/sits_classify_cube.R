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
#' @param  verbose         print processing information?
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
                                      version,
                                      verbose) {

    # some models have parallel processing built in
    if ("keras_model" %in% class(ml_model) | "ranger_model" %in% class(ml_model)
        | "xgb_model" %in% class(ml_model))
        multicores <- 1
    # retrieve the samples from the model
    samples <- .sits_ml_model_samples(ml_model)

    # retrieve the labels
    labels <- sits_labels(samples)

    # precondition - are the samples empty?
    assertthat::assert_that(
        nrow(samples) > 0,
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
    blocks <- .sits_raster_blocks(
        tile = tile,
        ml_model = ml_model,
        sub_image = sub_image,
        memsize = memsize,
        multicores = multicores
    )
    if (verbose) {
        message(paste0("Using ", length(blocks),
            " blocks of size (", unname(blocks[[1]]["nrows"]),
            " x ", unname(blocks[[1]]["ncols"]), ")"
        ))
    }

    # create the metadata for the probability cube
    probs_cube <- .sits_cube_probs(
        tile = tile,
        samples = samples,
        sub_image = sub_image,
        output_dir = output_dir,
        version = version
    )

    # show initial time for classification
    if (verbose)
        message(sprintf("Starting classification at %s", lubridate::now()))

    # save original future plan
    if (multicores > 1) {
        oplan <- future::plan("multisession", workers = multicores)
    } else {
        oplan <- future::plan("sequential")
    }
    on.exit(future::plan(oplan), add = TRUE)

    # read the blocks and compute the probabilities
    filenames <- furrr::future_map(blocks, function(b) {
        # read the data
        distances <- .sits_raster_data_read(
            cube = tile,
            samples = samples,
            extent = b,
            stats = stats,
            filter_fn = filter_fn,
            impute_fn = impute_fn,
            interp_fn = interp_fn,
            compose_fn = compose_fn
        )

        # get the attribute names
        attr_names <- names(.sits_distances(samples[1, ]))

        # set column names for DT
        colnames(distances) <- attr_names

        # predict the classification values
        pred_block <- ml_model(distances)

        # are the results consistent with the data input?
        assertthat::assert_that(nrow(pred_block) == nrow(distances),
            msg = paste(".sits_classify_cube: number of rows of probability",
                        "matrix is different from number of input pixels")
        )

        # convert probabilities matrix to INT2U
        scale_factor_save <- round(1 / .sits_config_probs_scale_factor())
        pred_block <- round(scale_factor_save * pred_block, digits = 0)

        # define the file name of the raster file to be written
        filename_block <- paste0(
            tools::file_path_sans_ext(probs_cube$file_info[[1]]$path),
            "_block_", b["row"], ".tif"
        )

        # write the probabilities to a raster file
        .sits_raster_api_write(
            params = .sits_raster_api_params_block(probs_cube, b),
            num_layers = length(labels),
            values = pred_block,
            filename = filename_block,
            datatype = "INT2U"
        )

        return(filename_block)
    }, .progress = TRUE)

    # Join the predictions
    .sits_raster_api_merge(in_files = unlist(filenames),
                           out_file = probs_cube$file_info[[1]]$path,
                           gdal_datatype = "UInt16",
                           overwrite = TRUE
    )

    # show final time for classification
    if (verbose)
        message(sprintf("End classification at %s", lubridate::now()))

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
    assertthat::assert_that(
        nrow(cube) > 0,
        msg = "sits_classify: invalid metadata for the cube"
    )

    # ensure the machine learning model has been built
    assertthat::assert_that(
        !purrr::is_null(ml_model),
        msg = "sits_classify: trained ML model not available"
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
#' @return                   A data table with predicted values of probs
.sits_classify_interval <- function(data, ml_model) {

    # single core
    # estimate the prediction vector
    prediction <- ml_model(data)

    # are the results consistent with the data input?
    assertthat::assert_that(
        nrow(prediction) == nrow(data),
        msg = paste(".sits_classify_cube: number of rows of probability",
                    "matrix is different from number of input pixels")
    )

    return(prediction)
}
