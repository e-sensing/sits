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
.sits_classify_multicores <- function(cube,
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
    samples <- environment(ml_model)$data

    # precondition - are the samples empty?
    assertthat::assert_that(nrow(samples) > 0,
        msg = "sits_classify: original samples not saved"
    )
    # precondition - are the cube bands the same as the sample bands?
    cube_bands <- sits_bands(cube)
    bands <- sits_bands(samples)
    assertthat::assert_that(
      all(bands %in% cube_bands),
      msg = "sits_classify: bands in samples different from cube bands"
    )

    # retrieve the normalization stats from the model
    stats <- environment(ml_model)$stats

    # get the attribute names
    attr_names <- names(.sits_distances(environment(ml_model)$data[1, ]))
    assertthat::assert_that(length(attr_names) > 0,
        msg = "sits_classify: training data not available"
    )

    # is there a region of interest?
    if (purrr::is_null(roi)) {
          sub_image <- .sits_raster_sub_image_default(cube)
      } else {
          # define the sub_image
          sub_image <- .sits_raster_sub_image(cube = cube, roi = roi)
      }

    # postcondition for subimage
    if (purrr::is_null(sub_image)) {
        message("region of interest outside of cube")
        return(NULL)
    }

    # divide the input data in blocks
    block_info <- .sits_raster_blocks(
        cube = cube,
        ml_model = ml_model,
        sub_image = sub_image,
        memsize = memsize,
        multicores = multicores
    )

    if (.verbose) {
        message(paste0(
            "Using ", block_info$n,
            " blocks of size (", block_info$nrows[1],
            " x ", block_info$ncols[1]
        ), ")")
    }

    # create the metadata for the classified cube
    cube_class <- .sits_cube_classified(
        cube = cube,
        samples = samples,
        name = name,
        sub_image = sub_image,
        output_dir = output_dir,
        version = version
    )

    # number of output raster objects
    n_objs <- length(.sits_cube_files(cube_class))

    # build a list with columns of data table to be processed for each interval
    select_lst <- .sits_timeline_raster_indexes(cube, samples)

    # number of classified cubes has to be equal the number of time instances
    assertthat::assert_that(n_objs == length(select_lst),
                        msg = "sits_classify_cube: problems with time indexes")

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
        if (.verbose) {
            message(paste0("Read and preprocess block ", b))
            read_data_start_time <- lubridate::now()
        }
        data <- .sits_raster_data_read(
            cube = cube,
            samples = samples,
            extent = extent,
            stats = stats,
            filter = filter,
            impute_fn = impute_fn,
            multicores = multicores,
            .verbose = .verbose
        )
        if (.verbose) {
              .sits_processing_task_time(
                  "Read block",
                  read_data_start_time
              )
          }

        # process one temporal instance at a time
        probs_time <- purrr::pmap(
            list(select_lst, c(1:n_objs)),
            function(time, iter) {
              # retrieve the values to be used for classification
              if (all(time)) {
                distances <- data
              } else {
                distances <- data[, time, with = FALSE]
              }
              # set column names for DT
              colnames(distances) <- attr_names
              # predict the classification values
              prediction <- .sits_classify_interval(
                data = distances,
                ml_model = ml_model,
                multicores = multicores
              )
              # convert probabilities matrix to INT2U
              scale_factor_save <- round(1/cube[1,]$scale_factors[[1]][1])
              prediction <- round(scale_factor_save * prediction, digits = 0)

              # estimate processing time
              .sits_est_class_time(
                start_time = start_time,
                n_intervals = length(select_lst),
                bs = block_info,
                block = b,
                time = iter
              )
              return(prediction)
            }
        )

        return(probs_time)
    })
    # now we have to untangle the probabilities
    n_blocks <- length(probs_blocks)
    n_times <- length(probs_blocks[[1]])

    # find out what are the labels
    labels <- sits_labels(samples)$label

    purrr::map(c(1:n_times), function(t) {
        b <- 1
        probs_time <- probs_blocks[[b]][[t]]
        while (b < n_blocks) {
            b <- b + 1
            probs_time <- rbind(probs_time, probs_blocks[[b]][[t]])
        }
        # define the file name of the raster file to be written
        filename <- .sits_cube_file(cube_class, t)

        # write the probabilities to a raster file
        .sits_raster_api_write(
            params = .sits_raster_api_params_cube(cube_class),
            num_layers = length(labels),
            values = probs_time,
            filename = filename,
            datatype = "INT2U"
        )
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
