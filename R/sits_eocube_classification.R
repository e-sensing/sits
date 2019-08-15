#' @title Classify a data cube created with the EOCUBES service
#' @name .sits_classify_eocubes
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Takes a set of spatio-temporal raster bricks, whose metadata is
#'              described by tibble (created by \code{\link[sits]{sits_cube}}),
#'              a set of samples used for training a classification model,
#'              a prediction model (created by \code{\link[sits]{sits_train}}),
#'              and produces a classified set of RasterLayers. This function is similar to
#'               \code{\link[sits]{sits_classify}} which is applied to time series stored in a sits tibble.
#'               There are two parameters for optimizing processing of large data sets. These
#'               parameters are "memsize" and "multicores". The "multicores" parameter defines the
#'               number of cores used for processing. The "memsize" parameter  controls
#'               the amount of memory available for classification.
#'
#' @param  cube            Tibble with information about a data cube.
#' @param  ml_model        An R model trained by \code{\link[sits]{sits_train}}.
#' @param  samples         Tibble with samples used for training the classification model.
#' @param  interval        Interval between two sucessive classifications, expressed in months.
#' @param  filter          Smoothing filter to be applied (if desired).
#' @param  memsize         Memory available for classification (in GB).
#' @param  multicores      Number of cores to be used for classification.
#' @return A tibble with the metadata for the vector of classified RasterLayers.
#'
.sits_classify_eocubes <- function(cube, ml_model, samples, interval, filter, memsize, multicores) {

    # get cube object
    cub.obj <- .sits_cube_robj(cube)

    # get bands names
    bands <- .sits_cube_bands(cube)

    # get bands info
    bands_info <- EOCubes::cube_bands_info(cube = cub.obj)

    # what is the reference start date?
    dates          <- sits_time_series_dates(samples)
    ref_start_date <- lubridate::as_date(dates[1])
    ts_length      <- length(dates)

    # get stacks from EOCubes
    stk.obj <- EOCubes::stacks(cube = cub.obj, bands = bands,
                               start_reference = ref_start_date, stack_length = ts_length,
                               starts_interval = interval)
    # get the params of the cube
    params <- .sits_raster_params(.sits_cube_robj(cube))
    # get the name of the cube
    name   <-  paste0(cube[1,]$name, "_probs")

    cube_class.tb <-
        dplyr::bind_rows(lapply(seq_along(stk.obj), function(i) {

            dplyr::bind_rows(lapply(seq_along(stk.obj[[i]]), function(j) {

                # tile/interval
                tile_interv <- stk.obj[[i]][[j]]

                # file sufix
                file_sufx <- names(stk.obj)[[i]]
                # set the metadate for the probability cube
                cube_stack <- .sits_create_cube(service = "STACK",
                                  URL       = cube$URL,
                                  satellite = cube$satellite,
                                  sensor    = cube$sensor,
                                  name      = file_sufx,
                                  bands     = bands,
                                  labels    = cube$labels,
                                  timelines  = list(tile_interv$timeline),
                                  missing_values = bands_info$fill[bands],
                                  scale_factors = bands_info$scale[bands],
                                  minimum_values = bands_info$min[bands],
                                  maximum_values = bands_info$max[bands],
                                  xmin  = params$xmin, xmax  = params$xmax,
                                  ymin  = params$ymin, ymax  = params$ymax,
                                  xres  = params$xres, yres  = params$yres,
                                  crs   = params$xmin,
                                  files = tile_interv$bands)

                # checks the classification params
                .sits_check_classify_params(cube_stack, ml_model)

                # create the raster objects and their respective filenames
                cube_class <- .sits_cube_classified(cube_stack, samples, interval)

                # classify the data
                cube_class.tb <- .sits_classify_multicores_cubes(cube_stack, cube_class, samples,
                                                                 ml_model, interval, filter,
                                                                 memsize, multicores)
            }))
        }))

    return(cube_class.tb)
}

#' @title Classify a stacks chunk using multicores
#' @name .sits_classify_multicores_cubes
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Classifies a block of data using multicores. It breaks
#' the data into horizontal blocks and divides them between the available cores.
#'
#' Reads data from a file using Rgdal, then cleans the data for NAs and missing values. The clean
#' data is stored in a data table that has all the time instances for all pixels of
#' the block. The algorithm then classifies data on an year by year basis.
#' For each year, it extracts the sub-blocks for each band.
#'
#' After all cores process their blocks, it joins the result and then writes it
#' in the classified images for each corresponding year.
#'
#' @param  cube            Tibble with metadata for a data cube derived from a raster brick.
#' @param  cube_class      Raster layer objects to be written.
#' @param  samples         Tibble with samples used for training the classification model.
#' @param  ml_model        A model trained by \code{\link[sits]{sits_train}}.
#' @param  interval        Classification interval.
#' @param  filter          Smoothing filter to be applied to the data.
#' @param  memsize         Memory available for classification (in GB).
#' @param  multicores      Number of cores.
#' @return List of the classified raster layers.
.sits_classify_multicores_cubes <-  function(cube,
                                             cube_class,
                                             samples,
                                             ml_model,
                                             interval,
                                             filter,
                                             memsize,
                                             multicores) {
    # retrieve the output raster layers
    bricks_probs <- .sits_cube_all_robjs(cube_class)

    #initiate writing
    for (i in 1:length(bricks_probs))
        bricks_probs[[i]] <- raster::writeStart(bricks_probs[[i]],
                                                    bricks_probs[[i]]@file@name, overwrite = TRUE)

    # retrieve the normalization stats
    stats     <- environment(ml_model)$stats.tb

    # divide the input data in blocks
    bs <- .sits_raster_blocks(cube, ml_model, interval, memsize, multicores)

    # build a list with columns of data table to be processed for each interval
    select.lst <- .sits_select_raster_indexes(cube, samples, interval)

    # get the attribute names
    attr_names <- names(environment(ml_model)$train_data_DT)
    ensurer::ensure_that(attr_names, length(.) > 0,
                         err_desc = "sits_classify_distances: training data not saved in the model environment")
    # get initial time for classification
    start_time <- lubridate::now()
    message(sprintf("Starting classification at %s", start_time))

    # read the blocks
    for (block in 1:bs$n) {
        # read the data
        data_DT <- .sits_read_data_cubes(cube, samples, ml_model,
                                         bs$row[block], bs$nrows[block],
                                         stats, filter, multicores)
        # process one temporal instance at a time

        for (time in 1:length(select.lst)) {
            # retrieve the values used for classification
            if (all(select.lst[[time]]))
                dist_DT <- data_DT
            else {
                dist_DT <- data_DT[, select.lst[[time]], with = FALSE]
                # set column names for DT
            }
            colnames(dist_DT) <- attr_names
            # predict the classification values
            prediction_DT <- .sits_predict_interval(dist_DT, time, ml_model, bs$row[block], multicores)


            # convert probabilities matrix to INT2U
            scale_factor_save <- 10000
            probs  <- .sits_scale_matrix_integer(as.matrix(prediction_DT), scale_factor_save, multicores)

            # write the probabilities
            bricks_probs[[time]] <- raster::writeValues(bricks_probs[[time]], probs, bs$row[block])

            # memory management
            rm(prediction_DT)
            gc()

            .sits_log_debug(paste0("Memory used after processing block ",
                                   block, " of year ", time, " - ", .sits_mem_used(), " GB"))
            # estimate processing time
            .sits_estimate_processing_time(start_time, select.lst, bs, block, time)
        }

        # save information about memory use for debugging later
        .sits_log_debug(paste0("Processed block starting from ",
                               bs$row[block], " to ", (bs$row[block] + bs$nrows[block] - 1)))
        .sits_log_debug(paste0("Memory used after processing block ",
                               block,  " - ", .sits_mem_used(), " GB"))

    }
    # finish writing
    for (i in 1:length(bricks_probs))
        bricks_probs[[i]] <- raster::writeStop(bricks_probs[[i]])


    return(cube_class)
}
