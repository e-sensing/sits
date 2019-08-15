#' @title Classify a data cube using multicore machines
#' @name .sits_classify_cube
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Takes a data cube, whose metadata is
#'              described by tibble (created by \code{\link[sits]{sits_cube}}),
#'              a set of samples used for training a classification model,
#'              a prediction model (created by \code{\link[sits]{sits_train}}),
#'              and produces a classified set of RasterLayers. These
#'               parameters are "memsize" and "multicores". The "multicores" parameter defines the
#'               number of cores used for processing. The "memsize" parameter  controls
#'               the amount of memory available for classification.
#'
#' @param  cube            Tibble with information about a data cube.
#' @param  ml_model        An R model trained by \code{\link[sits]{sits_train}}.
#' @param  interval        Interval between two sucessive classifications, expressed in months.
#' @param  filter          Smoothing filter to be applied (if desired).
#' @param  memsize         Memory available for classification (in GB).
#' @param  multicores      Number of cores to be used for classification.
#' @param  output_dir      Directory for output file
#' @return A tibble with the metadata for the vector of probabilities for classified RasterLayers.
#'
.sits_classify_cube <- function(cube , ml_model, interval, filter,
                                 memsize, multicores, output_dir) {

    if (.sits_cube_service(cube) == "EOCUBES") {
        res <- .sits_classify_eocubes(cube, ml_model, interval, filter, memsize, multicores, output_dir)
        return(res)
    }

    # checks the classification params
    .sits_check_classify_params(cube, ml_model)

    # CRAN limits the number of cores to 2
    chk <- Sys.getenv("_R_CHECK_LIMIT_CORES_", "")
    # if running on check mode, multicores must be 2
    if (nzchar(chk) && chk == "TRUE") {
        # use 2 cores in CRAN/Travis/AppVeyor
        multicores <- 2L
    }
    # find the number of cores
    if (purrr::is_null(multicores))
        multicores <- max(parallel::detectCores(logical = FALSE) - 1, 1)

    # retrieve the samples from the model
    samples  <- environment(ml_model)$data.tb
    ensurer::ensure_that(samples, NROW(.) > 0,
                        err_desc = "sits_classify: original samples not saved in the model environment")

    # classify the data
    cube_probs <- .sits_classify_multicores(cube, samples, ml_model,
                                                 interval, filter, memsize, multicores, output_dir)

    return(cube_probs)
}



#' @title Classify a raster chunk using multicores
#' @name .sits_classify_multicores
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
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
#' @param  samples         Tibble with samples used for training the classification model.
#' @param  ml_model        A model trained by \code{\link[sits]{sits_train}}.
#' @param  interval        Classification interval.
#' @param  filter          Smoothing filter to be applied to the data.
#' @param  memsize         Memory available for classification (in GB).
#' @param  multicores      Number of cores.
#' @param  output_dir      Output directory
#' @return List of the classified raster layers.
.sits_classify_multicores <-  function(cube,
                                       samples,
                                       ml_model,
                                       interval,
                                       filter,
                                       memsize,
                                       multicores,
                                       output_dir) {


    # get the reference r_obj from the existing cube
    r_obj <- .sits_cube_robj(cube)

    # create the medata for the classified cube
    cube_class <- .sits_cube_classified(cube, samples, interval, output_dir)
    # find out how many layers per brick
    n_layers  <- length(sits_labels(samples)$label)

    # create the Raster objects
    n_objs <- length(.sits_cube_files(cube_class))
    bricks <- vector("list", n_objs)

    # clone the bricks from existing r_obj
    for (i in 1:n_objs) {
        bricks[[i]] <- raster::brick(r_obj, nl = n_layers)
        raster::dataType(bricks[[i]]) <- "INT2U"
        bricks[[i]]@file@name <- .sits_cube_file(cube_class, i)
    }

    # initiate writing
    for (i in 1:length(bricks))
        bricks[[i]] <- raster::writeStart(bricks[[i]], bricks[[i]]@file@name, overwrite = TRUE)

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
        data_DT <- .sits_read_data(cube, samples, ml_model,
                                   bs$row[block], bs$nrows[block], stats, filter, multicores)
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
            bricks[[time]] <- raster::writeValues(bricks[[time]], probs, bs$row[block])

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
    for (i in 1:length(bricks))
        bricks[[i]] <- raster::writeStop(bricks[[i]])

    return(cube_class)
}


#' @title Classify one interval of data
#' @name  .sits_predict_interval
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param  DT                A data.table with distance values.
#' @param  time              Time interval to be processed.
#' @param  ml_model          Machine learning model to be applied.
#' @param  first_row         Initial row of the output layer to write block.
#' @param  multicores        Number of cores to process the time series.
#' @return                   A data table with predicted values of probs
.sits_predict_interval <- function(DT, time, ml_model, first_row, multicores) {
    nrows_DT <- nrow(DT)
    proc_cores <- multicores
    if (!(purrr::is_null(environment(ml_model)$model.keras)) ||
        !(purrr::is_null(environment(ml_model)$result_ranger)) ) {
        proc_cores <- 1
        .sits_log_debug(paste0("keras and ranger already run on multiple CPUs - setting multicores to 1"))
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
        .sits_log_debug(paste0("Memory used before split data - ", .sits_mem_used(), " GB"))
        block.lst <- .sits_split_data(DT, proc_cores)
        # memory management
        rm(DT)
        gc()

        .sits_log_debug(paste0("Memory used before mcapply - ", .sits_mem_used(), " GB"))
        # apply parallel processing to the split data (return the results in a list inside a prototype)
        predictions.lst <- parallel::mclapply(block.lst, classify_block,  mc.cores = proc_cores)

        #memory management
        rm(block.lst)
        gc()
        .sits_log_debug(paste0("Memory used after mclapply - ", .sits_mem_used(), " GB"))
        # compose result based on output from different cores
        prediction_DT <- data.table::as.data.table(do.call(rbind,predictions.lst))
        # memory management
        rm(predictions.lst)
        gc()
        .sits_log_debug(paste0("Memory used after removing predictions.lst - ", .sits_mem_used(), " GB"))
    }
    else {
        # memory management
        .sits_log_debug(paste0("Memory used before prediction - ", .sits_mem_used(), " GB"))

        # estimate the prediction vector
        prediction_DT <- ml_model(DT)
        # memory management
        rm(DT)
        gc()
    }

    # are the results consistent with the data input?
    ensurer::ensure_that(prediction_DT, nrow(.) == nrows_DT,
                         err_desc = ".sits_classify_cube - number of rows of probability matrix is different
                         from number of input pixels")

    return(prediction_DT)
}
