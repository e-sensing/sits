#' @title Classify a set of spatio-temporal raster bricks using multicore machines
#' @name sits_classify_raster
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Takes a set of spatio-temporal raster bricks, whose metadata is
#'              described by tibble (created by \code{\link[sits]{sits_coverage}}),
#'              a set of samples used for training a classification model,
#'              a prediction model (created by \code{\link[sits]{sits_train}}),
#'              and produces a classified set of RasterLayers. This function is similar to
#'               \code{\link[sits]{sits_classify}} which is applied to time series stored in a sits tibble.
#'               There are two parameters for optimizing processing of large data sets. These
#'               parameters are "memsize" and "multicores". The "multicores" parameter defines the
#'               number of cores used for processing. The "memsize" parameter  controls
#'               the amount of memory available for classification.
#'
#' @param  file            File name prefix to store the output. For each time interval, one file will be created.
#' @param  coverage        Tibble with information about a coverage of space-time raster bricks.
#' @param  ml_model        An R model trained by \code{\link[sits]{sits_train}}.
#' @param  interval        Interval between two sucessive classifications, expressed in months.
#' @param  filter          Smoothing filter to be applied (if desired).
#' @param  memsize         Memory available for classification (in GB).
#' @param  multicores      Number of cores to be used for classification.
#' @return A tibble with the metadata for the vector of classified RasterLayers.
#'
#' @examples
#' \donttest{
#' # Retrieve the set of samples for the Mato Grosso region (provided by EMBRAPA)
#' data(samples_mt_ndvi)
#'
#' # Build a machine learning model based on the samples
#' svm_model <- sits_train(samples_mt_ndvi, sits_svm())
#'
#' # read a raster file and put it into a vector
#' file <- system.file("extdata/raster/mod13q1/sinop-crop-ndvi.tif", package = "sits")
#'
#' # define the timeline
#' data(timeline_modis_392)
#'
#' # create a raster coverage file based on the information about the files
#' raster.tb <- sits_coverage(service = "RASTER", name  = "Sinop-crop",
#'   timeline = timeline_modis_392, bands = "ndvi", files = file)
#'
#' # classify the raster file
#' raster_class.tb <- sits_classify_raster("raster-class", raster.tb,
#'   ml_model = svm_model, memsize = 4, multicores = 1)
#' # plot the resulting classification
#' sits_plot_raster(raster_class.tb[1,], title = "SINOP class 2000-2001")
#' }
#' @export
sits_classify_raster <- function(file        = NULL,
                                 coverage    = NULL,
                                 ml_model    = NULL,
                                 interval    = "12 month",
                                 filter      = NULL,
                                 memsize     = 4,
                                 multicores  = NULL) {

    if (coverage$service[[1]] == "EOCUBES") {

        res <- sits_classify_cubes(file = file, coverage = coverage,
                                   ml_model = ml_model, interval = interval,
                                   filter = filter, memsize = memsize, multicores = multicores)
        return(res)
    }

    # checks the classification params
    .sits_check_classify_params(file, coverage, ml_model)

    # find the number of cores
    if (purrr::is_null(multicores))
        multicores <- max(parallel::detectCores(logical = FALSE) - 1, 1)

    # retrieve the samples from the model
    samples  <- environment(ml_model)$data.tb

    # create the raster objects and their respective filenames
    coverage_class <- .sits_coverage_raster_classified(coverage, samples, file, interval)

    # classify the data
    raster_class.tb <- .sits_classify_multicores(coverage,
                                                 coverage_class,
                                                 samples,
                                                 ml_model,
                                                 interval,
                                                 filter,
                                                 memsize,
                                                 multicores)

    return(raster_class.tb)
}

#' @title Classify a set of spatio-temporal raster cubes using multicore machines
#' @name sits_classify_cubes
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Takes a set of spatio-temporal raster bricks, whose metadata is
#'              described by tibble (created by \code{\link[sits]{sits_coverage}}),
#'              a set of samples used for training a classification model,
#'              a prediction model (created by \code{\link[sits]{sits_train}}),
#'              and produces a classified set of RasterLayers. This function is similar to
#'               \code{\link[sits]{sits_classify}} which is applied to time series stored in a sits tibble.
#'               There are two parameters for optimizing processing of large data sets. These
#'               parameters are "memsize" and "multicores". The "multicores" parameter defines the
#'               number of cores used for processing. The "memsize" parameter  controls
#'               the amount of memory available for classification.
#'
#' @param  file            File name prefix to store the output. For each time interval, one file will be created.
#' @param  coverage        Tibble with information about a coverage of space-time raster bricks.
#' @param  ml_model        An R model trained by \code{\link[sits]{sits_train}}.
#' @param  interval        Interval between two sucessive classifications, expressed in months.
#' @param  filter          Smoothing filter to be applied (if desired).
#' @param  memsize         Memory available for classification (in GB).
#' @param  multicores      Number of cores to be used for classification.
#' @return A tibble with the metadata for the vector of classified RasterLayers.
#'
#' @examples
#' \donttest{
#' # Retrieve the set of samples for the Mato Grosso region (provided by EMBRAPA)
#' data(samples_mt_ndvi)
#'
#' # Build a machine learning model based on the samples
#' svm_model <- sits_train(samples_mt_ndvi, sits_svm())
#'
#' # create a raster coverage file based on the information about the files
#' raster.tb <- sits_coverage(service = "EOCUBES", name  = "MOD13Q1/006")
#'
#' # classify the raster file
#' raster_class.tb <- sits_classify_cubes("raster-class", raster.tb,
#'   ml_model = svm_model, memsize = 4, multicores = 1)
#'
#' # plot the resulting classification
#' sits_plot_raster(raster_class.tb[1,], title = "Test class 2000-2001")
#' }
#' @export
sits_classify_cubes <- function(file        = NULL,
                                coverage    = NULL,
                                ml_model    = NULL,
                                interval    = "12 month",
                                filter      = NULL,
                                memsize     = 4,
                                multicores  = NULL) {

    if (coverage$service[[1]] == "RASTER" || coverage$service[[1]] == "STACK") {

        res <- sits_classify_raster(file = file, coverage = coverage,
                                    ml_model = ml_model, interval = interval,
                                    filter = filter, memsize = memsize, multicores = multicores)
        return(res)
    }


    # get cube object
    cub.obj <- coverage$r_objs[[1]][[1]]

    # get bands names
    bands <- coverage$bands[[1]]

    # get bands info
    bands_info <- EOCubes::cube_bands_info(cube = cub.obj)

    # retrieve the samples from the model
    samples  <- environment(ml_model)$data.tb

    # what is the reference start date?
    ref_start_date <- lubridate::as_date(samples$time_series[[1]]$Index[1])
    ts_length <- length(samples$time_series[[1]]$Index)

    # get stacks from EOCubes
    stk.obj <- EOCubes::stacks(cube = cub.obj, bands = bands,
                               start_reference = ref_start_date, stack_length = ts_length,
                               starts_interval = interval)

    # find the number of cores
    if (purrr::is_null(multicores))
        multicores <- max(parallel::detectCores(logical = FALSE) - 1, 1)

    raster_class.tb <-
        dplyr::bind_rows(lapply(seq_along(stk.obj), function(i) {

            dplyr::bind_rows(lapply(seq_along(stk.obj[[i]]), function(j) {

                # tile/interval
                tile_interv <- stk.obj[[i]][[j]]

                # file sufix
                file_sufx <- names(stk.obj)[[i]]

                coverage <- sits_coverage(service = "STACK", name = file_sufx,
                                          timeline = tile_interv$timeline, bands = bands,
                                          missing_values = bands_info$fill[bands],
                                          scale_factors = bands_info$scale[bands],
                                          minimum_values = bands_info$min[bands],
                                          maximum_values = bands_info$max[bands],
                                          files = tile_interv$bands)

                # checks the classification params
                .sits_check_classify_params(file, coverage, ml_model)

                # create the raster objects and their respective filenames
                coverage_class <- .sits_coverage_raster_classified(coverage, samples, paste0(file, "_", file_sufx), interval)

                # classify the data
                raster_class.tb <- .sits_classify_multicores_cubes(coverage,
                                                                   coverage_class,
                                                                   samples,
                                                                   ml_model,
                                                                   interval,
                                                                   filter,
                                                                   memsize,
                                                                   multicores)
            }))
        }))

    return(raster_class.tb)
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
#' @param  coverage        Tibble with metadata for a RasterBrick.
#' @param  coverage_class  Taster layer objects to be written.
#' @param  samples         Tibble with samples used for training the classification model.
#' @param  ml_model        A model trained by \code{\link[sits]{sits_train}}.
#' @param  interval        Classification interval.
#' @param  filter          Smoothing filter to be applied to the data.
#' @param  memsize         Memory available for classification (in GB).
#' @param  multicores      Number of cores.
#' @return List of the classified raster layers.
.sits_classify_multicores <-  function(coverage,
                                       coverage_class,
                                       samples,
                                       ml_model,
                                       interval,
                                       filter,
                                       memsize,
                                       multicores) {
    # retrieve the output raster layers
    layers_class.lst <- coverage_class[1,]$r_objs[[1]]
    bricks_probs.lst <- coverage_class[2,]$r_objs[[1]]

    #initiate writing
    for (i in 1:length(layers_class.lst))
        layers_class.lst[[i]] <- raster::writeStart(layers_class.lst[[i]], layers_class.lst[[i]]@file@name, overwrite = TRUE)
    for (i in 1:length(bricks_probs.lst))
        bricks_probs.lst[[i]] <- raster::writeStart(bricks_probs.lst[[i]], bricks_probs.lst[[i]]@file@name, overwrite = TRUE)

    # create a joint list of layers (values) and bricks (probs)
    output.lst <- list(layers = layers_class.lst, bricks = bricks_probs.lst)

    # retrieve the normalization stats
    stats     <- environment(ml_model)$stats.tb

    # divide the input data in blocks
    bs <- .sits_raster_blocks(coverage, ml_model, interval, memsize, multicores)

    # build a list with columns of data table to be processed for each interval
    select.lst <- .sits_select_raster_indexes(coverage, samples, interval)

    # get the labels of the data
    labels <- sits_labels(samples)$label

    # create a named vector with integers to match the class labels
    int_labels <- c(1:length(labels))
    names(int_labels) <- labels

    # get the attribute names
    attr_names <- names(environment(ml_model)$train_data_DT)

    # get initial time for classification
    start_time <- lubridate::now()
    message(sprintf("Starting classification at %s", start_time))

    # read the blocks
    for (block in 1:bs$n) {
        # read the data
        data_DT <- .sits_read_data(coverage, samples, ml_model, bs$row[block], bs$nrows[block], stats, filter, multicores)
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
            output.lst <- .sits_predict_interval(dist_DT, time, output.lst, ml_model, labels, int_labels, bs$row[block], multicores)

            .sits_log_debug(paste0("Memory used after processing block ", block, " of year ", time, " - ", .sits_mem_used(), " GB"))
            # estimate processing time
            .sits_estimate_processing_time(start_time, select.lst, bs, block, time)
        }

        # save information about memory use for debugging later
        .sits_log_debug(paste0("Processed block starting from ",  bs$row[block], " to ", (bs$row[block] + bs$nrows[block] - 1)))
        .sits_log_debug(paste0("Memory used after processing block ", block,  " - ", .sits_mem_used(), " GB"))

    }
    # finish writing
    layers_class.lst <- output.lst$layers
    bricks_probs.lst <- output.lst$bricks

    for (i in 1:length(layers_class.lst))
        layers_class.lst[[i]] <- raster::writeStop(layers_class.lst[[i]])
    for (i in 1:length(bricks_probs.lst))
        bricks_probs.lst[[i]] <- raster::writeStop(bricks_probs.lst[[i]])

    # update the raster objects
    coverage_class[1,]$r_objs <- list(layers_class.lst)
    coverage_class[2,]$r_objs <- list(bricks_probs.lst)

    return(coverage_class)
}

#' @title Classify a stacks chunk using multicores
#' @name .sits_classify_multicores_cubes
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
#' @param  coverage        Tibble with metadata for a RasterBrick.
#' @param  coverage_class  Taster layer objects to be written.
#' @param  samples         Tibble with samples used for training the classification model.
#' @param  ml_model        A model trained by \code{\link[sits]{sits_train}}.
#' @param  interval        Classification interval.
#' @param  filter          Smoothing filter to be applied to the data.
#' @param  memsize         Memory available for classification (in GB).
#' @param  multicores      Number of cores.
#' @return List of the classified raster layers.
.sits_classify_multicores_cubes <-  function(coverage,
                                             coverage_class,
                                             samples,
                                             ml_model,
                                             interval,
                                             filter,
                                             memsize,
                                             multicores) {
    # retrieve the output raster layers
    layers_class.lst <- coverage_class[1,]$r_objs[[1]]
    bricks_probs.lst <- coverage_class[2,]$r_objs[[1]]

    #initiate writing
    for (i in 1:length(layers_class.lst))
        layers_class.lst[[i]] <- raster::writeStart(layers_class.lst[[i]], layers_class.lst[[i]]@file@name, overwrite = TRUE)
    for (i in 1:length(bricks_probs.lst))
        bricks_probs.lst[[i]] <- raster::writeStart(bricks_probs.lst[[i]], bricks_probs.lst[[i]]@file@name, overwrite = TRUE)

    # create a joint list of layers (values) and bricks (probs)
    output.lst <- list(layers = layers_class.lst, bricks = bricks_probs.lst)

    # retrieve the normalization stats
    stats     <- environment(ml_model)$stats.tb

    # divide the input data in blocks
    bs <- .sits_raster_blocks(coverage, ml_model, interval, memsize, multicores)

    # build a list with columns of data table to be processed for each interval
    select.lst <- .sits_select_raster_indexes(coverage, samples, interval)

    # get the labels of the data
    labels <- sits_labels(samples)$label

    # create a named vector with integers to match the class labels
    int_labels <- c(1:length(labels))
    names(int_labels) <- labels

    # get the attribute names
    attr_names <- names(environment(ml_model)$train_data_DT)

    # get initial time for classification
    start_time <- lubridate::now()
    message(sprintf("Starting classification at %s", start_time))

    # read the blocks
    for (block in 1:bs$n) {
        # read the data
        data_DT <- .sits_read_data_cubes(coverage, samples, ml_model, bs$row[block], bs$nrows[block], stats, filter, multicores)
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
            output.lst <- .sits_predict_interval(dist_DT, time, output.lst, ml_model, labels, int_labels, bs$row[block], multicores)

            .sits_log_debug(paste0("Memory used after processing block ", block, " of year ", time, " - ", .sits_mem_used(), " GB"))
            # estimate processing time
            .sits_estimate_processing_time(start_time, select.lst, bs, block, time)
        }

        # save information about memory use for debugging later
        .sits_log_debug(paste0("Processed block starting from ",  bs$row[block], " to ", (bs$row[block] + bs$nrows[block] - 1)))
        .sits_log_debug(paste0("Memory used after processing block ", block,  " - ", .sits_mem_used(), " GB"))

    }
    # finish writing
    layers_class.lst <- output.lst$layers
    bricks_probs.lst <- output.lst$bricks

    for (i in 1:length(layers_class.lst))
        layers_class.lst[[i]] <- raster::writeStop(layers_class.lst[[i]])
    for (i in 1:length(bricks_probs.lst))
        bricks_probs.lst[[i]] <- raster::writeStop(bricks_probs.lst[[i]])

    # update the raster objects
    coverage_class[1,]$r_objs <- list(layers_class.lst)
    coverage_class[2,]$r_objs <- list(bricks_probs.lst)

    return(coverage_class)
}

#' @title Classify one interval of data
#' @name  .sits_predict_interval
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param  DT                A data.table with distance values.
#' @param  time              Time interval to be processed.
#' @param  output.lst        List with the raster objects for classification (values and probs).
#' @param  ml_model          Machine learning model to be applied.
#' @param  labels            Class labels.
#' @param  int_labels        Integer values corresponding to labels.
#' @param  first_row         Initial row of the output layer to write block.
#' @param  multicores        Number of cores to process the time series.
#' @return List of layers with classification results.
.sits_predict_interval <- function(DT, time, output.lst, ml_model, labels, int_labels, first_row, multicores) {
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
                         err_desc = "sits_classify_raster - number of rows of probability matrix is different
                         from number of input pixels")

    # write the raster values
    output.lst <- .sits_write_raster_values(output.lst, prediction_DT,
                                            labels, int_labels,
                                            time, first_row, multicores)

    # memory management
    rm(prediction_DT)
    gc()

    return(output.lst)
}
