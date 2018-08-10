#' @title Classify a set of spatio-temporal raster bricks using multicore machines
#' @name sits_classify_raster
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Takes a set of spatio-temporal raster bricks, whose metadata is
#'              described by tibble (created by \code{\link[sits]{sits_coverage}}),
#'              a set of samples used for training a classification model,
#'              a prediction model (created by \code{\link[sits]{sits_train}}),
#'              and produces a classified set of RasterLayers. This function is similar to
#'               \code{\link[sits]{sits_classify}} which is applied to time series stored in a SITS tibble.
#'               There are two parameters for optimizing processing of large data sets. These
#'               parameters are "memsize" and "multicores". The "multicores" parameter defines the
#'               number of cores used for processing. The "memsize" parameter  controls
#'               the amount of memory available for classification.
#'
#'
#' @param  file            vector of file names to store the output (one file per classified year)
#' @param  coverage        tibble with information about a coverage of space-time raster bricks
#' @param  ml_model        an R model trained by \code{\link[sits]{sits_train}}
#' @param  interval        interval between two sucessive classifications, expressed in months
#' @param  filter          smoothing filter to be applied (if desired)
#' @param  memsize         memory available for classification (in GB)
#' @param  multicores      number of cores to be used for classification
#' @return raster_class.tb tibble with the metadata for the vector of classified RasterLayers
#'
#' @examples
#' \donttest{
#' # Retrieve the set of samples for the Mato Grosso region (provided by EMBRAPA)
#' data(samples_MT_ndvi)
#'
#' # Build a machine learning model based on the samples
#' svm_model <- sits_train(samples_MT_ndvi, sits_svm())
#'
#' # read a raster file and put it into a vector
#' files  <- c(system.file ("extdata/raster/mod13q1/sinop-crop-ndvi.tif", package = "sits"))
#'
#' # define the timeline
#' data(timeline_modis_392)
#'
#' # create a raster metadata file based on the information about the files
#' #' # create a raster coverage file based on the information about the files
#' raster.tb <- sits_coverage(service = "RASTER", name  = "Sinop-crop",
#'              timeline = timeline_modis_392, bands = c("ndvi"), files = files)
#' # classify the raster file
#' raster_class.tb <- sits_classify_raster (file = "./raster-class", raster.tb,
#'    ml_model = svm_model, memsize = 4, multicores = 2)
#' # plot the resulting classification
#' sits_plot_raster(raster_class.tb[1,], title = "SINOP class 2000-2001")
#' }
#'
#' @export
sits_classify_raster <- function(file        = NULL,
                                 coverage    = NULL,
                                 ml_model    = NULL,
                                 interval    = "12 month",
                                 filter      = NULL,
                                 memsize     = 4,
                                 multicores  = NULL) {


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
#' @param  coverage        tibble with metadata for a RasterBrick
#' @param  coverage_class  raster layer objects to be written
#' @param  samples         tibble with samples used for training the classification model
#' @param  ml_model        a model trained by \code{\link[sits]{sits_train}}
#' @param  interval        classification interval
#' @param  filter          smoothing filter to be applied to the data
#' @param  memsize         memory available for classification (in GB)
#' @param  multicores      number of cores
#' @return layer.lst       list  of the classified raster layers
#'
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

#' @title Classify one interval of data
#' @name  .sits_predict_interval
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param  DT                data.table with distance values
#' @param  time              time interval to be processed
#' @param  output.lst        list with the raster objects for classification (values and probs)
#' @param  ml_model          machine learning model to be applied
#' @param  labels            class labels
#' @param  int_labels        integer values corresponding to labels
#' @param  first_row         initial row of the output layer to write block
#' @param  multicores        number of cores to process the time series
#' @return layers.lst        list of layers with classification results

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







