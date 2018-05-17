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
#'
#'
#' # classify the raster file
#' raster_class.tb <- sits_classify_raster (file = "./raster-class", raster.tb,
#'    ml_model = svm_model, memsize = 4, multicores = 2)
#' # plot the resulting classification
#' sits_plot_raster(raster_class.tb[1,], title = "SINOP class 2000-2001")
#' }
#'
#' @export
sits_classify_raster <- function(file       = NULL,
                                 coverage   = NULL,
                                 ml_model   = NULL,
                                 interval   = "12 month",
                                 filter     = NULL,
                                 memsize    = 4,
                                 multicores = NULL) {


    # checks the classification params
    .sits_check_classify_params(file, coverage, ml_model)

    # find the number of cores
    if (purrr::is_null(multicores))
        multicores <- parallel::detectCores(logical = FALSE)

    # retrieve the samples from the model
    samples  <- environment(ml_model)$data.tb

    # create the raster objects and their respective filenames
    coverage_class <- .sits_create_classified_raster(coverage, samples, file, interval)

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

#' @title Check clasification parameters
#' @name .sits_check_classify_params
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description Verify that required parameters are correct
#'
#' @param  file            vector of file names to store the output (one file per classified year)
#' @param  coverage        tibble with information about a set of space-time raster bricks
#' @param  ml_model        an R model trained by \code{\link[sits]{sits_train}}
#' @return OK              (logical) tests succeeded?
#'
.sits_check_classify_params <- function(file, coverage, ml_model){

    # ensure metadata tibble exists
    ensurer::ensure_that(coverage, NROW(.) > 0,
                         err_desc = "sits_classify_raster: need a valid metadata for coverage")

    # ensure that file name is provided
    ensurer::ensure_that(file, !purrr::is_null(.),
                         err_desc = "sits-classify-raster: please provide name of output file")

    # ensure the machine learning model has been built
    ensurer::ensure_that(ml_model,  !purrr::is_null(.), err_desc = "sits-classify: please provide a machine learning model already trained")

    return(invisible(TRUE))
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
    layers.lst <- unlist(coverage_class$r_objs)

    #initiate writing
    for (i in 1:length(layers.lst)) {
        layers.lst[[i]] <- raster::writeStart(layers.lst[[i]], layers.lst[[i]]@file@name, overwrite = TRUE)
    }

    # has normalization has been appplied to the data?
    normalize <- .sits_normalization_choice(ml_model)
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

    # get initial time
    start_time <- lubridate::now()
    message(sprintf("Starting classification at %s", start_time))

    # read the blocks
    for (i in 1:bs$n) {
        # read the data
        data_DT <- .sits_read_data(coverage, samples, ml_model, bs$row[i], bs$nrows[i], normalize, stats, filter, multicores)
        # process one temporal instance at a time

        for (t in 1:length(select.lst)) {
            # retrieve the values used for classification
            dist_DT <- data_DT[, select.lst[[t]], with = FALSE]

            # set column names for DT
            colnames(dist_DT) <- attr_names

            # predict the classification values
            layers.lst <- .sits_predict_interval(dist_DT, t, layers.lst, ml_model, int_labels, bs$row[i], multicores)

            # garbage collection
            rm(dist_DT)
            gc()
            .sits_log_debug(paste0("Memory used after processing block ", i, " of year ", t, " - ", .sits_mem_used(), " GB"))

            # compute current time
            current_time <- lubridate::now()

            # compute elapsed time and estimates remaining time
            if (((i - 1) * length(select.lst) + t) < (bs$n * length(select.lst))){
                message(sprintf("Elapsed time %s minute(s). Estimated remaining process time %s minute(s)...",
                                round(as.numeric((lubridate::time_length(current_time - start_time, unit = "minute"))), 1),
                                round(as.numeric((lubridate::time_length(current_time - start_time, unit = "minute")) / ((i - 1) *
                                                                    length(select.lst) + t)) * (bs$n * length(select.lst)), 1)))
            } else {
                message(sprintf("Classification finished at %s. Total elapsed time: %s minute(s).",
                                current_time,
                                round(as.numeric((lubridate::time_length(current_time - start_time, unit = "minute"))), 1)))
            }
        }
        # remove distance data.table (trying to use as little memory as possible)
        rm(data_DT)
        gc()

        # save information about memory use for debugging later
        .sits_log_debug(paste0("Processed block starting from ",  bs$row[i], " to ", (bs$row[i] + bs$nrows[i] - 1)))
        .sits_log_debug(paste0("Memory used after processing block ", i,  " - ", .sits_mem_used(), " GB"))

    }

    # finish writing
    for (i in 1:length(layers.lst)) {
        layers.lst[[i]] <- raster::writeStop(layers.lst[[i]])
    }

    # update the raster objects
    coverage_class$r_objs <- layers.lst

    return(coverage_class)

}
#' @title Define a reasonable block size to process a RasterBrick
#' @name .sits_raster_blocks
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Defines the size of the block of a Raster Brick to be read into memory.
#' The total pixels of a RasterBrick is given by combining the size of the timeline
#' with the number of rows and columns of the Brick. For example, a Raster Brick
#' with 500 rows and 500 columns and 400 time instances will have a total pixel size
#' of 800 Mb if pixels are 64-bit. I
#'
#' @param  coverage        input raster coverage
#' @param  ml_model        machine learning model
#' @param  interval        classification interval
#' @param  memsize         memory available for classification (in GB)
#' @param  multicores      number of threads to process the time series.
#' @return bs              list with three attributes: n (number of blocks), rows (list of rows to begin),
#'                    nrows - number of rows to read at each iteration
#'
.sits_raster_blocks <- function(coverage, ml_model, interval, memsize, multicores){

    # number of bands
    bands  <- coverage[1,]$bands[[1]]
    nbands <-  length(bands)
    # number of rows and cols
    nrows <- coverage[1,]$nrows
    ncols <- coverage[1,]$ncols
    # recover the timeline
    timeline <- coverage[1,]$timeline[[1]]
    # total number of instances
    ninstances <- length(timeline)
    # number of instances per classification interval
    interval_dates <- lubridate::as.duration(lubridate::as_date(timeline) - lubridate::as_date(timeline[1])) > lubridate::as.duration(interval)
    if (any(interval_dates))
        ninterval <- which(interval_dates)[1] - 1
    else
        ninterval <- ninstances
    # number of bytes por pixel
    nbytes <-  8
    # estimated memory bloat
    bloat <- sits.env$config$R_memory_bloat

    # estimated full size of the data
    full_data_size <- as.numeric(ninstances)*as.numeric(nrows)*as.numeric(ncols)*as.numeric(nbytes)*as.numeric(nbands)

    # estimated size of memory required for scaling and normalization
    mem_required_scaling <- (full_data_size + as.numeric(pryr::mem_used()))*bloat

    # estimated size of the data for classification
    class_data_size <- as.numeric(ninterval)*as.numeric(nrows)*as.numeric(ncols)*as.numeric(nbytes)*as.numeric(nbands)

    # memory required for processing
    if ( !(purrr::is_null(environment(ml_model)$model.keras)) || !(purrr::is_null(environment(ml_model)$attr_names_X))
         || !(purrr::is_null(environment(ml_model)$result_ranger)))  {
        multicores <- 1
        .sits_log_debug(paste0("keras, ranger and liquidSVM already run on multiple CPUs - setting multicores to 1"))
    }
    mem_required_processing <- multicores*(as.numeric(pryr::mem_used()) + class_data_size) + full_data_size*bloat

    # number of passes to read the full data sets
    nblocks <- max(ceiling(mem_required_scaling/(memsize*1e+09)), ceiling(mem_required_processing/(memsize*1e+09)))

    # number of rows per block
    block_rows <- ceiling(nrows/nblocks)

    # initial row of each block
    row.vec <- seq.int(from = 1, to = nrows, by = block_rows)
    # number of rows in each block
    nrows.vec <- rep.int(block_rows, length(row.vec))
    # check that total number of rows is the same as the sum of all blocks
    # correct the last block for overflow
    if (sum(nrows.vec) != nrows )
        nrows.vec[length(nrows.vec)] <- nrows - sum(nrows.vec[1:(length(nrows.vec) - 1)])

    # find out the size of the block in pixels
    size.vec <- nrows.vec * ncols

    # elements of the block list
    # n          number of blocks
    # row        starting row from the RasterBrick
    # nrow       Number of rows in the block extracted from the RasterBrick
    # size       size of each block in pixels

    bs <- list(n = nblocks, row = row.vec, nrows = nrows.vec, size = size.vec)

    return(bs)

}
#' @title Read a block of values retrived from a set of raster bricks
#' @name  .sits_read_data
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param  coverage        input raster coverage
#' @param  samples         tibble with samples
#' @param  ml_model        machine learning model
#' @param  first_row       first row to start reading
#' @param  n_rows_block    number of rows in the block
#' @param  normalize       (boolean) has normalization been applied?
#' @param  stats           normalization parameters
#' @param  filter          smoothing filter to be applied
#' @param  multicores      number of cores to process the time series
#' @return dist_DT         data.table with values for classification
#'
.sits_read_data <- function(coverage, samples, ml_model, first_row, n_rows_block, normalize, stats, filter, multicores) {

    # get the bands in the same order as the samples
    bands <- sits_bands(samples)

    # get the missing values, minimum values and scale factors
    missing_values <- unlist(coverage$missing_values)
    minimum_values <- unlist(coverage$minimum_values)
    scale_factors  <- unlist(coverage$scale_factors)

    # get the raster bricks to be read
    bricks.lst <- coverage$files[[1]]

    ordered_bricks.lst <- vector(mode = "list", length = length(bands))

    for (i in 1:length(bands))
        ordered_bricks.lst[[i]] <- bricks.lst[[bands[i]]]

    names(ordered_bricks.lst) <- bands

    # index to go through the bands vector
    b <- 0

    # set the offset and region to be read by GDAL
    offset     <- c(first_row - 1, 0)
    region.dim <- c(n_rows_block, coverage[1,]$ncols)

    # read the values from the raster bricks ordered by bands
    values.lst <- ordered_bricks.lst %>%
        purrr::map(function(r_brick) {
            # the readGDAL function returns a matrix
            # the rows of the matrix are the pixels
            # the cols of the matrix are the layers
            values.mx    <- as.matrix(suppressWarnings(rgdal::readGDAL(r_brick, offset, region.dim, silent = TRUE))@data)

            # proprocess the input data
            b <<- b + 1
            band <- bands[b]
            values.mx <- .sits_preprocess_data(values.mx, band, missing_values[band], minimum_values[band], scale_factors[band],
                                               normalize, stats, filter, multicores)

            # save information about memory use for debugging later
            .sits_log_debug(paste0("Memory used after readGDAL - ", .sits_mem_used(), " GB"))
            .sits_log_debug(paste0("Read band ", b, " from rows ", first_row, "to ", (first_row + n_rows_block - 1)))

            return(values.mx)
        })
    # create a data.table joining the values
    data_DT <- data.table::as.data.table(do.call(cbind,values.lst))

    # memory cleanup
    .sits_log_debug(paste0("Memory used after binding bricks  - ", .sits_mem_used(), " GB"))
    rm(values.lst)
    gc()
    .sits_log_debug(paste0("Memory used after removing values - ", .sits_mem_used(), " GB"))


    # create two additional columns for prediction
    size <- n_rows_block*coverage[1,]$ncols
    two_cols_DT <- data.table::data.table("original_row" = rep(1,size),
                                          "reference"    = rep("NoClass", size))

    # join the two columns with the data values
    data_DT <- data.table::as.data.table(cbind(two_cols_DT, data_DT))

    # memory debug
    .sits_log_debug(paste0("Memory used after adding two first cols - ", .sits_mem_used(), " GB"))

    return(data_DT)
}

#' @title Preprocess a set of values retrived from a raster brick
#' @name  .sits_preprocess_data
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param  values.mx        matrix of values retrieved from a brick
#' @param  band             band to be processed
#' @param  missing_value    missing value for the band
#' @param  minimum_value    minimum values for the band
#' @param  scale_factor     scale factor for each band (only for raster data)
#' @param  normalize        (integer) 0 = no normalization, 1 = normalize per band, 2 = normalize per dimension
#' @param  stats            normalization parameters
#' @param  filter           smoothing filter to be applied
#' @param  multicores       number of cores to process the time series
#' @return values.mx        matrix with pre-processed values
.sits_preprocess_data <- function(values.mx, band, missing_value, minimum_value, scale_factor,
                                  normalize, stats, filter, multicores){

    # correct minimum value
    values.mx[is.na(values.mx)] <- minimum_value
    values.mx[values.mx <= minimum_value] <- minimum_value

    # memory management
    .sits_log_debug(paste0("Memory used before scaling - ", .sits_mem_used(), " GB"))

    # scale the data set
    # auxiliary function to scale a block of data
    scale_block <- function(chunk, scale_factor) {
        scaled_block.mx <- scale_data(chunk, scale_factor)
    }
    # use multicores to speed up scaling
    if (multicores > 1) {
        chunk.lst <- .sits_split_data(values.mx, multicores)
        rows.lst  <- parallel::mclapply(chunk.lst, scale_block, scale_factor, mc.cores = multicores)
        values.mx <- do.call(rbind, rows.lst)
        rm(chunk.lst)
        rm(rows.lst)
        gc()
    }
    else
        values.mx <- scale_data(values.mx, scale_factor)

    .sits_log_debug(paste0("Memory used after scaling ", .sits_mem_used(), " GB"))

    # normalize the data by bands
    if (normalize && !purrr::is_null(stats)) {
        values.mx <- .sits_normalize_matrix(values.mx, stats, band, multicores)
        .sits_log_debug(paste0("Model has been normalized"))
    }

    if (!(purrr::is_null(filter))) {
        values.mx <- filter(values.mx)
    }
    return(values.mx)
}
#' @title Classify one interval of data
#' @name  .sits_predict_interval
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param  DT                data.table with distance values
#' @param  time              time interval to be processed
#' @param  layers.lst        list of layers with classification results
#' @param  ml_model          machine learning model to be applied
#' @param  int_labels        integer values corresponding to labels
#' @param  first_row         initial row of the output layer to write block
#' @param  multicores        number of cores to process the time series
#' @return layers.lst        list of layers with classification results

.sits_predict_interval <- function(DT, time, layers.lst, ml_model, int_labels, first_row, multicores) {


    if (!(purrr::is_null(environment(ml_model)$model.keras))) {
        multicores <- 1
        .sits_log_debug(paste0("keras already runs on multiple CPUs - setting multicores to 1"))
    }
    if (!(purrr::is_null(environment(ml_model)$attr_names_X))) {
        multicores <- 1
        .sits_log_debug(paste0("liquidSVM already runs on multiple CPUs - setting multicores to 1"))
    }
    if (!(purrr::is_null(environment(ml_model)$result_ranger))) {
        multicores <- 1
        .sits_log_debug(paste0("ranger already runs on multiple CPUs - setting multicores to 1"))
    }
    # classify a block of data
    classify_block <- function(block) {
        # predict the values for each time interval
        pred_block.vec <- ml_model(block)
        return(pred_block.vec)
    }
    # set up multicore processing
    if (multicores > 1) {
        # memory management
        .sits_log_debug(paste0("Memory used before split_data - ", .sits_mem_used(), " GB"))

        # estimate the list for breaking a block
        block.lst <- .sits_split_data(DT, multicores)

        .sits_log_debug(paste0("Memory used after split_data - ", .sits_mem_used(), " GB"))

        # apply parallel processing to the split data and join the results
        pred.vec <- unlist(parallel::mclapply(block.lst, classify_block,  mc.cores = multicores))

        # memory management
        .sits_log_debug(paste0("Memory used after mclapply - ", .sits_mem_used(), " GB"))
        rm(block.lst)
        gc()
        .sits_log_debug(paste0("Memory used after removing blocks - ", .sits_mem_used(), " GB"))
    }
    else # one core only
        # estimate the prediction vector
        pred.vec <- as.character(ml_model(DT))

    # check the result has the right dimension
    ensurer::ensure_that(pred.vec, length(.) == nrow(DT),
                         err_desc = "sits_classify_raster - number of classified pixels is different
                         from number of input pixels")

    # memory management
    rm(DT)
    gc()
    .sits_log_debug(paste0("Memory used after removing DT - ", .sits_mem_used(), " GB"))

    # for each layer, write the predicted values

    layers.lst[[time]] <- raster::writeValues(layers.lst[[time]], as.integer(int_labels[pred.vec]), first_row)

    # memory management
    rm(pred.vec)
    gc()

    return(layers.lst)
}
#' @title Normalize the time series values in the case of a matrix
#' @name .sits_normalize_matrix
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description this function normalizes one band of the values read from a raster brick
#'
#' @param  data.mx        matrix of values
#' @param  stats.tb       statistics for normalization
#' @param  band           band to be normalized
#' @param  multicores     number of cores
#' @return data.mx        a normalized matrix
#'
.sits_normalize_matrix <- function(data.mx, stats.tb, band, multicores) {
    # select the 2% and 98% quantiles
    quant_2   <- as.numeric(stats.tb[2, band])
    quant_98  <- as.numeric(stats.tb[3, band])

    # auxiliary function to normalize a block of data
    normalize_block <- function(chunk, quant_2, quant_98) {
        # normalize a block of data
        values_block.mx <- normalize_data(chunk, quant_2, quant_98)
    }

    # memory management
    .sits_log_debug(paste0("Memory used before normalization - ", .sits_mem_used(), " GB"))

    # parallel processing for normalization
    if (multicores > 1) {
        chunk.lst <- .sits_split_data(data.mx, multicores)
        rows.lst  <- parallel::mclapply(chunk.lst, normalize_block, quant_2, quant_98, mc.cores = multicores)
        data.mx <- do.call(rbind, rows.lst)
        rm(chunk.lst)
        rm(rows.lst)
        gc()
    }
    else
        data.mx <- normalize_data(data.mx, quant_2, quant_98)

    .sits_log_debug(paste0("Data has been normalized between ", quant_2 , " (2%) and ", quant_98, "(98%)"))
    .sits_log_debug(paste0("Memory used after normalization - ", .sits_mem_used(), " GB"))

    return(data.mx)
}
