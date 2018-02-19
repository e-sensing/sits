#' @title Classify a raster chunk using machine learning models
#' @name .sits_classify_bigdata
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Classifies a block of data one interval at a time. Reads data from
#' a file using Rgdal, then cleans the data for NAs and missing values. The clean
#' data is store in a data table that has all the time instances for all pixels of
#' the block. The algorithm then classifies data on an year by year basis.
#' For each year, it extracts the sub-blocks for each band. It then breaks
#' the sub-block horizontally and divides its among the server cores available.
#' After all cores process their sub-blocks, it joins the result and then writes it
#' in the classified image for the corresponding year. Then, it starts again in the
#' next year to be classified.
#'
#' @param  raster.tb       tibble with metadata for a RasterBrick
#' @param  layers.lst      list of the classified raster layers
#' @param  time_index.lst  a list with the indexes to extract data for each time interval
#' @param  bands           vector with the names of the bands
#' @param  attr_names      vector with the attribute names
#' @param  int_labels      conversion vector from the labels to integer values
#' @param  init_row        starting row from the RasterBrick
#' @param  nrows           number of rows in the block extracted from the RasterBrick
#' @param  adj_fun         sdjustment function to be applied to the data
#' @param  ml_model        a model trained by \code{\link[sits]{sits_train}}
#' @param  multicores      number of threads to process the time series.
#' @param  verbose         run function in verbose mode? (useful for working with big data sets)
#' @return layer.lst       list  of the classified raster layers
#'
.sits_classify_bigdata <-  function(raster.tb,
                                    layers.lst,
                                    time_index.lst,
                                    bands,
                                    attr_names,
                                    int_labels,
                                    init_row,
                                    nrows,
                                    adj_fun,
                                    ml_model,
                                    multicores,
                                    verbose) {


    ensurer::ensure_that(ml_model, !purrr::is_null(.),
                         err_desc = "sits-classify: please provide a machine learning model already trained")

    # get the vector of bricks
    bricks.vec <- raster.tb$files[[1]]
    # get the bands, scale factors and missing values
    bands <- unlist(raster.tb$bands)
    missing_values <- unlist(raster.tb$missing_values)
    minimum_values <- .sits_get_minimum_values("RASTER", bands)
    scale_factors  <- unlist(raster.tb$scale_factors)

    # get the adjustment value
    adj_value <- as.double(.sits_get_adjustment_shift())

    # set the offset and region to be read by GDAL
    offset <- c(init_row - 1, 0)
    ncols <- raster.tb$ncols
    region.dim <- c(nrows, ncols)
    # create a pointer to the years
    i <- 0
    if (verbose)
        .sits_log_debug(paste0("Memory used before readGDAL - ", .sits_mem_used(), " GB"))

    # go through all the bricks
    values.lst <- bricks.vec %>%
        purrr::map(function(r_brick) {
            # the readGDAL function returns a matrix
            # the rows of the matrix are the pixels
            # the cols of the matrix are the layers
            values.mx    <- as.matrix(rgdal::readGDAL(r_brick, offset, region.dim, silent = TRUE)@data)

            # get the associated band
            i <<- i + 1
            band <- bands[i]
            missing_value <- missing_values[band]
            minimum_value <- minimum_values[band]
            scale_factor  <- scale_factors[band]

            if (verbose) {
                .sits_log_debug(paste0("Memory used after readGDAL - ", .sits_mem_used(), " GB"))
                message(paste0("Read band ", band, " from rows ", init_row, "to ", (init_row + nrows - 1)))
            }

            values.mx[is.na(values.mx)] <- minimum_value

            values.mx <- preprocess_data(values.mx, missing_value, minimum_value, scale_factor, adj_value)

            return(values.mx)
        })

    dist.tb <- data.table::as.data.table(do.call(cbind, values.lst))

    # memory cleanup
    if (verbose)
        .sits_log_debug(paste0("Memory used after binding bricks  - ", .sits_mem_used(), " GB"))

    rm(values.lst)
    gc()
    if (verbose)
        .sits_log_debug(paste0("Memory used after removing values - ", .sits_mem_used(), " GB"))

    # create a data table for better memory management
    size <- nrows*ncols
    two_cols.tb <- data.table::data.table("original_row" = rep(1,size),
                                          "reference" = rep("NoClass", size))

    dist.tb <- cbind(two_cols.tb, dist.tb)

    # memory debug
    if (verbose)
        .sits_log_debug(paste0("Memory used after adding two first cols - ", .sits_mem_used(), " GB"))

    # iterate through time intervals
    t <- 1
    time_index.lst %>%
        purrr::map(function(idx){
            # for a given time index, build the data.table to be classified
            # build the classification matrix but extracting the relevant columns
            selec <- logical(length = ncol(dist.tb))
            selec[1:2] <- TRUE
            for (b in 1:length(bands)) {
                selec[idx[(2*b - 1)]:idx[2*b]] <- TRUE
            }
            # retrieve the values used for classification
            dist1.tb <- dist.tb[,selec]
            # set the names of the columns of dist1.tb
            colnames(dist1.tb) <- attr_names

            # memory management
            if (verbose)
                .sits_log_debug(paste0("Memory used after selecting data subset  - ", .sits_mem_used(), " GB"))

            # classify a block of data
            classify_block <- function(block.tb) {
                # predict the values for each time interval
                pred_block.vec <- .sits_predict(block.tb, ml_model)
                return(pred_block.vec)
            }

            # set up multicore processing
            if (multicores > 1) {
                # estimate the list for breaking a block
                block_size.lst <- .sits_split_block_size(nrow(dist1.tb), multicores)
                # divide the input matrix into blocks for multicore processing
                blocks.lst <- block_size.lst %>%
                    purrr::map(function(bs){
                        # select a chunk of data for each core
                        block.tb <- dist1.tb[bs[1]:bs[2],]
                        return(block.tb)
                    })
                # memory management
                if (verbose)
                    .sits_log_debug(paste0("Memory used after building chunks  - ", .sits_mem_used(), " GB"))
                rm(dist1.tb)
                gc()
                if (verbose)
                    .sits_log_debug(paste0("Memory used before calling parallel processing - ", .sits_mem_used(), " GB"))

                # apply parallel processing to the split data and join the results
                pred.vec <- unlist(parallel::mclapply(blocks.lst, classify_block, mc.cores = multicores))

                # memory management
                if (verbose)
                    .sits_log_debug(paste0("Memory used after calling parallel processing - ", .sits_mem_used(), " GB"))
                rm(block_size.lst)
                rm(blocks.lst)
                gc()
                if (verbose)
                    .sits_log_debug(paste0("Memory used after removing blocks - ", .sits_mem_used(), " GB"))
            }
            else {
                # estimate the prediction vector
                pred.vec <- classify_block(dist1.tb)

                # memory management
                rm(dist1.tb)
                gc()
                if (verbose)
                    .sits_log_debug(paste0("Memory used after classification in one core - ", .sits_mem_used(), " GB"))
            }

            # check the result has the right dimension
            ensurer::ensure_that(pred.vec, length(.) == nrow(dist.tb),
                                 err_desc = "sits_classify_raster - number of classified pixels is different
                                        from number of input pixels")

            # for each layer, write the predicted values
            values <- as.integer(int_labels[pred.vec])
            layers.lst[[t]]  <- raster::writeValues(layers.lst[[t]], values, init_row)
            t <<- t + 1

            if (verbose)
                message(paste0("Processed year ", t, " starting from row ", init_row))

            # memory management
            rm(pred.vec)
            rm(values)
            gc()
            if (verbose)
                .sits_log_debug(paste0("Memory used after classification of year ", t, " - ", .sits_mem_used(), " GB"))
        })
    # memory management
    rm(dist.tb)
    gc()
    if (verbose) {
        .sits_log_debug(paste0("Memory used after end of processing all years - ", .sits_mem_used(), " GB"))
        message(paste0("Processed block starting from ", init_row, " to ", (init_row + nrows - 1)))
    }


    return(layers.lst)
}
#' @title Find the time index of the blocks to be extracted for classification
#' @name .sits_get_time_index
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Obtains the indexes of the blocks to be extract for each time interval
#' associated with classification
#'
#' @param class_info.tb A tibble with information required for classification
#' @return attr_names   A vector with the names of the columns with the matrix to be classified
#'
.sits_get_time_index <- function(class_info.tb) {
    # find the subsets of the input data
    dates_index.lst <- class_info.tb$dates_index[[1]]

    #retrieve the timeline of the data
    timeline <- class_info.tb$timeline[[1]]

    # retrieve the bands
    bands <- class_info.tb$bands[[1]]

    #retrieve the time index
    time_index.lst  <- .sits_time_index(dates_index.lst, timeline, bands)

    return(time_index.lst)

}
#' @title Obtain the names of the columns of the matrix to be classified
#' @name .sits_get_attr_names
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Obtains the names of the columns of the matrix to be classified
#'
#' @param class_info.tb A tibble with information required for classification
#' @return attr_names   A vector with the names of the columns with the matrix to be classified
#'
.sits_get_attr_names <- function(class_info.tb){

    # get information about the bands
    bands <- class_info.tb$bands[[1]]

    # find the subsets of the input data
    dates_index.lst <- class_info.tb$dates_index[[1]]

    # find the number of the samples
    nsamples <- dates_index.lst[[1]][2] - dates_index.lst[[1]][1] + 1

    # define the column names
    attr_names.lst <- bands %>%
        purrr::map(function(b){
            attr_names_b <- c(paste(c(rep(b, nsamples)), as.character(c(1:nsamples)), sep = ""))
            return(attr_names_b)
        })
    attr_names <- unlist(attr_names.lst)
    attr_names <- c("original_row", "reference", attr_names)

    return(attr_names)
}
#' @title Define the split of the data blocks for multicore processing
#' @name .sits_split_block_size
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description this functions defines the rows of the input data table that will be
#' split to fit to be divided between the different cores
#'
#' @param nrows number of rows in the input data table
#' @param ncores number of cores for processing
#' @return block_size.lst  list of pairs of positions (first row, last row) to be assigned to each core
#'
.sits_split_block_size <- function(nrows, ncores){

    # find the remainder and quotient
    quo <- nrows %/% ncores
    rem <- nrows %% ncores
    # c
    # create a list to store the result
    block_size.lst <- list(length = ncores)
    block_size_end = 0
    for (i in 1:(ncores)) {
        block_size_start <- block_size_end + 1
        block_size_end   <- block_size_start + quo - 1
        if (i == ncores )
            block_size_end <- block_size_end + rem
        block_size.lst[[i]] <- c(block_size_start, block_size_end)
    }
    return(block_size.lst)
}

#' @title Define a reasonable block size to process a RasterBrick
#' @name .sits_raster_block_size
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Defines the size of the block of a Raster Brick to be read into memory.
#' The total pixels of a RasterBrick is given by combining the size of the timeline
#' with the number of rows and columns of the Brick. For example, a Raster Brick
#' with 500 rows and 500 columns and 400 time instances will have a total pixel size
#' of 250 Mb if pixels are 16-bit (about a GigaByte). If there are 4 bands to be processed together, there will be 4 Raster Bricks.
#' Thus, a block size of 250000 will use a 1 GB just to store the image data.
#'
#' As a rule of thumb, consider that for a typical MODIS data set such as MOD13Q1 there will be
#' about 23 time instances per year. In 20 years, this means about 460 instances.
#' In a small desktop with 8 GBytes, a block size of 250000 will use 1Gb of memory.
#' This is taken to be the default for small machines.
#' In a larger server, users should increase the block size for improved processing.
#'
#' @param  brick.tb   Metadata for a RasterBrick
#' @param  blocksize  Default size of the block (rows * cols)
#' @return block      list with three attributes: n (number of blocks), rows (list of rows to begin),
#'                    nrows - number of rows to read at each iteration
#'
.sits_raster_block_size <- function(brick.tb, blocksize = 250000){


    # number of rows per block
    block_rows <- min(ceiling(blocksize/brick.tb$ncols), brick.tb$nrows)
    # number of blocks to be read
    nblocks <- ceiling(brick.tb$nrows/block_rows)

    row <- seq.int(from = 1, to = brick.tb$nrows, by = block_rows)
    nrows <- rep.int(block_rows, length(row))
    if (sum(nrows) != brick.tb$nrows )
        nrows[length(nrows)] <- brick.tb$nrows - sum(nrows[1:(length(nrows) - 1)])

    # find out the size of the block in pixels
    size <- nrows * brick.tb$ncols

    # elements of the block list
    # n          number of blocks
    # row        starting row from the RasterBrick
    # nrow       Number of rows in the block extracted from the RasterBrick

    block <- list(n = nblocks, row = row, nrows = nrows, size = size)

    return(block)

}



