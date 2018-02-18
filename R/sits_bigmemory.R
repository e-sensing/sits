#' @title Classify a distances matrix extracted from raster using machine learning models
#' @name .sits_classify_big_memory
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Classifies a block of data one interval at a time
#'
#' @param  raster.tb      Metadata for a RasterBrick
#' @param  layers.lst      list of the classified raster layers
#' @param  time_index.lst  a list with the indexes to extract data for each time interval
#' @param  bands           vector with the names of the bands
#' @param  attr_names      vector with the attribute names
#' @param  int_labels      conversion vector from the labels to integer values
#' @param  init_row             Starting row from the RasterBrick
#' @param  nrows           Number of rows in the block extracted from the RasterBrick
#' @param  adj_fun         Adjustment function to be applied to the data
#' @param  ml_model        a model trained by \code{\link[sits]{sits_train}}
#' @param  multicores      Number of threads to process the time series.
#' @param  verbose         Run function in verbose mode (useful for working with big data sets)
#' @return layer.lst       list  of the classified raster layers
#'
.sits_classify_bigmemory <-  function(raster.tb, layers.lst,
                                     time_index.lst, bands,
                                     attr_names, int_labels,
                                     init_row, nrows,
                                     adj_fun, ml_model,
                                     multicores, verbose) {


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
    i <- 0
    if (verbose) .sits_log_debug(paste0("Memory used before readGDAL - ", .sits_mem_used(), " GB"))
    # go through all the bricks
    values.lst <- bricks.vec %>%
        purrr::map(function(r_brick) {
            # the readGDAL function returns a matrix
            # the rows of the matrix are the pixels
            # the cols of the matrix are the layers
            values.mx    <- as.matrix(rgdal::readGDAL(r_brick, offset, region.dim)@data)
            if (verbose) .sits_log_debug(paste0("Memory used after readGDAL - ", .sits_mem_used(), " GB"))

            # get the associated band
            i <<- i + 1
            band <- bands[i]
            missing_value <- missing_values[band]
            minimum_value <- minimum_values[band]
            scale_factor  <- scale_factors[band]

            values.mx[is.na(values.mx)] <- minimum_value

            values.mx <- preprocess_data(values.mx, missing_value, minimum_value, scale_factor, adj_value)

            return(values.mx)
        })

    dist.tb <- data.table::as.data.table(do.call(cbind, values.lst))
    if (verbose) .sits_log_debug(paste0("Memory used after binding bricks  - ", .sits_mem_used(), " GB"))
    # cleanup
    rm(values.lst)
    gc()
    if (verbose) .sits_log_debug(paste0("Memory used after removing values - ", .sits_mem_used(), " GB"))
    # create a data table for better memory management
    size <- nrows*ncols
    two_rows.tb <- data.table::data.table("original_row" = rep(1,size),
                                      "reference" = rep("NoClass", size))

    dist.tb <- cbind(two_rows.tb, dist.tb)
    if (verbose) .sits_log_debug(paste0("Memory used after adding two first rows  - ", .sits_mem_used(), " GB"))

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
            if (verbose) .sits_log_debug(paste0("Memory used after selecting data subset  - ", .sits_mem_used(), " GB"))

            colnames(dist1.tb) <- attr_names

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
                if (verbose) .sits_log_debug(paste0("Memory used after building chunks  - ", .sits_mem_used(), " GB"))
                rm(dist1.tb)
                gc()
                if (verbose) .sits_log_debug(paste0("Memory used before calling parallel processing - ", .sits_mem_used(), " GB"))
                # apply parallel processing to the split data and join the results
                pred.vec <- unlist(parallel::mclapply(blocks.lst, classify_block, mc.cores = multicores))
                # clean-up
                if (verbose) .sits_log_debug(paste0("Memory used after calling parallel processing - ", .sits_mem_used(), " GB"))
                rm(block_size.lst)
                rm(blocks.lst)
                gc()
                if (verbose) .sits_log_debug(paste0("Memory used after removing blocks - ", .sits_mem_used(), " GB"))
            }
            else {
                pred.vec <- classify_block(dist1.tb)
                rm(dist1.tb)
                gc()
                if (verbose) .sits_log_debug(paste0("Memory used after classification in one core - ", .sits_mem_used(), " GB"))
            }

            # check the result has the right dimension
            ensurer::ensure_that(pred.vec, length(.) == nrow(dist.tb),
                                 err_desc = "sits_classify_raster - number of classified pixels is different
                             from number of input pixels")

            # for each layer, write the predicted values
            values <- as.integer(int_labels[pred.vec])
            layers.lst[[t]]  <- raster::writeValues(layers.lst[[t]], values, init_row)


            message(paste0("Processed year ", t, " starting from row ", init_row))
            t <<- t + 1
            # clean-up
            rm(pred.vec)
            rm(values)
            gc()
            if (verbose) .sits_log_debug(paste0("Memory used after classification of year ", t, " - ", .sits_mem_used(), " GB"))
        })
    rm(dist.tb)
    gc()
    if (verbose) .sits_log_debug(paste0("Memory used after end of processing all years - ", .sits_mem_used(), " GB"))
    return(layers.lst)
}
