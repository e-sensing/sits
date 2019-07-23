#' @title Define a reasonable block size to process a RasterBrick
#' @name .sits_raster_blocks
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Defines the size of the block of a Raster Brick to be read into memory.
#' The total pixels of a RasterBrick is given by combining the size of the timeline
#' with the number of rows and columns of the Brick. For example, a Raster Brick
#' with 500 rows and 500 columns and 400 time instances will have a total pixel size
#' of 800 Mb if pixels are 64-bit.
#'
#' @param  cube            Input data cube.
#' @param  ml_model        Machine learning model.
#' @param  interval        Classification interval.
#' @param  memsize         Memory available for classification (in GB).
#' @param  multicores      Number of threads to process the time series.
#' @return List with three attributes: n (number of blocks), rows (list of rows to begin),
#' nrows (number of rows to read at each iteration).
.sits_raster_blocks <- function(cube, ml_model, interval, memsize, multicores){
    # number of bands
    nbands <-  length(cube[1,]$bands[[1]])
    # number of rows and cols
    nrows <- cube[1,]$nrows
    ncols <- cube[1,]$ncols
    # timeline
    timeline <- cube[1,]$timeline[[1]][[1]]

    nblocks <- .sits_estimate_nblocks(ml_model, nbands, nrows, ncols, timeline, interval, memsize, multicores)

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

#' @title Estimate the number of blocks
#' @name .sits_estimate_nblocks
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Defines the number of blocks of a Raster Brick to be read into memory.
#'
#' @param  ml_model        Machine learning model.
#' @param  nbands          Number of bands.
#' @param  nrows           Number of rows per brick.
#' @param  ncols           Number of cols per brick.
#' @param  timeline        Timeline of the brick.
#' @param  interval        Classification interval.
#' @param  memsize         Memory available for classification (in GB).
#' @param  multicores      Number of threads to process the time series.
#' @return Number of blocks to be read.
.sits_estimate_nblocks <- function(ml_model, nbands, nrows, ncols, timeline, interval, memsize, multicores) {
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
    bloat <- as.numeric(sits.env$config$R_memory_bloat)
    # estimated processing bloat
    proc_bloat <- as.numeric(sits.env$config$R_processing_bloat)
    if (proc_bloat == 0) proc_bloat <- multicores

    # single instance size
    single_data_size <- as.numeric(nrows)*as.numeric(ncols)*as.numeric(nbytes)
    # total size including all bricks
    bricks_data_size <- single_data_size*as.numeric(nbands)

    # estimated full size of the data
    full_data_size <- as.numeric(ninstances)*bricks_data_size

    # estimated size of memory required for scaling and normalization
    mem_required_scaling <- (full_data_size + as.numeric(.sits_mem_used()))*bloat

    .sits_log_debug(paste0("max memory required for scaling (GB) - ", round(mem_required_scaling/1e+09, digits = 3)))

    # number of labels
    nlabels <- length(sits_labels(environment(ml_model)$data.tb)$label)
    # estimated size of the data for classification
    input_class_data_size <- as.numeric(ninterval)*bricks_data_size
    output_class_data_size <- as.numeric(nlabels)*single_data_size
    class_data_size <- input_class_data_size + output_class_data_size

    # memory required for processing depends on the model
    if ( !(purrr::is_null(environment(ml_model)$model.keras)) || !(purrr::is_null(environment(ml_model)$result_ranger)))  {
        .sits_log_debug(paste0("keras and ranger run on multiple threads"))
        mem_required_processing <- (class_data_size + as.numeric(.sits_mem_used()))*proc_bloat
    }
    else {
        # test two different cases
        if (ninstances == ninterval) # one interval only
           mem_required_processing <- as.numeric(multicores)*(class_data_size + as.numeric(.sits_mem_used()))
        else
            mem_required_processing <- as.numeric(multicores)*(.sits_mem_used() + class_data_size + full_data_size)
    }
    .sits_log_debug(paste0("max memory required for processing (GB) - ", round(mem_required_processing/1e+09, digits = 3)))

    # number of passes to read the full data sets
    nblocks <- max(ceiling(mem_required_scaling/(memsize*1e+09)), ceiling(mem_required_processing/(memsize*1e+09)))

    return(nblocks)
}

#' @title Split a data.table or a matrix for multicore processing
#' @name .sits_split_data
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function splits a data.table into a list of chunks for multicore processing.
#'
#' @param data             Data (data.table or matrix).
#' @param ncores           Number of cores for processing.
#' @return List of pairs of positions (first row, last row) to be assigned to each core.
.sits_split_data <- function(data, ncores){
    # number of rows in the data
    nrows <- nrow(data)
    # find the number of rows per core
    step <- ceiling(nrows/ncores)

    # create a vector with the initial rows per block
    blocks <- seq(from = 1, to = nrows, by = step)

    # create a list to store the result
    block.lst <- vector("list", ncores)

    # fill the list with the initial and final row per block
    for (i in 1:length(blocks)) {
        start <- blocks[i]
        end   <- start + step - 1
        if (i == ncores )
            end <- nrows
        block.lst[[i]] <- data[start:end,]
    }
    return(block.lst)
}
