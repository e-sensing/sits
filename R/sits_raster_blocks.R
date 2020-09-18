#' @title Define a reasonable block size to process a RasterBrick
#' @name .sits_raster_blocks
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Defines the size of the block of a Raster Brick to be read.
#' The total pixels of a RasterBrick  the size of the timeline
#' with the number of rows and columns of the Brick. For example, a Raster Brick
#' with 500 rows and 500 columns and 400 time instances will have a total pixel size
#' of 800 Mb if pixels are 64-bit.
#'
#' @param  cube            input data cube.
#' @param  ml_model        machine learning model.
#' @param  sub_image       bounding box of the ROI
#' @param  memsize         memory available for classification (in GB).
#' @param  multicores      number of threads to process the time series.
#' @return                 list with three attributes: n (number of blocks),
#'                         rows (list of rows to begin),
#'                         nrows (number of rows to read at each iteration).
#'
.sits_raster_blocks <- function(cube, ml_model, sub_image, memsize, multicores){
    # number of bands
    nbands <-  length(.sits_cube_bands(cube))
    # timeline
    timeline <- sits_timeline(cube[1,])
    # get the number of blocks
    nblocks <- .sits_raster_blocks_estimate(ml_model   = ml_model,
                                            nbands     = nbands,
                                            sub_image  = sub_image,
                                            timeline   = timeline,
                                            memsize    = memsize,
                                            multicores = multicores)

    block.lst <- .sits_raster_block_list(nblocks = nblocks, sub_image  = sub_image)

    return(block.lst)
}
#' @title Estimate the number of blocks
#' @name .sits_raster_blocks_estimate
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Defines the number of blocks of a Raster Brick
#'              to be read into memory.
#'
#' @param  ml_model        Machine learning model.
#' @param  nbands          Number of bands.
#' @param  sub_image       Area of interest in the image
#' @param  timeline        Timeline of the brick.
#' @param  memsize         Memory available for classification (in GB).
#' @param  multicores      Number of threads to process the time series.
#' @return Number of blocks to be read.
.sits_raster_blocks_estimate <- function(ml_model,
                                         nbands,
                                         sub_image,
                                         timeline,
                                         memsize,
                                         multicores) {
    # total number of instances
    ninstances <- length(timeline)
    # number of instances per classification interval
    samples <- environment(ml_model)$data
    ninterval <- nrow(samples[1,]$time_series[[1]])
    # number of bytes per pixel
    nbytes <-  8
    # estimated memory bloat
    bloat <- as.numeric(.sits_config_memory_bloat())
    # estimated processing bloat
    proc_bloat <- as.numeric(.sits_config_processing_bloat())
    if (proc_bloat == 0) proc_bloat <- multicores

    # number of rows and cols
    nrows <- sub_image["nrows"]
    ncols <- sub_image["ncols"]
    # single instance size
    single_data_size <- as.numeric(nrows)*as.numeric(ncols)*as.numeric(nbytes)
    # total size including all bands
    nbands_data_size <- single_data_size*as.numeric(nbands)

    # estimated full size of the data
    full_size <- as.numeric(ninstances)*nbands_data_size

    # estimated size of memory required for scaling and normalization
    mem_required_scaling <- (full_size + as.numeric(.sits_mem_used()))*bloat

    # number of labels
    nlabels <- length(sits_labels(environment(ml_model)$data)$label)
    # estimated size of the data for classification
    input_class_data_size <- as.numeric(ninterval)*nbands_data_size
    output_class_data_size <- as.numeric(nlabels)*single_data_size
    class_data_size <- input_class_data_size + output_class_data_size

    # memory required for processing depends on the model
    if ("keras_model" %in% class(ml_model) || "rfor_model" %in% class(ml_model))
    {
        mem_required_processing <- (class_data_size +
                                        as.numeric(.sits_mem_used()))*proc_bloat
    }
    else {
        # test two different cases
        if (ninstances == ninterval) # one interval only
            mem_required_processing <- as.numeric(multicores) *
                (class_data_size + as.numeric(.sits_mem_used()))
        else
            mem_required_processing <- as.numeric(multicores) *
                (.sits_mem_used() + class_data_size + full_size)
    }

    # number of passes to read the full data sets
    nblocks <- max(ceiling(mem_required_scaling/(memsize*1e+09)),
                   ceiling(mem_required_processing/(memsize*1e+09)))

    return(nblocks)
}
#' @title Calculate a list of blocks to be read from disk to memory
#' @name .sits_raster_block_list
#' @keywords internal
#'
#' @param  nblocks         number of blocks to read from each image
#' @param  sub_image       nrea of interest in the image
#' @return        a list with n (number of blocks), row (vector of starting rows),
#'                nrow (vector with number of rows for each block) and
#'                size (vector with size of each block)
#'
.sits_raster_block_list <- function(nblocks, sub_image){
    # number of rows per block
    nrows <- unname(sub_image["nrows"])
    ncols <- unname(sub_image["ncols"])
    block_rows <- ceiling(nrows/nblocks)
    first_row <- unname(sub_image["first_row"])
    last_row  <- first_row + nrows - 1
    first_col <- unname(sub_image["first_col"])
    last_col  <-  first_col + ncols - 1

    yres <- sub_image["yres"]
    xres <- sub_image["xres"]
    # initial row of each block
    row.vec <- seq.int(from = first_row, to = last_row, by = block_rows)
    # number of rows in each block
    nrows.vec <- rep.int(block_rows, length(row.vec))
    # check that total number of rows is the same as the sum of all blocks
    # correct the last block for overflow
    if (sum(nrows.vec) != nrows )
        nrows.vec[length(nrows.vec)] <- nrows - sum(nrows.vec[1:(length(nrows.vec) - 1)])

    # Y coordinates of block
    ymax.vec <- floor(sub_image["ymax"] - (row.vec - 1)*yres)
    ymin.vec <- floor(sub_image["ymax"] - (nrows.vec)*yres)


    # find out the size of the block in pixels
    size.vec <- nrows.vec * ncols
    # get the output blocks
    row_out.vec <- row.vec - first_row + 1

    # elements of the block list
    # n          number of blocks
    # row        starting row in each block (vector)
    # nrows      number of rows in each block (vector)
    # col        first col
    # ncols      number of cols in each block

    block.lst <- list(n = length(row.vec),
                      row = row.vec,
                      row_out = row_out.vec,
                      nrows = nrows.vec,
                      col   = unname(sub_image["first_col"]),
                      ncols = ncols,
                      ymax.vec = ymax.vec,
                      ymin.vec = ymin.vec,
                      xmin  = sub_image["xmin"],
                      xmax  = sub_image["xmax"],
                      size = size.vec)

    return(block.lst)
}
