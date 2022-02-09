#' @title Define a reasonable block size to process an image subset
#' @name .sits_raster_blocks
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Defines the size of the block of an image to be read.
#' For example, a Raster Brick with 500 rows and 500 columns
#' and 400 time instances will have a total pixel size
#' of 800 Mb if pixels are 64-bit.
#'
#' @param  tile            tile of input data cube.
#' @param  ml_model        machine learning model.
#' @param  sub_image       bounding box of the ROI
#' @param  memsize         memory available for classification (in GB).
#' @param  multicores      number of threads to process the time series.
#' @return                 list with three attributes: n (number of blocks),
#'                         rows (list of rows to begin),
#'                         nrows (number of rows to read at each iteration).
#'
.sits_raster_blocks <- function(tile, ml_model, sub_image,
                                memsize, multicores) {

    # get the number of blocks
    nblocks <- .sits_raster_blocks_estimate(
        tile = tile,
        ml_model = ml_model,
        sub_image = sub_image,
        memsize = memsize,
        multicores = multicores
    )

    blocks <- .sits_raster_block_list(
        nblocks = nblocks,
        sub_image = sub_image
    )

    return(blocks)
}
#' @title Estimate the number of blocks
#' @name .sits_raster_blocks_estimate
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Defines the number of blocks of a Raster Brick
#'              to be read into memory.
#'
#' @param  tile            tile of data cube
#' @param  ml_model        machine learning model.
#' @param  sub_image       area of interest in the image
#' @param  memsize         Memory available for classification (in GB).
#' @param  multicores      Number of threads to process the time series.
#' @return Number of blocks to be read.
.sits_raster_blocks_estimate <- function(tile,
                                         ml_model,
                                         sub_image,
                                         memsize,
                                         multicores) {
    # total number of instances in the time
    ninstances <- length(sits_timeline(tile))
    # retrieve the samples
    samples <- environment(ml_model)$data
    # get the number of bands
    nbands <- length(sits_bands(samples))
    # does the cube have a cloud band?
    cube_bands <- sits_bands(tile)
    cld_band <- .source_cloud()
    # the cube has the cloud band, add one more band to the calculation
    if (cld_band %in% cube_bands) {
        nbands <- nbands + 1
    }
    # number of instances per classification interval
    ninterval <- nrow(samples$time_series[[1]])
    # number of bytes per pixel
    nbytes <- 8
    # estimated processing bloat
    proc_bloat <- as.numeric(.config_processing_bloat())
    if (proc_bloat == 0) proc_bloat <- multicores

    # number of rows and cols
    nrows <- sub_image[["nrows"]]
    ncols <- sub_image[["ncols"]]
    # single instance size
    single_data_size <- nrows * ncols * nbytes
    # total size including all bands
    nbands_data_size <- single_data_size * nbands

    # number of labels
    nlabels <- length(sits_labels(environment(ml_model)$data))
    # estimated size of the data for classification
    input_data_size <- as.numeric(ninterval) * nbands_data_size
    output_data_size <- as.numeric(nlabels) * single_data_size
    class_data_size <- (input_data_size + output_data_size) * proc_bloat

    # number of passes to read the full data sets
    nblocks <- ceiling(class_data_size * 1e-09 / memsize * multicores)

    return(nblocks)
}
#' @title Calculate a list of blocks to be read from disk to memory
#' @name .sits_raster_block_list
#' @keywords internal
#' @param  nblocks         number of blocks to read from each image
#' @param  sub_image       area of interest in the image
#' @return        a list with named vectors ("first_row", "nrows", "first_col", "ncols")
#'
.sits_raster_block_list <- function(nblocks, sub_image) {

    # set caller to show in errors
    .check_set_caller(".sits_raster_block_list")

    # number of rows per block
    block_rows <- ceiling(sub_image[["nrows"]] / nblocks)

    first_row <- unname(sub_image[["first_row"]])
    last_row <- first_row + unname(sub_image[["nrows"]]) - 1

    # initial row of each block
    row_vec <- seq.int(
        from = first_row,
        to = last_row,
        by = block_rows
    )

    # number of rows in each block
    n_rows <- length(row_vec)
    .check_that(
        x = n_rows > 0,
        msg = "empty row vector"
    )
    nrows_vec <- rep.int(block_rows, n_rows)

    # check that total number of rows is the same as the sum of all blocks
    # correct the last block for overflow
    if (sum(nrows_vec) != sub_image[["nrows"]]) {
        nrows_vec[length(nrows_vec)] <-
            sub_image[["nrows"]] - sum(nrows_vec[1:(length(nrows_vec) - 1)])
    }

    # elements of the block list
    # row        starting row in each block
    # nrows      number of rows in each block
    # col        first col
    # ncols      number of cols in each block
    blocks <- purrr::map2(row_vec, nrows_vec, function(rv, nr){
        block <- c("first_row"   = rv,
                   "nrows"       = nr,
                   "first_col"   = sub_image[["first_col"]],
                   "ncols"       = sub_image[["ncols"]]
        )

        return(block)
    })
    return(blocks)
}




#' @title Define a reasonable block size to process an image subset
#' @name .sits_raster_blocks_apply
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Defines the size of the block of an image to be read.
#' For example, a Raster Brick with 500 rows and 500 columns
#' and 400 time instances will have a total pixel size
#' of 800 Mb if pixels are 64-bit.
#'
#' @param  tile            tile of input data cube.
#' @param  sub_image       bounding box of the ROI
#' @param  memsize         memory available for classification (in GB).
#' @param  multicores      number of threads to process the time series.
#' @return                 list with three attributes: n (number of blocks),
#'                         rows (list of rows to begin),
#'                         nrows (number of rows to read at each iteration).
#'
.sits_raster_blocks_apply <- function(tile, sub_image, memsize, multicores) {

    # get the number of blocks
    nblocks <- .sits_raster_blocks_estimate_apply(
        tile = tile,
        sub_image = sub_image,
        memsize = memsize,
        multicores = multicores
    )

    blocks <- .sits_raster_block_list(
        nblocks = nblocks,
        sub_image = sub_image
    )

    return(blocks)
}
#' @title Estimate the number of blocks
#' @name .sits_raster_blocks_estimate_apply
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Defines the number of blocks of a Raster Brick
#'              to be read into memory.
#'
#' @param  tile            tile of data cube
#' @param  sub_image       area of interest in the image
#' @param  memsize         Memory available for classification (in GB).
#' @param  multicores      Number of threads to process the time series.
#' @return Number of blocks to be read.
.sits_raster_blocks_estimate_apply <- function(tile,
                                               sub_image,
                                               memsize,
                                               multicores) {

    # get the number of bands
    nbands <- length(sits_bands(tile))
    # does the cube have a cloud band?
    cube_bands <- sits_bands(tile)
    cld_band <- .source_cloud()
    # the cube has the cloud band, add one more band to the calculation
    if (cld_band %in% cube_bands) {
        nbands <- nbands + 1
    }
    # number of bytes per pixel
    nbytes <- 8
    # estimated processing bloat
    proc_bloat <- as.numeric(.config_processing_bloat())
    if (proc_bloat == 0) proc_bloat <- multicores

    # number of rows and cols
    nrows <- sub_image[["nrows"]]
    ncols <- sub_image[["ncols"]]
    # single instance size
    output_data_size <- nrows * ncols * nbytes
    # total size including all bands
    input_data_size <- output_data_size * nbands

    # number of output instances is the same as input
    # estimated size of the data for apply
    class_data_size <- (input_data_size + input_data_size) * proc_bloat

    # number of passes to read the full data sets
    nblocks <- ceiling(class_data_size * 1e-09 / memsize * multicores)

    return(nblocks)
}
