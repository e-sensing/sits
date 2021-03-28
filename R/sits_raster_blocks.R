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
#' @param  cube            input data cube.
#' @param  ml_model        machine learning model.
#' @param  sub_image       bounding box of the ROI
#' @param  memsize         memory available for classification (in GB).
#' @param  multicores      number of threads to process the time series.
#' @return                 list with three attributes: n (number of blocks),
#'                         rows (list of rows to begin),
#'                         nrows (number of rows to read at each iteration).
#'
.sits_raster_blocks <- function(cube, ml_model, sub_image,
                                memsize, multicores) {

    # get the number of blocks
    nblocks <- .sits_raster_blocks_estimate(
        cube = cube,
        ml_model = ml_model,
        sub_image = sub_image,
        memsize = memsize,
        multicores = multicores
    )

    block_lst <- .sits_raster_block_list(
        nblocks = nblocks,
        sub_image = sub_image
    )

    return(block_lst)
}
#' @title Estimate the number of blocks
#' @name .sits_raster_blocks_estimate
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Defines the number of blocks of a Raster Brick
#'              to be read into memory.
#'
#' @param  cube            input data cube
#' @param  ml_model        machine learning model.
#' @param  sub_image       area of interest in the image
#' @param  memsize         Memory available for classification (in GB).
#' @param  multicores      Number of threads to process the time series.
#' @return Number of blocks to be read.
.sits_raster_blocks_estimate <- function(cube,
                                         ml_model,
                                         sub_image,
                                         timeline,
                                         memsize,
                                         multicores) {
    # total number of instances
    timeline <- sits_timeline(cube[1, ])
    ninstances <- length(timeline)
    # retrieve the samples
    samples <- environment(ml_model)$data
    # get the number of bands
    nbands <- length(sits_bands(samples))
    # does the cube have a cloud band?
    cube_bands <- sits_bands(cube)
    cld_band <- .sits_config_cloud_band(cube)
    # the cube has the cloud band, add one more band to the calculation
    if (cld_band %in% cube_bands) {
        nbands <- nbands + 1
    }
    # number of instances per classification interval
    ninterval <- nrow(samples[1, ]$time_series[[1]])
    # number of bytes per pixel
    nbytes <- 8
    # estimated memory bloat
    bloat <- as.numeric(.sits_config_memory_bloat())
    # estimated processing bloat
    proc_bloat <- as.numeric(.sits_config_processing_bloat())
    if (proc_bloat == 0) proc_bloat <- multicores

    # number of rows and cols
    nrows <- as.numeric(sub_image["nrows"])
    ncols <- as.numeric(sub_image["ncols"])
    # single instance size
    single_data_size <- nrows * ncols * nbytes
    # total size including all bands
    nbands_data_size <- single_data_size * nbands

    # estimated full size of the data
    full_size <- as.numeric(ninstances) * nbands_data_size

    # estimated size of memory required for scaling and normalization
    mem_required_scaling <- (full_size + as.numeric(.sits_mem_used())) * bloat

    # number of labels
    nlabels <- length(sits_labels(environment(ml_model)$data))
    # estimated size of the data for classification
    input_class_data_size <- as.numeric(ninterval) * nbands_data_size
    output_class_data_size <- as.numeric(nlabels) * single_data_size
    class_data_size <- input_class_data_size + output_class_data_size

    # memory required for processing depends on the model
    if ("keras_model" %in% class(ml_model) | "ranger_model" %in% class(ml_model)
        | "xgb_model" %in% class(ml_model)) {
        mem_required_processing <-
            (class_data_size + as.numeric(.sits_mem_used())) * proc_bloat
    }
    else {
        # test two different cases
        if (ninstances == ninterval) { # one interval only
            mem_required_processing <- as.numeric(multicores) *
                (class_data_size + as.numeric(.sits_mem_used()))
        } else {
            mem_required_processing <- as.numeric(multicores) *
                (.sits_mem_used() + class_data_size + full_size)
        }
    }

    # number of passes to read the full data sets
    nblocks <- max(
        ceiling(mem_required_scaling / (memsize * 1e+09)),
        ceiling(mem_required_processing / (memsize * 1e+09))
    )

    return(nblocks)
}
#' @title Calculate a list of blocks to be read from disk to memory
#' @name .sits_raster_block_list
#' @keywords internal
#' @param  nblocks         number of blocks to read from each image
#' @param  sub_image       nrea of interest in the image
#' @return        a list with n (number of blocks),
#'                row (vector of starting rows),
#'                nrow (vector with number of rows for each block) and
#'                size (vector with size of each block)
#'
.sits_raster_block_list <- function(nblocks, sub_image) {
    # number of rows per block
    block_rows <- ceiling(sub_image["nrows"] / nblocks)

    first_row <- unname(sub_image["first_row"])
    last_row <- first_row + unname(sub_image["nrows"]) - 1

    # initial row of each block
    row_vec <- seq.int(
        from = first_row,
        to = last_row,
        by = block_rows
    )

    # number of rows in each block
    n_rows <- length(row_vec)
    assertthat::assert_that(n_rows > 0, msg = "empty row vector")
    nrows_vec <- rep.int(block_rows, n_rows)

    # check that total number of rows is the same as the sum of all blocks
    # correct the last block for overflow
    if (sum(nrows_vec) != sub_image["nrows"]) {
        nrows_vec[length(nrows_vec)] <-
            sub_image["nrows"] - sum(nrows_vec[1:(length(nrows_vec) - 1)])
    }

    # find out the size of the block in pixels
    size_vec <- nrows_vec * sub_image["ncols"]

    # elements of the block list
    # n          number of blocks
    # row        starting row in each block (vector)
    # nrows      number of rows in each block (vector)
    # col        first col
    # ncols      number of cols in each block

    blocks <- list(
        n = length(row_vec),
        row = row_vec,
        nrows = nrows_vec,
        col = sub_image["first_col"],
        ncols = sub_image["ncols"],
        size = size_vec
    )

    message(
        "Using ", blocks$n, " blocks of size ",
        blocks$nrows[1], " x ", blocks$ncols
    )

    return(blocks)
}
#' @title Shows the memory used in GB
#' @name .sits_mem_used
#' @keywords internal
#' @description Calls the gc() and rounds the result in GB.
#' @return Memory used in GB.
#' @export
.sits_mem_used <- function() {
    dt <- gc()
    return(sum(dt[, 2] / 1000))
}
