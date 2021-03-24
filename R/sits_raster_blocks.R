#' @title Get the raster block size
#' @name .sits_raster_block_size
#' @keywords internal
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Retrieves the block size of an image.
#'
#' @param  cube            input data cube tile.
#' @return                 list with two attributes: n_rows, n_cols
#'
.sits_raster_block_size <- function(cube) {

    # call gdalinfo
    info_str <- gdalUtils::gdalinfo(cube$file_info[[1]]$path[[1]])

    # get first block band
    info_str <- stringr::str_subset(string = info_str,
                                    pattern = "Band 1 Block")

    # extract block size string
    info_str <- stringr::str_extract(string = info_str,
                                     pattern = "[0-9]+x[0-9]+")

    # post condition 1: block_size length == 1
    assertthat::assert_that(
        length(info_str) == 1,
        msg = ".sits_raster_block_size: gdalinfo output is not reconizable"
    )

    # split string
    block_size <- as.numeric(stringr::str_split(string = info_str,
                                                pattern = "x")[[1]])

    # check result
    # post condition 1: block_size length == 2
    assertthat::assert_that(
        length(block_size) == 2,
        msg = ".sits_raster_block_size: block size length is wrong"
    )

    # post condition 2: block_size values are strictly positive
    assertthat::assert_that(
        all(block_size > 0),
        msg = ".sits_raster_block_size: block size value must be positive"
    )

    # prepare result
    block_size <- lapply(block_size, c)
    names(block_size) <- c("n_cols", "n_rows")

    return(block_size)
}

#' @title Calculate a list of blocks to be read from disk to memory
#' @name .sits_raster_blocks
#' @keywords internal
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Defines the size of the block of an image to be read.
#'
#' @param  cube            input data cube tile.
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

    # get image block size
    block_size <- .sits_raster_block_size(cube = cube)

    # get the number of blocks
    block_lst <- .sits_raster_block_list(
        cube = cube,
        ml_model = ml_model,
        sub_image = sub_image,
        block_x_size = block_size[["n_cols"]],
        block_y_size = block_size[["n_rows"]],
        memsize = memsize,
        multicores = multicores
    )

    return(block_lst)
}
#' @title Calculate a list of blocks to be read from disk to memory
#' @name .sits_raster_block_list
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Defines the number of blocks of a set of images
#'              to be read into memory and compute its range.
#'
#' @param  cube            input data cube
#' @param  ml_model        machine learning model
#' @param  sub_image       area of interest in the image
#' @param  block_x_size    raster block cols
#' @param  block_y_size    raster block rows
#' @param  memsize         Memory available for classification (in GB).
#' @param  multicores      Number of threads to process the time series.
#' @return
#' a list with block elements. Each element containing:
#'   c1, c2 (start and end columns for the block), r1, and r2
#'   (start and end rows for the block)
#'
.sits_raster_block_list <- function(cube,
                                    ml_model,
                                    sub_image,
                                    block_x_size,
                                    block_y_size,
                                    memsize,
                                    multicores) {

    # retrieve the samples
    samples <- .sits_ml_model_samples(ml_model)

    # total number of instances
    n_instances <- length(sits_timeline(cube))

    # get the number of bands
    n_bands <- length(sits_bands(samples))

    # does the cube have a cloud band?
    cld_band <- .sits_config_cloud_band(cube)

    # the cube has the cloud band, add one more band to the calculation
    if (cld_band %in% sits_bands(cube)) {

        n_bands <- n_bands + 1
    }

    # number of bytes per pixel
    n_bytes <- 8

    # estimated processing bloat
    proc_bloat <- .sits_config_processing_bloat()

    # number of blocks to process at a time
    # this number is the minimum between multicores and number of blocks
    # to cover all sub_image
    num_blocks <- min(
        as.integer(sub_image[["ncols"]] * sub_image[["nrows"]] /
                       block_x_size / block_y_size),
        multicores
    )

    # total estimated block size in GB
    estimated_block_mem <- block_x_size *
        block_y_size * n_bands * n_bytes *
        n_instances * proc_bloat * num_blocks * 1e-09

    # check if memsize is sufficient to process a block in each core
    assertthat::assert_that(
        memsize >= estimated_block_mem,
        msg = paste(".sits_raster_blocks_estimate: insuficient memory to",
                    "process", num_blocks, "block(s) at a time")
    )

    # check if it is possible to process two or more blocks at a time
    mem_factor <- memsize / estimated_block_mem

    # increase vertically first
    if (mem_factor >= 2) {

        # block_y_size <- min(sub_image[["nrows"]],
        #                     as.integer(mem_factor) * block_y_size)
        block_y_size <- min(
            as.integer(mem_factor) * block_y_size,
            ceiling(sub_image[["nrows"]] / block_y_size) * block_y_size
        )
    }

    # compute new estimated block size in GB
    estimated_block_mem <- block_x_size *
        block_y_size * n_bands * n_bytes *
        n_instances * proc_bloat * num_blocks * 1e-09

    # check if it is possible to process even more blocks
    mem_factor <- memsize / estimated_block_mem

    # increase horizontally now
    if (mem_factor >= 2) {

        # block_x_size <- min(sub_image[["ncols"]],
        #                     as.integer(mem_factor) * block_x_size)
        block_x_size <- min(
            as.integer(mem_factor) * block_x_size,
            ceiling(sub_image[["ncols"]] / block_x_size) * block_x_size
        )
    }

    # generate blocks list
    # compute columns
    img_x_size <- cube$ncols
    c1 <- ceiling(seq(1, img_x_size - 1, by = block_x_size))
    c2 <- c(c1[-1] - 1)

    # filter only intersecting sub_image columns
    c1 <- c(sub_image[["first_col"]],
            c1[c1 > sub_image[["first_col"]] &
                   c1 <= sub_image[["first_col"]] + sub_image[["ncols"]]])
    c2 <- c(c2[c2 >= sub_image[["first_col"]] &
                   c2 < sub_image[["first_col"]] + sub_image[["ncols"]]],
            sub_image[["first_col"]] + sub_image[["ncols"]] - 1)

    # compute rows
    img_y_size <- cube$nrows
    r1 <- ceiling(seq(1, img_y_size - 1, by = block_y_size))
    r2 <- c(r1[-1] - 1)

    # filter only intersecting sub_image rows
    r1 <- c(sub_image[["first_row"]],
            r1[r1 > sub_image[["first_row"]] &
                   r1 <= sub_image[["first_row"]] + sub_image[["nrows"]]])
    r2 <- c(r2[r2 >= sub_image[["first_row"]] &
                   r2 < sub_image[["first_row"]] + sub_image[["nrows"]]],
            sub_image[["first_row"]] + sub_image[["nrows"]] - 1)

    # define each block as a list element with names r1, r2, c1, c2
    blocks <- do.call(mapply,
                      args = c(list(FUN = list, SIMPLIFY = FALSE),
                               merge(dplyr::tibble(row = r1,
                                                   nrows = r2 - r1 + 1),
                                     dplyr::tibble(col = c1,
                                                   ncols = c2 - c1 + 1)))
    )

    return(blocks)
}
#' @title Shows the memory used in GB
#' @name .sits_mem_used
#' @keywords internal
#' @description Calls the gc() and rounds the result in GB.
#' @return Memory used in GB.
.sits_mem_used <- function() {
    dt <- gc()
    return(sum(dt[, 2] / 1000))
}
