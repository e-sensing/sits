#' @title Create a filename to store an output block from smoothing operation
#' @name .smth_block_filename
#' @keywords internal
#'
#' @param tile         Output tile to be written.
#' @param output_dir   Directory where block will be written.
#' @param block        Block designation.
#'
#' @return  A filename to store an output block from smoothing operation.
#'
.smth_filename <- function(tile,
                           output_dir,
                           block) {
    band <- .file_info_bands(tile)

    start_date <- .file_info_start_date(tile)

    end_date <- .file_info_end_date(tile)

    b_filename <- paste("cube",
        .cube_tiles(tile),
        band,
        start_date,
        end_date,
        sep = "_"
    )

    b_filename <- paste(b_filename,
        "block",
        block[["first_row"]],
        block[["nrows"]] + block[["first_row"]] - 1,
        sep = "_"
    )

    b_path <- paste0(file.path(output_dir, b_filename), ".tif")

    return(b_path)
}
#' @title Estimate the number of blocks to run .sits_split_cluster
#' @name .smth_estimate_block_size
#' @keywords internal
#'
#' @param cube         input data cube
#' @param multicores   number of processes to split up the data
#' @param memsize      maximum overall memory size (in GB)
#'
#' @return  returns a list with following information:
#'             - multicores theoretical upper bound;
#'             - block x_size (horizontal) and y_size (vertical)
#'
.smth_estimate_block_size <- function(cube, multicores, memsize) {

    # set caller to show in errors
    .check_set_caller(".smth_estimate_block_size")

    # precondition 1 - check if cube has probability data
    .check_that(
        x = inherits(cube, "probs_cube"),
        msg = "input is not probability cube"
    )
    size <- .cube_size(cube[1, ])
    n_layers <- length(cube$labels[[1]])
    bloat_mem <- .config_get(key = "processing_bloat_smooth")
    n_bytes <- 8

    # total memory needed to do all work in GB
    image_size <- size[["ncols"]] * size[["nrows"]]
    needed_memory <- image_size * 1E-09 * n_layers * bloat_mem * n_bytes

    # minimum block size
    min_block_x_size <- size["ncols"] # for now, only vertical blocking
    min_block_y_size <- 1

    # compute factors
    memory_factor <- needed_memory / memsize

    blocking_factor <- image_size / (min_block_x_size * min_block_y_size)

    # stop if blocking factor is less than memory factor!
    # reason: the provided memory is not enough to process the data by
    # breaking it into small chunks
    .check_that(
        x = memory_factor <= blocking_factor,
        msg = "provided memory not enough to run the job"
    )

    # update multicores to the maximum possible processes given the available
    # memory and blocking factor
    multicores <- min(floor(blocking_factor / memory_factor), multicores)

    # compute blocking allocation that maximizes the
    # block / (memory * multicores) ratio, i.e. maximize parallel processes
    # and returns the following information:
    # - multicores theoretical upper bound;
    # - block x_size (horizontal) and y_size (vertical)
    blocks <- list(
        # theoretical max_multicores = floor(blocking_factor / memory_factor),
        block_x_size = floor(min_block_x_size),
        block_y_size = min(
            floor(blocking_factor / memory_factor / multicores),
            size[["nrows"]]
        )
    )

    return(blocks)
}

# function to compute blocks grid
.smth_compute_blocks <- function(xsize,
                                 ysize,
                                 block_y_size,
                                 overlapping_y_size) {
    r1 <- seq(1, ysize - 1, by = block_y_size)
    r2 <- c(r1[-1] - 1, ysize)
    nr1 <- r2 - r1 + 1
    ovr_r1 <- c(1, c(r1[-1] - overlapping_y_size))
    ovr_r2 <- c(r2[-length(r2)] + overlapping_y_size, ysize)
    ovr_nr1 <- ovr_r2 - ovr_r1 + 1

    # define each block as a list element
    blocks <- mapply(
        list,
        first_row      = ovr_r1,
        nrows          = ovr_nr1,
        first_col      = 1,
        ncols          = xsize,
        crop_first_row = r1 - ovr_r1 + 1,
        crop_nrows     = nr1,
        crop_first_col = 1,
        crop_ncols     = xsize,
        SIMPLIFY       = FALSE
    )

    return(blocks)
}
