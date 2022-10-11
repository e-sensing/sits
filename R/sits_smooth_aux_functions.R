#' @name .smth_compute_blocks
#' @description Function to compute blocks grid
#' @keywords internal
#' @noRd
#'
#' @param xsize        X size of the image
#' @param ysize        Y size of the image
#' @param block_y_size Y size of the block
#' @param overlapping_y_size   Size of the overlap
#' @return  A list of blocks
#'
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
        row = ovr_r1,
        nrows = ovr_nr1,
        col = 1,
        ncols = xsize,
        crop_row = r1 - ovr_r1 + 1,
        crop_nrows = nr1,
        crop_col = 1,
        crop_ncols = xsize,
        SIMPLIFY = FALSE
    )

    return(blocks)
}
