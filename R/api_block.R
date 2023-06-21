#' @title Block API
#' @noRd
#'
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param x  Any object to extract a block.
#'
#' @description
#' A block represents a region of a matrix. A block is any
#' list or tibble containing col, row, ncols, and nrows fields.
#'
#' @examples
#' if (sits_run_examples()) {
#'     x <- list(a = 0, z = 0)
#'     .block(x) # NULL
#'     x <- list(a = 0, col = 1, row = 2, ncols = 3, nrows = 4, z = 0)
#'     .block(x)
#'     .block_size(x, 0)
#'     .block_size(x, 2)
#' }
#'
NULL

# block fields
.block_cols <- c("col", "row", "ncols", "nrows")
#' @title Check if an object contains a block
#' @noRd
#' @returns A logical indicating if an object contains a block.
.has_block <- function(x) {
    all(.block_cols %in% names(x))
}
#' @title Create a block
#' @noRd
#' @returns A block from any given object
.block <- function(x) {
    if (!.has_block(x)) {
        return(NULL)
    }
    col <- .default(x = .col(x), default = 1)
    row <- .default(x = .row(x), default = 1)
    # Return a block
    .common_size(col = col, row = row, ncols = .ncols(x), nrows = .nrows(x))
}
#' @title Compute block size in pixels
#' @noRd
#' @param block  A block.
#' @param overlap  Pixels to increase/decrease block `ncols` and `nrows`.
#' @returns The size of a block with overlaps.
.block_size <- function(block, overlap = 0) {
    (.nrows(block) + 2 * overlap) * (.ncols(block) + 2 * overlap)
}
