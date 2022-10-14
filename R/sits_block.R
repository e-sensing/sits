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
#' x <- list(a = 0, z = 0)
#' .block(x) # NULL
#' x <- list(a = 0, col = 1, row = 2, ncols = 3, nrows = 4, z = 0)
#' .block(x)
#' .block_size(x, 0)
#' .block_size(x, 2)
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

#' @title Block accessors
#' @noRd
#'
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description
#' These functions are accessors of block fields in a vector.
#' Getters functions returns the respective field values with the expected
#' data type. Setters functions convert value to expected data type and
#' store it in respective fields on a given object. If value has no length
#' and the vector is not atomic, it is removed from the object.
#'
#' @examples
#' if (sits_run_examples()) {
#' x <- list(nrows = 123)
#' .nrows(x)
#' .ncols(x) <- 234
#' x
#' }
NULL

.col <- function(x) {
    .as_int(.compact(x[["col"]]))
}
`.col<-` <- function(x, value) {
    x[["col"]] <- .as_int(value)
    x
}
.row <- function(x) {
    .as_int(.compact(x[["row"]]))
}
`.row<-` <- function(x, value) {
    x[["row"]] <- .as_int(value)
    x
}
.ncols <- function(x) {
    .as_int(.compact(x[["ncols"]]))
}
`.ncols<-` <- function(x, value) {
    x[["ncols"]] <- .as_int(value)
    x
}
.nrows <- function(x) {
    .as_int(.compact(x[["nrows"]]))
}
`.nrows<-` <- function(x, value) {
    x[["nrows"]] <- .as_int(value)
    x
}
