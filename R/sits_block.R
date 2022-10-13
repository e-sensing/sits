#
# Block API
#
# A block represents a region of a matrix. A block is any
# list or tibble containing col, row, ncols, and nrows fields.
# block fields
#'@noRd
.block_cols <- c("col", "row", "ncols", "nrows")

#' @name .block
#' @param x        Any object to extract a block.
#' @return        block
#' @keywords internal
#' @noRd
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
.block <- function(x) {
    if (!all(.block_cols %in% names(x))) {
        return(NULL)
    }
    as.list(x[.block_cols])
}

#' @noRd
#' @name .block_size
#' @param block    A block.
#' @param overlap  Pixels to increase/decrease block ncols  and nrows.
#' @returns        The size of a block with overlaps
.block_size <- function(block, overlap = 0) {
    (block[["nrows"]] + 2 * overlap) * (block[["ncols"]] + 2 * overlap)
}
#  Block accessors
#
#  These functions are accessors of block} fields in a vector.
#  Getters functions returns the respective field values with the expected
#  data type. Setters functions convert value to expected data type and
#  store it in respective fields on a given object. If value has no length
#  and the vector is not atomic, it is removed from the object.
#
#
.col <- function(x) {
    .as_int(.compact(x[["col"]]))
}
#
`.col<-` <- function(x, value) {
    x[["col"]] <- .as_int(value)
    x
}
#
.row <- function(x) {
    .as_int(.compact(x[["row"]]))
}
#
`.row<-` <- function(x, value) {
    x[["row"]] <- .as_int(value)
    x
}
#
.ncols <- function(x) {
    .as_int(.compact(x[["ncols"]]))
}
#
`.ncols<-` <- function(x, value) {
    x[["ncols"]] <- .as_int(value)
    x
}
#
.nrows <- function(x) {
    .as_int(.compact(x[["nrows"]]))
}
#
`.nrows<-` <- function(x, value) {
    x[["nrows"]] <- .as_int(value)
    x
}
