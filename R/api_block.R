#' @title Block API
#' @noRd
#'
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#'
#' @param x  Any object to extract a block.
#'
#' @description
#' A block represents a region of a matrix. A block is any
#' list or tibble containing col, row, ncols, and nrows fields.
#'
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
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#'
#' @description
#' These functions are accessors of block fields in a vector.
#' Getters functions returns the respective field values with the expected
#' data type. Setters functions convert value to expected data type and
#' store it in respective fields on a given object. If value has no length
#' and the vector is not atomic, it is removed from the object.
NULL

#' @title Returns the initial column of the block
#' @name .col
#' @description Returns the the initial column of the block
#' @noRd
#' @param x    A block
#' @returns    Initial column
.col <- function(x) {
    .as_int(.compact(x[["col"]]))
}
#' @title Assigns the initial column of the block
#' @name `.col<-`
#' @description Assigns the the initial column of the block
#' @noRd
#' @param x        A block
#' @param value    Initial column
#' @returns  Updated block
`.col<-` <- function(x, value) {
    x[["col"]] <- .as_int(value)
    x
}
#' @title Returns the initial row of the block
#' @name .row
#' @description Returns the the initial row of the block
#' @noRd
#' @param x    A block
#' @returns    Initial row
.row <- function(x) {
    .as_int(.compact(x[["row"]]))
}
#' @title Assigns the initial row of the block
#' @name `.row<-`
#' @description Assigns the the initial row of the block
#' @noRd
#' @param x        A block
#' @param value    Initial row
#' @returns  Updated block
`.row<-` <- function(x, value) {
    x[["row"]] <- .as_int(value)
    x
}
#' @title Number of columns in the block
#' @name .ncols
#' @description Returns the number of columns of the block
#' @noRd
#' @param x    A block
#' @returns    Number of columns
.ncols <- function(x) {
    .as_int(.compact(x[["ncols"]]))
}
#' @title Assigns number of columns in the block
#' @name `.ncols<-`
#' @description Assigns the number of columns of the block
#' @noRd
#' @param x        A block
#' @param value    Number of columns
#' @returns        Updated block
`.ncols<-` <- function(x, value) {
    x[["ncols"]] <- .as_int(value)
    x
}
#' @title Number of rows in the block
#' @name .nrows
#' @description Returns the number of rows in the block
#' @noRd
#' @param x    A block
#' @returns    Number of rows
.nrows <- function(x) {
    .as_int(.compact(x[["nrows"]]))
}
#' @title Assigns number of rows in the block
#' @name `.nrows<-`
#' @description Assigns the number of rows in the block
#' @noRd
#' @param x        A block
#' @param value    Number of rows
#' @returns        Updated block
`.nrows<-` <- function(x, value) {
    x[["nrows"]] <- .as_int(value)
    x
}

#' @title Regulate block size
#'
#' @description
#' Terra requires at least two pixels to recognize an extent as valid
#' polygon and not a line or point
#'
#' @noRd
#' @param block  A block.
#' @returns A block with the size fixed
.block_regulate_size <- function(block) {
    block[block == 1] <- 2
    block
}
