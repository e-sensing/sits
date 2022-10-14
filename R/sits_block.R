#' Block API
#'
#' A block represents a region of a matrix. A \code{block} is any
#' \code{list} or \code{tibble} containing \code{col}, \code{row},
#' \code{ncols}, and \code{nrows} fields.
#'
#' @param x Any object to extract a \code{block}.
#' @param block Any object with \code{ncols}, \code{nrows} fields.
#' @param overlap Pixels to increase/decrease block \code{ncols} and
#' \code{nrows}.
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
#' @seealso \link{block_accessors}
#' @family region objects API
#' @keywords internal
#' @noRd
#' @name block_api
NULL

# block fields
.block_cols <- c("col", "row", "ncols", "nrows")

#' @describeIn block_api Does vector \code{x} has \code{block} fields?
#' @returns \code{.has_block()}: \code{logical}.
#' @noRd
.has_block <- function(x) {
    all(.block_cols %in% names(x))
}

#' @describeIn block_api Extract a \code{block} from any given \code{vector}.
#' @returns \code{.block()}: \code{block}.
#' @noRd
.block <- function(x) {
    if (!.has_block(x)) {
        return(NULL)
    }
    col <- .default(x = .col(x), default = 1)
    row <- .default(x = .row(x), default = 1)
    # Return a block
    .common_size(col = col, row = row, ncols = .ncols(x), nrows = .nrows(x))
}

#' @describeIn block_api Compute the number of pixels for a \code{block}
#'   considering an additional overlapping parameter.
#' @returns \code{.block_size()}: \code{integer}.
#' @noRd
.block_size <- function(block, overlap = 0) {
    (block[["nrows"]] + 2 * overlap) * (block[["ncols"]] + 2 * overlap)
}
