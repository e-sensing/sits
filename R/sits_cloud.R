#' @title Retrieve the indexes to be removed in time series
#' @name .sits_bitmask_values
#' @keywords internal
#'
#' @param cld_values         Cloud band values.
#' @param cld_index          Indexes to be removed in cloud values.
#' @return The values with indexes to be removed in time series.
.sits_bitmask_values <- function(cld_values, cld_index) {

    values_mask <-  purrr::map_dfr(seq_len(nrow(cld_values)), function(i) {

        purrr::map_dbl(cld_values[i,], function(x){

            bitwise <- bitwAnd(x, cld_index)
            if (any(bitwise != 0))
                return(bitwise[bitwise != 0][[1]])
            return(x[[1]])
        })
    })

    values_mask
}
