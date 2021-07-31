#' @title Add new sits bands and drops existing.
#' @name .sits_ops_compute
#' @keywords internal
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Adds new bands and drops existing in the time series
#'               of a sits tibble using dplyr::transmute function.
#' @param data          A sits tibble.
#' @param ...           Pair expressions in the format `name = value`.
#'                      See \code{\link[dplyr]{mutate}} help for more details.
#' @return A sits tibble with same samples and the new bands.
#'
#'
.sits_ops_compute <- function(data, ...) {

    # verify if data is valid
    .sits_tibble_test(data)

    # tricky to include "Index" column and expand `...` arguments
    proc_fun <- function(..., Index = Index) {
        Index <- quote(Index)
        purrr::map(data$time_series, function(ts) {
            ts_computed <- dplyr::transmute(ts, !!(Index), ...)
            return(ts_computed)
        })
    }

    # compute transmute for each time_series tibble
    tryCatch({
        data$time_series <- proc_fun(...)
    },
    error = function(e) {
        msg <- paste0("Error - Are your band names all uppercase?")
        message(msg)
    }
    )
    return(data)
}
