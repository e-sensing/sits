#' @title Apply a function over SITS bands.
#' @name sits_apply
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  `sits_apply` returns a sits_table with the same samples points and new bands computed by `fun`,
#' `fun_index` functions. These functions must be defined inline and are called by `sits_apply` for each band,
#' whose vector values is passed as the function argument.
#'
#' `fun` function may either return a vector or a list of vectors. In the first case, the vector will be the new values
#' of the corresponding band. In the second case, the returned list must have names, and each element vector will
#' generate a new band which name composed by concatenating original band name and the corresponding list element name.
#'
#' If a suffix is provided in `bands_suffix`, all resulting bands names will end with provided suffix separated by a ".".
#'
#' @param data.tb       a valid sits table
#' @param fun           a function with one parameter as input and a vector or list of vectors as output.
#' @param fun_index     a function with one parameter as input and a Date vector as output.
#' @param bands_suffix  a string informing the resulting bands name's suffix.
#' @return data.tb    a sits_table with same samples and the new bands
#' @export
sits_apply <- function(data.tb, fun, fun_index = function(index){ return(index) }, bands_suffix = "") {

    # verify if data.tb has values
    .sits_test_table(data.tb)

    # computes fun and fun_index for all time series and substitutes the original time series data
    data.tb$time_series <- data.tb$time_series %>%
        purrr::map(function(ts.tb) {
            ts_computed.lst <- dplyr::select(ts.tb, -Index) %>%
                purrr::map(fun)

            # append bands names' suffixes
            if (nchar(bands_suffix) != 0)
                names(ts_computed.lst) <- paste0(names(ts_computed.lst), ".", bands_suffix)

            # unlist if there are more than one result from `fun`
            if (is.recursive(ts_computed.lst[[1]]))
                ts_computed.lst <- unlist(ts_computed.lst, recursive = FALSE)

            # convert to tibble
            ts_computed.tb <- tibble::as_tibble(ts_computed.lst)

            # compute Index column
            ts_computed.tb <- dplyr::mutate(ts_computed.tb, Index = fun_index(ts.tb$Index))

            # reorganizes time series tibble
            return(dplyr::select(ts_computed.tb, Index, dplyr::everything()))
        })
    return(data.tb)
}

#' @title Add new SITS bands and drops existing.
#' @name sits_transmute
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Adds new bands and drops existing in a sits_table's time series,
#'               using dplyr::transmute function
#' @param data.tb       a valid sits table
#' @param ...           `name=value` pairs expressions. See `dplyr::transmute` help for more details.
#' @return data.tb      a sits_table with same samples and the new bands
#' @export
sits_transmute <- function(data.tb, ...){

    # verify if data.tb has values
    .sits_test_table(data.tb)

    # tricky to include "Index" column and expand `...` arguments
    proc_fun <- function(..., Index = Index){
        Index <- quote(Index)
        purrr::map(data.tb$time_series, function (ts.tb) {
            ts_computed.tb <- dplyr::transmute(ts.tb, !!(Index), ...)
        })
    }

    # compute transmute for each time_series tibble
    data.tb$time_series <- proc_fun(...)
    return(data.tb)
}
#' @title Add new SITS bands.
#' @name sits_mutate
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Adds new bands and preserves existing in a sits_table's time series,
#'               using dplyr::mutate function
#' @param data.tb       a valid sits table
#' @param ...           `name=value` pairs expressions. See `dplyr::mutate` help for more details.
#' @return data.tb      a sits_table with same samples and the new bands
#' @export
sits_mutate <- function(data.tb, ...){

    # verify if data.tb has values
    .sits_test_table(data.tb)

    # compute mutate for each time_series tibble
    proc_fun <- function(...){
        data.tb$time_series <- data.tb$time_series %>%
            purrr::map(function(ts.tb) {
                ts_computed.tb <- ts.tb %>%
                    dplyr::mutate(...)
                return(ts_computed.tb)
            })
    }

    # compute mutate for each time_series tibble
    data.tb$time_series <- proc_fun(...)
    return(data.tb)
}

#' @title Filter bands on a SITS table
#' @name sits_select
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description returns a sits table with the selected bands
#'
#' @param data.tb      a sits table with the time series of the selected bands
#' @param ...          one or more band names separated by commas. See `dplyr::select` help for more details.
#' @return data.tb     a tibble in SITS format with the selected bands
#' @export
sits_select <- function (data.tb, ...) {

    # verify if data.tb has values
    .sits_test_table(data.tb)

    # tricky to include "Index" column and expand `...` arguments
    proc_fun <- function(..., Index = Index){
        Index <- quote(Index)
        purrr::map(data.tb$time_series, function (ts.tb) {
                ts_computed.tb <- dplyr::select(ts.tb, !!(Index), ...)
        })
    }

    # select the chosen bands for the time series
    data.tb$time_series <- proc_fun(...)
    return (data.tb)
}
