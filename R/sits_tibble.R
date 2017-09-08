#' @title Create a sits table to store the time series information
#' @name sits_tibble
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description this function returns an empty sits table.
#' SITS tibbles are the main structures of the "sits" package.
#  They contain both the satellite image time series and its metadata.
#' A sits tibble is a tibble with pre-defined columns that
#' has the metadata and data for each time series. The columns are
#' <longitude, latitude, start_date, end_date, label, coverage, time_series>
#' Most functions on the sits package use a sits tibble as input (with additional parameters)
#' and a sits tibble as output. This allows for chaining of operation on time series.
#'
#' @return result.tb  a tibble in SITS format
#' @export

sits_tibble <- function () {
    result.tb <- tibble::tibble(longitude   = double(),
                                latitude    = double (),
                                start_date  = as.Date(character()),
                                end_date    = as.Date(character()),
                                label       = character(),
                                coverage    = character(),
                                time_series = list()
    )
    class (result.tb) <- append (class(result.tb), "sits_tibble")
    return (result.tb)
}


#' @title Tests if a sits tibble is valid
#' @name .sits_test_tibble
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Tests if a SITS tibble exists or has data inside
#'
#' @param data.tb  a SITS tibble
#' @return returns TRUE if data.tb has data.
#'
.sits_test_tibble<- function (data.tb) {
    ensurer::ensure_that(data.tb, !purrr::is_null(.),
                         err_desc = "input data not provided")
    ensurer::ensure_that(data.tb, NROW(.) > 0,
                         err_desc = "input data is empty")
    return (TRUE)
}
