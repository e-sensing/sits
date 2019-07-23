#' @title Create a sits tibble to store the time series information
#' @name .sits_tibble
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function returns an empty sits tibble.
#' Sits tibbles are the main structures of sits package.
#' They contain both the satellite image time series and its metadata.
#' A sits tibble is a tibble with pre-defined columns that
#' has the metadata and data for each time series. The columns are
#' <longitude, latitude, start_date, end_date, label, cube, time_series>.
#' Most functions of sits package get a sits tibble as input (with additional parameters)
#' and return another sits tibble as output. This allows chaining functions over sits tibbles.
#'
#' @return A sits tibble.
#' @export
.sits_tibble <- function() {
    result.tb <- tibble::tibble(longitude   = double(),
                                latitude    = double(),
                                start_date  = as.Date(character()),
                                end_date    = as.Date(character()),
                                label       = character(),
                                cube        = character(),
                                time_series = list()
    )
    class(result.tb) <- append(class(result.tb), "sits_tibble")
    return(result.tb)
}
