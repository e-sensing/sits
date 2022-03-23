#' @title Export time series to zoo format
#' @name sits_to_zoo
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Converts data from a sits tibble to a list of a zoo series.
#'
#' @param  data       A sits tibble with time series.
#' @param  band       Band to be exported (if NULL all bands are exported).
#' @return List of time series in zoo format.
#' @examples
#' # read a tibble with 400 samples of Cerrado and 346 samples of Pasture
#' data(cerrado_2classes)
#' # export a time series to zoo
#' zoo.lst <- sits_to_zoo(cerrado_2classes[1:5, ])
#' @export
#'
sits_to_zoo <- function(data, band = NULL) {
    # verifies if zoo package is installed
    if (!requireNamespace("zoo", quietly = TRUE)) {
        stop("Please install package zoo", call. = FALSE)
    }
    zoo_lst <- data$time_series %>%
        purrr::map(function(ts) {
            if (purrr::is_null(band)) {
                band <- colnames(ts[-1:0])
            }
            # transform each sits time series to the zoo format
            ts_zoo <- zoo::zoo(ts[, band, drop = FALSE], ts$Index)
            return(ts_zoo)
        })

    return(zoo_lst)
}
