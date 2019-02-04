#' @title Import time series in the zoo format to a sits tibble
#' @name sits_from_zoo
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Converts data from an instance of a zoo series to a sits tibble.
#'
#' @param ts.zoo        Zoo time series.
#' @param longitude     Longitude of the chosen location.
#' @param latitude      Latitude of the chosen location.
#' @param label         Label to attach to the time series (optional).
#' @param name          Name of the coverage where data comes from.
#' @return Time series in sits tibble format.
#'
#' @examples
#' # Read a time series in ZOO format
#' data(ts_zoo)
#' # Convert the zoo series into a sits tibble
#' data.tb <- sits_from_zoo (ts_zoo, longitude = -54.2313, latitude = -14.0482,
#'            label = "Cerrado", name = "mod13q1")
#' @export
sits_from_zoo <- function(ts.zoo, longitude = 0.00, latitude = 0.00, label = "NoClass", name  = "unknown"){
    # convert the data from the zoo format to a tibble used by sits
    ts.tb <- tibble::as_tibble(zoo::fortify.zoo(ts.zoo))
    # create a list to store the zoo time series
    ts.lst <- list()
    # put the time series in the list
    ts.lst[[1]] <- ts.tb

    # get the start date
    start_date <- ts.tb[1, ]$Index
    # get the end date
    end_date <- ts.tb[NROW(ts.tb), ]$Index

    # create a tibble to store the WTSS data
    data.tb <- sits_tibble()
    # add one row to the tibble
    data.tb <- tibble::add_row(data.tb,
                               longitude    = longitude,
                               latitude     = latitude,
                               start_date   = as.Date(start_date),
                               end_date     = as.Date(end_date),
                               label        = label,
                               coverage     = name,
                               time_series  = ts.lst)
    class(data.tb) <- append(class(data.tb), "sits_tibble")

    return(data.tb)
}

#' @title Export data to be used to the zoo format
#' @name sits_to_zoo
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Converts data from a sits tibble to a list of a zoo series.
#'
#' @param  data.tb    A sits tibble with time series.
#' @param  band       Name of the band to be exported (if NULL all bands are exported).
#' @return List of time series in zoo format.
#' @examples
#' # read a tibble with 400 samples of Cerrado and 346 samples of Pasture
#' data(cerrado_2classes)
#' # export a time series to zoo
#' zoo.lst <- sits_to_zoo (cerrado_2classes[1:5,])
#' @export
sits_to_zoo <- function(data.tb, band = NULL){
    zoo.lst <- data.tb$time_series %>%
        purrr::map(function(ts) {
            if (purrr::is_null(band))
                band <-  colnames(ts[-1:0])
            # transform each sits time series to the zoo format
            zoo.ts <- zoo::zoo(ts[, band, drop = FALSE], ts$Index)
            return(zoo.ts)
        })

    return(zoo.lst)
}
