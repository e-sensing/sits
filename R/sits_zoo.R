#' @title Import time series in the zoo format to a SITS tibble
#' @name sits_fromZOO
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Converts data from an instance of a zoo series to a SITS tibble
#'
#' @param ts.zoo        A zoo time series
#' @param longitude     Longitude of the chosen location
#' @param latitude      Latitude of the chosen location
#' @param label         Label to attach to the time series (optional)
#' @param name          Name of the coverage where data comes from
#' @return data.tb      A time series in SITS tibble format
#'
#' @examples
#' # Read a time series in ZOO format
#' data(ts_zoo)
#' # Convert the zoo series into a SITS tibble
#' data.tb <- sits_fromZOO (ts_zoo, longitude = -54.2313, latitude = -14.0482,
#'            label = "Cerrado", name = "mod13q1")
#' @export
sits_fromZOO <- function(ts.zoo, longitude = 0.00, latitude = 0.00, label = "NoClass", name  = "unknown"){

    # convert the data from the zoo format to a tibble used by SITS
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
    class(data.tb) <- append(class(data.tb), "sits")

    return(data.tb)
}

#' @title Export data to be used to the zoo format
#' @name sits_toZOO
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Converts data from a SITS tibble to a list of a zoo series.
#'
#' @param  data.tb    a SITS time series
#' @param  band       the name of the band to be exported (if NULL all bands are exported)
#' @return zoo.lst    a list of time series in zoo format
#' @examples
#'
#' # read a tibble with 400 samples of Cerrado and 346 samples of Pasture
#' data(cerrado_2classes)
#' # export a time series to zoo
#' zoo.lst <- sits_toZOO (cerrado_2classes[1:5,])
#'
#' @export
sits_toZOO <- function(data.tb, band = NULL){

    zoo.tb <- data.tb %>%
        purrrlyr::by_row(function(row) {
            ts.tb <- row$time_series[[1]]
            if (purrr::is_null(band))
                band <-  colnames(ts.tb[-1:0])
            # transform each sits time series to the zoo format
            zoo.ts <- zoo::zoo(ts.tb[, band, drop = FALSE], ts.tb$Index)
            return(zoo.ts)
        }, .to = "zoo"
        )

    return(zoo.tb$zoo)
}



