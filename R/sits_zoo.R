#' @title Import time series in the zoo format to a sits tibble
#' @name sits_from_zoo
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Converts data from an instance of a zoo series to a sits tibble.
#'
#' @param ts_zoo        Zoo time series.
#' @param longitude     Longitude of the chosen location.
#' @param latitude      Latitude of the chosen location.
#' @param label         Label to attach to the time series (optional).
#' @param name          Name of the data cube where data comes from.
#' @return Time series in sits tibble format.
#'
#' @examples
#' # Read a time series in ZOO format
#' data(ts_zoo)
#' # Convert the zoo series into a sits tibble
#' data <- sits_from_zoo(ts_zoo,
#'     longitude = -54.2313, latitude = -14.0482,
#'     label = "Cerrado", name = "mod13q1"
#' )
#'
#' @export
#'
sits_from_zoo <- function(ts_zoo, longitude = 0.00, latitude = 0.00,
                          label = "NoClass", name = "unknown") {
    # verifies if zoo package is installed
    if (!requireNamespace("zoo", quietly = TRUE)) {
        stop("zoo needed for this function to work.
              Please install it.", call. = FALSE)
    }
    # preconditions
    assertthat::assert_that(class(ts_zoo) == "zoo",
                            msg = "input is not a zoo time series")
    assertthat::assert_that((longitude >= -180. & longitude <= 180.),
                            msg = "invalid longitude value"
    )
    assertthat::assert_that((latitude >= -90. & longitude <= 90.),
                            msg = "invalid latitudevalue"
    )

    # convert the data from the zoo format to a tibble used by sits
    ts <- tibble::as_tibble(zoo::fortify.zoo(ts_zoo))

    # create a tibble to store the data
    data <- tibble::tibble(
        longitude = longitude,
        latitude = latitude,
        start_date = as.Date(ts[1, ]$Index),
        end_date = as.Date(ts[nrow(ts), ]$Index),
        label = label,
        cube = name,
        time_series = list(ts)
    )

    return(data)
}

#' @title Export data to be used to the zoo format
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
#'
#' @export
#'
sits_to_zoo <- function(data, band = NULL) {
    # verifies if zoo package is installed
    if (!requireNamespace("zoo", quietly = TRUE)) {
        stop("zoo needed for this function to work.
              Please install it.", call. = FALSE)
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
