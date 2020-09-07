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
#' @param name          Name of the data cube where data comes from.
#' @return Time series in sits tibble format.
#'
#' @examples
#' # Read a time series in ZOO format
#' data(ts_zoo)
#' # Convert the zoo series into a sits tibble
#' data <- sits_from_zoo (ts_zoo, longitude = -54.2313, latitude = -14.0482,
#'            label = "Cerrado", name = "mod13q1")
#' @export
sits_from_zoo <- function(ts.zoo, longitude = 0.00, latitude = 0.00,
                          label = "NoClass", name  = "unknown"){
    # verifies if zoo package is installed
    if (!requireNamespace("zoo", quietly = TRUE)) {
        stop("zoo needed for this function to work.
              Please install it.", call. = FALSE)
    }
    # preconditions
    assertthat::assert_that(class(ts.zoo) == "zoo", msg = "input is not a zoo time series")
    assertthat::assert_that((longitude >= -180. & longitude <= 180.),
                            msg = "invalid longitude value")
    assertthat::assert_that((latitude >= -90. & longitude <= 90.),
                            msg = "invalid latitudevalue")

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

    # create a tibble to store the data
    data <- .sits_tibble()
    # add one row to the tibble
    data    <- tibble::add_row(data,
                               longitude    = longitude,
                               latitude     = latitude,
                               start_date   = as.Date(start_date),
                               end_date     = as.Date(end_date),
                               label        = label,
                               cube         = name,
                               time_series  = ts.lst)

    return(data)
}

#' @title Export data to be used to the zoo format
#' @name sits_to_zoo
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Converts data from a sits tibble to a list of a zoo series.
#'
#' @param  data       A sits tibble with time series.
#' @param  band       Name of the band to be exported (if NULL all bands are exported).
#' @return List of time series in zoo format.
#' @examples
#' # read a tibble with 400 samples of Cerrado and 346 samples of Pasture
#' data(cerrado_2classes)
#' # export a time series to zoo
#' zoo.lst <- sits_to_zoo (cerrado_2classes[1:5,])
#' @export
sits_to_zoo <- function(data, band = NULL){
    # verifies if zoo package is installed
    if (!requireNamespace("zoo", quietly = TRUE)) {
        stop("zoo needed for this function to work.
              Please install it.", call. = FALSE)
    }
    zoo.lst <- data$time_series %>%
        purrr::map(function(ts) {
            if (purrr::is_null(band))
                band <-  colnames(ts[-1:0])
            # transform each sits time series to the zoo format
            zoo.ts <- zoo::zoo(ts[, band, drop = FALSE], ts$Index)
            return(zoo.ts)
        })

    return(zoo.lst)
}
#' @title Import a matrix to a sits tibble
#' @name sits_from_matrix
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param values         matrix of values extracted from a raster object
#' @param raster_obj     raster object from which the matrix of values have been extracted
#' @param timeline       timeline associated to the data
#' @param initial_row    initial row in the raster object from which the matrix has been extracted
#' @param nrows          number of rows read in the raster object
#' @param band           spectral band or index associated to the data
#'
#' @export
#'
sits_from_matrix <- function (values, raster_obj, timeline, initial_row, nrows, band) {
    # preconditions
    assertthat::assert_that(length(values) > 0, msg = "no values in matrix")
    assertthat::assert_that("matrix" %in% class(values), msg = "input values is not a matrix")
    assertthat::assert_that("Date" %in% class(timeline), msg = "invalid timeline")
    assertthat::assert_that(length(timeline) == ncol(values),
                            msg = "length of timeline does not match input matrix")
    assertthat::assert_that(attributes(class(raster_obj))$package == "raster",
                            msg = "raster_obj variable is not a raster")
    assertthat::assert_that(initial_row <= raster::nrow(raster_obj),
                            msg = "invalid row number")

    start_date <- timeline[1]
    end_date   <- timeline[length(timeline)]

    # create a tibble to store the data
    data <- .sits_tibble()
    # create a list of lat/long values
    for (i in 1:nrows) {
        for (j in 1:ncol(raster_obj)){
            row <- initial_row + i - 1
            c   <- raster::cellFromRowCol(raster_obj, row, i)
            xy  <- raster::xyFromCell(raster_obj, c)
            ll  <- .sits_proj_to_latlong(xy[,"x"], xy[,"y"],
                                as.character(suppressWarnings(raster::crs(raster_obj))))
            data <- tibble::add_row(data,
                                    longitude    = ll[,"longitude"],
                                    latitude     = ll[,"latitude"],
                                    start_date   = start_date,
                                    end_date     = end_date,
                                    label        = "NoClass",
                                    cube         = "matrix",
                                    time_series  = list(tibble::tibble(Index = timeline,
                                                                       X1 = values[row,])))
        }
    }
    data <- sits_rename(data, names = band)

    return(data)

}
