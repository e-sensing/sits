
#' @title Extract a time series from a ST raster data set
#' @name sits_fromRaster
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Reads metadata about a raster data set to retrieve a set of
#' time series.
#'
#' @param raster.tb       A tibble with metadata describing a spatio-temporal data set
#' @param file            A CSV file with lat/long locations to be retrieve
#' @param longitude       double - the longitude of the chosen location
#' @param latitude        double - the latitude of the chosen location
#' @param start_date      date - the start of the period
#' @param end_date        date - the end of the period
#' @param label           string - the label to attach to the time series
#' @return data.tb        a SITS tibble with the time series
#'
#' @examples
#'
#' #' # Read a point in a Raster Brick
#' # define the file that has the raster brick
#' files  <- c(system.file ("extdata/raster/mod13q1/sinop-crop-ndvi.tif", package = "sits"))
#' # select the bands
#' bands <- c("ndvi")
#' # define the timeline
#' data(timeline_mod13q1)
#' timeline <- lubridate::as_date (timeline_mod13q1$V1)
#' # create a raster metadata file based on the information about the files
#' raster.tb <- sits_coverageRaster(product = "MOD13Q1", coverage = "Sinop-Crop",
#'              timeline = timeline, bands = c("ndvi"), files = files)
#' # read the point from the raster
#' point.tb <- sits_fromRaster(raster.tb, longitude = -55.55502, latitude = -11.52774)
#'
#' @export
sits_fromRaster <- function(raster.tb,
                            file = NULL,
                            longitude = NULL,
                            latitude = NULL,
                            start_date = NULL,
                            end_date  = NULL,
                            label = "NoClass"){

    # ensure metadata tibble exists
    ensurer::ensure_that(raster.tb, NROW(.) == 1,
                         err_desc = "sits_classify_raster: need a valid metadata for coverage")

    # get data based on CSV file
    if (!purrr::is_null(file) && tolower(tools::file_ext(file)) == "csv") {
        data.tb <- .sits_ts_fromRasterCSV(raster.tb, file)
    }

    if (!purrr::is_null(longitude) && !purrr::is_null(latitude)) {
        xy <- .sits_latlong_to_proj(longitude, latitude, raster.tb[1, ]$crs)
        data.tb <- .sits_ts_fromRasterXY(raster.tb, xy, longitude, latitude, label)
    }
    return(data.tb)
}

