#' @title Create a metadata tibble to store the description of a spatio-temporal raster dataset
#' @name sits_coverageRaster
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description  This function creates a tibble containing the metadata for
#'               a set of spatio-temporal raster files, organized as a set of "Raster Bricks".
#'               These files should be of the same size and
#'               projection. Each raster brick file should contain one band
#'               per time step. Different bands are archived in different raster files.
#'
#' @param  files         Vector with the file paths of the raster files
#' @param  product       The image product where the files are extracted (e.g. MOD13Q1)
#' @param  name          The name of the coverage file
#' @param  timeline      Vector of dates with the timeline of the bands
#' @param  bands         The bands contained in the Raster Brick set (in the same order as the files)
#' @param  scale_factors Scale factors to convert each band to [0..1] range (in the same order as the files)
#' @return raster.tb   A tibble with metadata information about a raster data set
#'
#'
#' @examples
#' # read a raster file and put it into a vector
#' files  <- c(system.file ("extdata/raster/mod13q1/sinop-crop-ndvi.tif", package = "sits"))
#'
#' # define the timeline
#' data(timeline_mod13q1)
#' timeline <- lubridate::as_date (timeline_mod13q1$V1)
#'
#' # create a raster metadata file based on the information about the files
#' raster.tb <- sits_coverageRaster (files, timeline, bands = c("ndvi"), scale_factors = c(0.0001))
#'
#' @export
sits_coverageRaster <- function(files, product = "MOD13Q1", name = NULL, timeline, bands = NULL) {

    ensurer::ensure_that(bands, length(.) == length(files),
                         err_desc = "sits_coverageRaster: number of bands does not match number of files")
    ensurer::ensure_that(name, !purrr::is_null(.),
                         err_desc = "sits_coverageRaster: name of the image must be provided")

    # create a list to store the raster objects
    raster.lst <- list()

    raster.tb <- purrr::pmap(list(files, bands),
                function(file, band) {
                    # create a raster object associated to the file
                    raster.obj <- raster::brick(file)
                    # find out how many layers the object has
                    n_layers   <-  raster.obj@file@nbands
                    # check that there are as many layers as the length of the timeline
                    ensurer::ensure_that(n_layers, (.) == length(timeline),
                                         err_desc = "duration of timeline is not matched by number of layers in raster")
                    # add the object to the raster object list
                    raster.lst[[length(raster.lst) + 1]] <- raster.obj
                })
    raster.tb <- .sits_tibble_raster(raster.lst, name, bands, timeline, product, files)

    return(raster.tb)
}
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
#' @param coverage        string - the name of the coverage to be retrieved
#' @return data.tb        a SITS tibble with the time series
#'
#' @examples
#'
#' #' # Read a point in a Raster Brick
#' # define the file that has the raster brick
#' files  <- c(system.file ("extdata/raster/mod13q1/sinop-crop-ndvi.tif", package = "sits"))
#' # select the bands
#' bands <- c("ndvi")
#' # define the scale factors
#' scale_factors <- c(0.0001)
#' # define the timeline
#' data(timeline_mod13q1)
#' timeline <- lubridate::as_date (timeline_mod13q1$V1)
#' # create a raster metadata file based on the information about the files
#' raster.tb <- sits_STRaster (files, timeline, bands, scale_factors)
#' # read the point from the raster
#' point.tb <- sits_fromRaster(raster.tb, longitude = -55.50563, latitude = -11.71557)
#'
#' @export
sits_fromRaster <- function(raster.tb,
                            file = NULL,
                            longitude = NULL,
                            latitude = NULL,
                            start_date = NULL,
                            end_date  = NULL,
                            label = "NoClass",
                            coverage = NULL){

    # ensure metadata tibble exists
    .sits_test_tibble(raster.tb)

    # get data based on CSV file
    if (!purrr::is_null(file) && tolower(tools::file_ext(file)) == "csv") {
        data.tb <- .sits_ts_fromRasterCSV(raster.tb, file)
    }

    if (!purrr::is_null(longitude) && !purrr::is_null(latitude)) {
        xy <- .sits_latlong_to_proj(longitude, latitude, raster.tb[1, ]$crs)
        data.tb <- .sits_ts_fromRasterXY(raster.tb, xy, longitude, latitude, label, coverage)
    }
    return(data.tb)
}

