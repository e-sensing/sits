#' @title Create a metadata tibble to store the description of a spatio-temporal raster dataset
#' @name sits_STRaster
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description  This function creates a tibble containing the metadata for
#'               a set of spatio-temporal raster files, organized as a set of "Raster Bricks".
#'               These files should be of the same size and
#'               projection. Each raster brick file should contain one band
#'               per time step. Different bands are archived in different raster files.
#'
#' @param  files         Vector with the file paths of the raster files
#' @param  timeline      Vector of dates with the timeline of the bands
#' @param  bands         The bands contained in the Raster Brick set (in the same order as the files)
#' @param  scale_factors Scale factors to convert each band to [0..1] range (in the same order as the files)
#' @return raster.tb   A tibble with metadata information about a raster data set
#'
#'
#' @examples
#' # read a raster file and put it into a vector
#' files  <- c(system.file ("extdata/mod13q1/sinop_ndvi_sample.tif", package = "sits"))
#'
#' # define the timeline
#' timeline <- read.csv(system.file("extdata/mod13q1/timeline.csv", package = "sits"), header = FALSE)
#' timeline <- lubridate::as_date (timeline$V1)
#' # define the bands
#' bands <- c("ndvi")
#' # define the scale factor
#' scale_factors <-  c(0.0001)
#'
#' # create a raster metadata file based on the information about the files
#' raster.tb <- sits_STRaster (files, timeline, bands, scale_factors)
#'
#' @export
sits_STRaster <- function (files, timeline, bands, scale_factors){

    ensurer::ensure_that (bands, length(.) == length(files), err_desc = "number of bands does not match number of files")
    ensurer::ensure_that (scale_factors, length(.) == length(files), err_desc = "scale_factors do not match number of files")

    raster.tb <- purrr::pmap(list(files, bands, scale_factors),
                function (file, band, sf){
                    # create a raster object associated to the file
                    raster.obj <- raster::brick (file)
                    # find out how many layers the object has
                    n_layers    <-  raster.obj@file@nbands
                    # check that there are as many layers as the length of the timeline
                    ensurer::ensure_that(n_layers, (.) == length(timeline),
                                         err_desc = "duration of timeline is not matched by number of layers in raster")

                    row_raster.tb <- .sits_tibble_raster (raster.obj, band, timeline, sf)

                }) %>% dplyr::bind_rows()

    return (raster.tb)
}


