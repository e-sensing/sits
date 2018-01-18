#' @title Provides information about one coverage used to retrieve data
#' @name sits_coverage
#'
#' @description uses the configuration file to print information and save metadata about a
#' chosen coverage:
#'  r_obj          - R object associated with the coverage
#'  coverage       - name of the coverage (must be unique)
#'  service        - name of time series service that provides the coverage (e.g., "WTSS", "SATVEG", "RASTER")
#'  product        - name of the product associated with coverage (e.g., "MOD13Q1")
#'  bands          - list of bands
#'  start_date     - the start date for the time series data in the coverage
#'  end_date       - the end date for the time series data in the coverage
#'  timeline       - the timeline of the coverage
#'  xmin           - spatial extent (xmin)
#'  ymin           - spatial extent (ymin)
#'  xmax           - spatial extent (xmax)
#'  ymax           - spatial extent (ymin)
#'  xres           - spatial resolution (x dimension)
#'  yres           - spatial resolution (y dimension)
#'  crs            - Projection crs
#'  files          - Files associated with the coverage (in the case of service == "RASTER")
#'
#' @param service    the name of the time series service
#' @param product    the name of the product
#' @param name       the name of the coverage
#' @param timeline   timeline of the coverage
#' @param bands      list of bands (for raster data)
#' @param files      file names (for raster data)
#' @examples
#' # Retrieve information about a WTSS coverage
#' coverage.tb <- sits_coverage(service = "WTSS-INPE-1", product = "MOD13Q1", name = "mod13q1_512")
#'
#' # read a raster file and put it into a vector
#' files  <- c(system.file ("extdata/raster/mod13q1/sinop-crop-ndvi.tif", package = "sits"))
#'
#' # define the timeline
#' data(timeline_mod13q1)
#' timeline <- lubridate::as_date (timeline_mod13q1$V1)
#'
#' # create a raster coverage file based on the information about the files
#' raster.tb <- sits_coverage(service = "RASTER", product = "MOD13Q1", name  = "Sinop-crop",
#'              timeline = timeline, bands = c("ndvi"), files = files)
#'
#' @export
#'
sits_coverage <- function(service  = "WTSS-INPE-1",
                          product  = "MOD13Q1",
                          name     = "mod13q1_512",
                          timeline = NULL,
                          bands    = NULL,
                          files    = NA) {
    # pre-condition
    .sits_check_service(service)

    protocol <- .sits_get_protocol(service)

    if (protocol == "WTSS") {
        tryCatch({
            URL  <- .sits_get_server(service)
            # obtains information about the available coverages
            wtss.obj         <- wtss::WTSS(URL)
            # temporal extent
            timeline <- .sits_get_timeline(service, product, name)
            # create a coverage
            coverage.tb <- .sits_coverage_create(wtss.obj, service, product, name, timeline)

        }, error = function(e){
            msg <- paste0("WTSS service not available at URL ", URL)
            .sits_log_error(msg)
            message(msg)
        })
    }

    else if (protocol == "SATVEG") {
        r_obj <- NA
        coverage.tb <- .sits_coverage_create(r_obj, service, product, name, timeline)
    }
    else
        coverage.tb <- .sits_coverage_raster(product, name, timeline, bands, files)

    return(coverage.tb)

}

#' @title Provides information about one coverage of a web time series service
#' @name .sits_coverage_create
#'
#' @description creates a tibble with metadata about a given coverage
#'
#' @param r_obj      the R object associated with the coverage
#' @param service    the time series service
#' @param product    the image product
#' @param name       the name of the coverage
#' @param timeline   (optional) the coverage timeline
#' @param bands      vector with names of bands
#' @param file_names vector of names of raster files where the data is stored
#'
.sits_coverage_create <- function(r_obj    = NA, service, product, name,
                               timeline = NULL, bands = NULL, files = NA) {


    if (purrr::is_null(bands))
            bands <- .sits_get_bands(service = service, product = product)

    # get the timeline
    if (purrr::is_null(timeline))
        timeline <- .sits_get_timeline(service, product, name)

    # get the size of the coverage
    size <- .sits_get_size(service, product, r_obj)
    # get the bounding box of the product
    bbox <- .sits_get_bbox(service, product, r_obj)
    # get the resolution of the product
    res <- .sits_get_resolution(product)
    # get the CRS projection
    crs <- .sits_get_projection(service, product, r_obj)

    # create a tibble to store the metadata
    coverage.tb <- tibble::tibble(r_obj          = list(r_obj),
                                  name           = name,
                                  service        = service,
                                  product        = product,
                                  bands          = list(bands),
                                  start_date     = as.Date(timeline[1]),
                                  end_date       = as.Date(timeline[length(timeline)]),
                                  timeline       = list(timeline),
                                  nrows          = as.integer(size["nrows"]),
                                  ncols          = as.integer(size["ncols"]),
                                  xmin           = as.numeric(bbox["xmin"]),
                                  xmax           = as.numeric(bbox["xmax"]),
                                  ymin           = as.numeric(bbox["ymin"]),
                                  ymax           = as.numeric(bbox["ymax"]),
                                  xres           = as.numeric(res["xres"]),
                                  yres           = as.numeric(res["yres"]),
                                  crs            = crs,
                                  file           = NA)


    return(coverage.tb)
}

#' @title Create a metadata tibble to store the description of a spatio-temporal raster dataset
#' @name .sits_coverage_raster
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description  This function creates a tibble containing the metadata for
#'               a set of spatio-temporal raster files, organized as a set of "Raster Bricks".
#'               These files should be of the same size and
#'               projection. Each raster brick file should contain one band
#'               per time step. Different bands are archived in different raster files.
#'
#' @param  product       The image product where the files are extracted (e.g. MOD13Q1)
#' @param  name         The name of the coverage file
#' @param  timeline      Vector of dates with the timeline of the bands
#' @param  bands         The bands contained in the Raster Brick set (in the same order as the files)
#' @param  files         Vector with the file paths of the raster files
#' @return raster.tb     A tibble with metadata information about a raster data set
#'
.sits_coverage_raster <- function(product = "MOD13Q1", name = NULL, timeline = NULL, bands, files) {

    ensurer::ensure_that(bands, length(.) == length(files),
                         err_desc = "sits_coverageRaster: number of bands does not match number of files")
    ensurer::ensure_that(name, !purrr::is_null(.),
                         err_desc = "sits_coverageRaster: name of the coverega must be provided")
    ensurer::ensure_that(bands, !purrr::is_null(.),
                         err_desc = "sits_coverageRaster - bands must be provided")
    ensurer::ensure_that(files, !purrr::is_null(.),
                         err_desc = "sits_coverageRaster - files must be provided")

    # get the timeline
    if (purrr::is_null(timeline))
        timeline <- .sits_get_timeline(service = "RASTER", product = product, name = name)

    # create a list to store the raster objects

    brick.lst <- purrr::pmap(list(files, bands),
                             function(file, band) {
                                 # create a raster object associated to the file
                                 raster.obj <- raster::brick(file)
                                 # find out how many layers the object has
                                 n_layers   <-  raster.obj@file@nbands
                                 # check that there are as many layers as the length of the timeline
                                 ensurer::ensure_that(n_layers, (.) == length(timeline),
                                                      err_desc = "duration of timeline is not matched by number of layers in raster")
                                 # add the object to the raster object list
                                 return(raster.obj)
                             })
    coverage.tb <- .sits_create_raster_coverage(brick.lst   = brick.lst,
                                                service  = "RASTER",
                                                product  = product,
                                                name     = name,
                                                timeline = timeline,
                                                bands    = bands,
                                                files    = files)

    return(coverage.tb)
}


