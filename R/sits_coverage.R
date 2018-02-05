#' @title Provides information about one coverage used to retrieve data
#' @name sits_coverage
#'
#' @description uses the configuration file to print information and save metadata about a
#' chosen coverage:
#'  service        - name of time series service that provides the coverage (e.g., "WTSS", "SATVEG", "RASTER")
#'  name           - name of the coverage (must be unique)
#'  bands          - vector of bands
#'  scale_factor   - vector of scale factors
#'  missing_values - vector of missing values
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
#' @param service           name of the time series service
#' @param name              name of the coverage
#' @param timeline          timeline of the coverage
#' @param bands             bands (only for raster data)
#' @param scale_factors     scale factor (only for raster data)
#' @param missing_values    missing values (only for raster data)
#' @param files             file names (only for raster data)
#' @examples
#' # Retrieve information about a WTSS coverage
#' coverage.tb <- sits_coverage(service = "WTSS-INPE-1", name = "mod13q1_512")
#'
#' # read a raster file and put it into a vector
#' files  <- c(system.file ("extdata/raster/mod13q1/sinop-crop-ndvi.tif", package = "sits"))
#'
#' # define the timeline
#' data(timeline_mod13q1)
#' timeline <- lubridate::as_date (timeline_mod13q1$V1)
#'
#' # create a raster coverage file based on the information about the files
#' raster.tb <- sits_coverage(service = "RASTER", name  = "Sinop-crop",
#'              timeline = timeline, bands = c("ndvi"), files = files)
#'
#' @export
#'
sits_coverage <- function(service  = NULL,
                          name,
                          timeline = NULL,
                          bands    = NULL,
                          missing_values = NULL,
                          scale_factors  = NULL,
                          files    = NA) {

    # if no service is specified, but the names of files are provided,
    # assume we are dealing with raster data
    if (purrr::is_null(service) && all(file.exists(files)))
        service <- "RASTER"
    # pre-condition
    if (all(file.exists(files)) && service != "RASTER") {
        msg <- paste0("inconsitent specification of coverage paramets - files should
            be provided only when service is RASTER")
        .sits_log_error(msg)
        message(msg)
        return(NULL)
    }
    # pre-condition
    .sits_check_service(service)
    # get the protocol associated with the service
    protocol <- .sits_get_protocol(service)

    if (protocol == "WTSS") {
        tryCatch({
            URL  <- .sits_get_server(service)
            # obtains information about the available coverages
            wtss.obj         <- wtss::WTSS(URL)
            # create a coverage
            coverage.tb <- .sits_coverageWTSS(wtss.obj, service, name)

        }, error = function(e){
            msg <- paste0("WTSS service not available at URL ", URL)
            .sits_log_error(msg)
            message(msg)
        })
    }

    else if (protocol == "SATVEG") {
        coverage.tb <- .sits_coverage_SATVEG(name, timeline)
    }
    else
        coverage.tb <- .sits_coverage_raster(name = name,
                                             timeline = timeline,
                                             bands = bands,
                                             scale_factors = scale_factors,
                                             missing_values = missing_values,
                                             files = files)

    return(coverage.tb)

}
#' @title Provides information about one coverage of the WTSS service
#' @name .sits_coverageWTSS
#'
#' @description uses the WTSS services to print information and save metadata about a
#' chosen coverage
#'
#' @param wtss.obj   R WTSS object associated to the service
#' @param service    name of the service
#' @param name       name of the coverage
#'
.sits_coverageWTSS <- function(wtss.obj, service, name) {

    # obtains information about the available coverages
    coverages.vec    <- wtss::listCoverages(wtss.obj)

    # is the coverage in the list of coverages?
    ensurer::ensure_that(name, (.) %in% coverages.vec,
                             err_desc = ".sits_coverageWTSS: coverage is not available in the WTSS server")

    # describe the coverage
    cov.lst    <- wtss::describeCoverage(wtss.obj, name)
    cov        <- cov.lst[[name]]

    # temporal extent
    timeline <- cov$timeline

    # retrieve information about the bands
    band_info <- cov$attributes

    attr <- as.data.frame(band_info)
    bands <- as.vector(attr[,"name"])
    missing_values <- as.vector(attr[,"missing_values"])
    names(missing_values) <- bands
    scale_factors  <- as.vector(attr[,"scale_factor"])
    names(scale_factors) <- bands

    # Spatial extent
    xmin <- cov$spatial_extent$xmin
    ymin <- cov$spatial_extent$ymin
    xmax <- cov$spatial_extent$xmax
    ymax <- cov$spatial_extent$ymax

    # Spatial resolution
    xres <- cov$spatial_resolution$x
    yres <- cov$spatial_resolution$y

    # Size (rows and cols)
    nrows <- cov$dimension$y$max_idx - cov$dimensions$y$min_idx + 1
    ncols <- cov$dimension$x$max_idx - cov$dimensions$x$min_idx + 1

    # Projection CRS
    crs <- cov$crs$proj4

    # create a tibble to store the metadata
    coverage.tb <- tibble::tibble(r_obj          = list(wtss.obj),
                                  name           = name,
                                  service        = service,
                                  bands          = list(bands),
                                  scale_factors  = list(scale_factors),
                                  missing_values = list(missing_values),
                                  start_date     = as.Date(timeline[1]),
                                  end_date       = as.Date(timeline[length(timeline)]),
                                  timeline       = list(timeline),
                                  nrows          = nrows,
                                  ncols          = ncols,
                                  xmin           = xmin,
                                  xmax           = xmax,
                                  ymin           = ymin,
                                  ymax           = ymax,
                                  xres           = xres,
                                  yres           = yres,
                                  crs            = crs,
                                  files          = NA)

    # return the tibble with coverage info
    return(coverage.tb)
}

#' @title Provides information about one coverage of a web time series service
#' @name .sits_coverage_SATVEG
#'
#' @description creates a tibble with metadata about a given coverage
#'
#' @param name       the name of the coverage
#' @param timeline   timeline of the coverage
#'
.sits_coverage_SATVEG <- function(name, timeline) {

    service <- "SATVEG"
    # get the bands
    bands <- .sits_get_bands(service, name)

    # get the timeline
    if (purrr::is_null(timeline))
        timeline <- .sits_SATVEG_timeline()

    # get the size of the coverage
    size <- .sits_get_size(service, name)
    # get the bounding box of the coverage
    bbox <- .sits_get_bbox(service, name)
    # get the resolution of the product
    res <- .sits_get_resolution(service, name)
    # get the CRS projection
    crs <- .sits_get_projection(service, name)

    scale_factors <- .sits_get_scale_factors(service, name, bands)

    missing_values <- .sits_get_missing_values(service, name, bands)

    # create a tibble to store the metadata
    coverage.tb <- tibble::tibble(r_obj          = NA,
                                  name           = name,
                                  service        = service,
                                  bands          = list(bands),
                                  scale_factors  = list(scale_factors),
                                  missing_values = list(missing_values),
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
                                  files          = NA)


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
#' @param  name         The name of the coverage file
#' @param  timeline      Vector of dates with the timeline of the bands
#' @param  bands         The bands contained in the Raster Brick set (in the same order as the files)
#' @param  scale_factors     vector of scale factors (one per band)
#' @param  missing_values    vector of missing values (one per band)
#' @param  files         Vector with the file paths of the raster files
#' @return raster.tb     A tibble with metadata information about a raster data set
#'
.sits_coverage_raster <- function(name, timeline, bands,
                                  scale_factors, missing_values, files) {

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
        timeline <- .sits_get_timeline(service = "RASTER", name = name)

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

    coverage.tb <- .sits_create_raster_coverage(brick.lst      = brick.lst,
                                                service        = "RASTER",
                                                name           = name,
                                                timeline       = timeline,
                                                bands          = bands,
                                                scale_factors  = scale_factors,
                                                missing_values = missing_values,
                                                files          = files)

    return(coverage.tb)
}


