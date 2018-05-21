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
#' @param timeline          vector - timeline of the coverage
#' @param bands             vector - bands
#' @param scale_factors     vector - scale factor for each band
#' @param missing_values    vector - missing values for each band
#' @param minimum_values    vector - minimum values for each band
#' @param files             vector - file names for each band (only for raster data)
#' @examples
#' \donttest{
#' # Example 1. Retrieve information about a WTSS coverage
#' coverage.tb <- sits_coverage(service = "WTSS-INPE", name = "MOD13Q1")
#'
#' # Example 2. Create a raster coverage with metadata
#' # read a raster file and put it into a vector
#' files  <- c(system.file ("extdata/raster/mod13q1/sinop-crop-ndvi.tif", package = "sits"))
#'
#' # define the timeline
#' data(timeline_modis_392)
#'
#' # create a raster coverage file based on the information about the files
#' raster.tb <- sits_coverage(service = "RASTER", name  = "Sinop-crop",
#'              timeline = timeline_modis_392, bands = c("ndvi"), files = files)
#' }
#' @export
#'
sits_coverage <- function(service        = "RASTER",
                          name,
                          timeline       = NULL,
                          bands          = NULL,
                          missing_values = NULL,
                          scale_factors  = NULL,
                          minimum_values = NULL,
                          files          = NA) {

    # if no service is specified, but the names of files are provided,
    # assume we are dealing with raster data
    if (service == "RASTER") {
        r <- suppressWarnings(rgdal::GDALinfo(files, silent = FALSE))
        ensurer::ensure_that(r, all(!purrr::is_null(.)),
                                        err_desc = "sits_coverage: raster files cannot be accessed")
    }

    # pre-condition
    if (any(!is.na(files))) {
        if (all(file.exists(files)) && service != "RASTER") {
            msg <- paste0("inconsistent specification of coverage parameters - files should
                          be provided only when service is RASTER")
            .sits_log_error(msg)
            message(msg)
            return(NULL)
        }
    }

    # pre-condition
    .sits_check_service(service)
    # get the protocol associated with the service
    protocol <- .sits_get_protocol(service)

    if (protocol == "WTSS") {
        tryCatch({
            URL  <- .sits_get_server(service)
            # obtains information about the available coverages
            wtss.obj   <- wtss::WTSS(URL)

        }, error = function(e){
            msg <- paste0("WTSS service not available at URL ", URL)
            .sits_log_error(msg)
            message(msg)
        })

        # create a coverage
        coverage.tb <- .sits_coverageWTSS(wtss.obj, service, name)
    }

    else if (protocol == "SATVEG") {
        coverage.tb <- .sits_coverage_SATVEG(name, timeline)
    }
    else
        coverage.tb <- .sits_coverage_raster(name = name,
                                             timeline.vec       = timeline,
                                             bands.vec          = bands,
                                             scale_factors.vec  = scale_factors,
                                             missing_values.vec = missing_values,
                                             minimum_values.vec = minimum_values,
                                             files.vec          = files)

    return(coverage.tb)

}

#' @title Creates a coverage metadata
#' @name .sits_create_coverage
#'
#' @description uses the configuration file to print information and save metadata about a
#' chosen coverage
#'
#'  service        - name of time series service that provides the coverage (e.g., "WTSS", "SATVEG", "RASTER")
#'  name           - name of the coverage (must be unique)
#'  bands          - vector of bands
#'  scale_factor   - vector of scale factors
#'  missing_values - vector of missing values
#'  start_date     - the start date for the time series data in the coverage
#'  end_date       - the end date for the time series data in the coverage
#'  timeline       - the timeline of the coverage
#'
#'  crs            - Projection crs
#'  files          - Files associated with the coverage (in the case of service == "RASTER")
#'
#' @param r_obs.lst          list of raster objects contained in the coverage
#' @param name               name of the coverage
#' @param service            name of the time series service
#' @param bands.vec          vector with the names of the bands
#' @param labels.vec         vector with labels (only valid for classified data)
#' @param scale_factors.vec  vector with scale factor for each band
#' @param missing_values.vec vector with missing values for each band
#' @param minimum_values.vec vector with minimum values for each band
#' @param timeline.lst       list with vectors of valid timelines for each band
#' @param nrows              number of rows in the coverage
#' @param ncols              number of columns in the coverage
#' @param xmin               spatial extent (xmin)
#' @param ymin               spatial extent (ymin)
#' @param xmax               spatial extent (xmax)
#' @param ymax               spatial extent (ymin)
#' @param xres               spatial resolution (x dimension)
#' @param yres               spatial resolution (y dimension)
#' @param crs                CRS for coverage
#' @param files.vec          vector with associated files
.sits_create_coverage <- function (r_objs.lst, name, service,
                                   bands.vec, labels.vec, scale_factors.vec,
                                   missing_values.vec, minimum_values.vec, timeline.lst,
                                   nrows, ncols, xmin, xmax, ymin, ymax,
                                   xres, yres, crs, files.vec) {

    # create a tibble to store the metadata
    coverage.tb <- tibble::tibble(r_objs         = list(r_objs.lst),
                                  name           = name,
                                  service        = service,
                                  bands          = list(bands.vec),
                                  labels         = list(labels.vec),
                                  scale_factors  = list(scale_factors.vec),
                                  missing_values = list(missing_values.vec),
                                  minimum_values = list(minimum_values.vec),
                                  timeline       = list(timeline.lst),
                                  nrows          = nrows,
                                  ncols          = ncols,
                                  xmin           = xmin,
                                  xmax           = xmax,
                                  ymin           = ymin,
                                  ymax           = ymax,
                                  xres           = xres,
                                  yres           = yres,
                                  crs            = crs,
                                  files          = list(files.vec))


    return(coverage.tb)
}
