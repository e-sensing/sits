#' @title Provides information about one coverage used to retrieve data
#' @name sits_coverage
#'
#' @description uses the configuration file to print information and save metadata about a
#' chosen coverage:
#'  r_obj          - R object associated with the coverage
#'  coverage       - name of the coverage (must be unique)
#'  service        - name of time series service that provides the coverage (e.g., "WTSS", "SATVEG", "RASTER")
#'  product        - name of the product associated with coverage (e.g., "MOD13Q1")
#'  band_info      - tibble with information about the bands (name, scale_factor and missing_value)
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
#' @param coverage   the name of the coverage
#' @param timeline   timeline of the coverage
#' @param bands      list of bands (for raster data)
#' @param files      file names (for raster data)
#' @examples
#' # Retrieve information about a WTSS coverage
#' coverage.tb <- sits_coverage(service = "WTSS", product = "MOD13Q1", coverage = "mod13q1_512")
#' @export
#'
sits_coverage <- function(service  = "WTSS",
                          product  = "MOD13Q1",
                          coverage = "mod13q1_512",
                          timeline = NULL,
                          bands    = NULL,
                          files    = NA) {
    # pre-condition
    .sits_check_service(service)

    if (service == "WTSS")
        coverage.tb <- sits_coverageWTSS(product, coverage)
    else if (service == "SATVEG")
        coverage.tb <- sits_coverageSATVEG(product, coverage, timeline)
    else
        coverage.tb <- sits_coverageRaster(product, coverage, timeline, bands, files)

    return(coverage.tb)

}
#' @title Provides information about one coverage of the WTSS service
#' @name sits_coverageWTSS
#'
#' @description uses the WTSS services to print information and save metadata about a
#' chosen coverage:
#'  bands          - the information about the bands of the data to be retrieved from the WTSS
#'  start_date     - the start date for the time series data in the coverage
#'  end_date       - the end date for the time series data in the coverage
#'  xres           - spatial resolution (x dimension)
#'  yres           - spatial resolution (y dimension)
#'  start_date     - initial date of the coverage time series
#'  end_date       - final date of the coverage time series
#'  xmin           - spatial extent (xmin)
#'  ymin           - spatial extent (ymin)
#'  xmax           - spatial extent (xmax)
#'  ymax           - spatial extent (ymin)
#'  scale_factor   - scale factor to covert bands to a [0..1] scale
#'  crs            - Projection crs
#'
#' @param product    the name of the product
#' @param coverage   the name of the coverage
#' @examples
#' # Retrieve information about a WTSS coverage
#' coverage.tb <- sits_coverageWTSS(coverage = "mod13q1_512")
#' @export
#'
sits_coverageWTSS <- function(product = "MOD13Q1", coverage = "mod13q1_512") {

    tryCatch({
        URL  <- .sits_get_server("WTSS")
        # obtains information about the available coverages
        wtss.obj         <- wtss::WTSS(URL)

        # temporal extent
        timeline <- .sits_get_timeline("WTSS", product, coverage)

        # create a coverage metadata
        coverage.tb <- .sits_coverage_web(r_obj      = wtss.obj,
                                          service    = "WTSS",
                                          product    = product,
                                          coverage   = coverage,
                                          timeline   = timeline)

    }, error = function(e){
        msg <- paste0("WTSS service not available at URL ", URL)
        .sits_log_error(msg)
        message(msg)
    })

    # return the tibble with coverage info
    return(coverage.tb)
}

#' @title Retrieve a coverage name from the SATVEG service
#' @name sits_coverageSATVEG
#' @author Julio Esquerdo, \email{julio.esquerdo@@embrapa.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Retrieves a coverage name based on the capabilities of the SATVEG service
#'
#' @param product            the SATVEG product
#' @param coverage           the name of the coverage ("terra", "aqua", "comb")
#' @param timeline           the timeline for the SATVEG product (optional)
#' @return coverage.tb       metadata about the coverage
#' @examples
#' coverage.tb <- sits_coverageSATVEG(product = "MOD13Q1", coverage = "terra")
#' @export
#'
sits_coverageSATVEG <- function(product = "MOD13Q1", coverage = "terra", timeline = NULL) {

    coverage.tb <- .sits_coverage_web(service = "SATVEG",
                                      product = "MOD13Q1",
                                      coverage = coverage,
                                      timeline = timeline)

    return(coverage.tb)
}

#' @title Provides information about one coverage of a web time series service
#' @name .sits_coverage_web
#'
#' @description creates a tibble with metadata about a given coverage
#'
#' @param r_obj      the R object associated with the coverage
#' @param service    the time series service
#' @param product    the SATVEG product
#' @param coverage   the name of the coverage
#' @param timeline   (optional) the coverage timeline
#' @param bands      vector with names of bands
#' @param file_names vector of names of raster files where the data is stored
#'
.sits_coverage_web <- function(r_obj   = NA,
                               service = "WTSS",
                               product = "MOD13Q1",
                               coverage,
                               timeline = NULL,
                               bands    = NULL,
                               files    = NA) {


    if (purrr::is_null(bands))
            bands <- .sits_get_bands(service = service, product = product)

    # get the timeline
    if (purrr::is_null(timeline))
        timeline <- .sits_get_timeline(service, product, coverage)

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
                                  coverage       = coverage,
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


