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
#' @param coverage   the name of the coverage
#' @param product    the name of the product
#' @param .show      show information about the coverage (Default: TRUE)
#' @examples
#' # Retrieve information about a WTSS coverage
#' coverage.tb <- sits_coverageWTSS("mod13q1_512")
#' @export
#'
sits_coverageWTSS <- function(coverage, product = "MOD13Q1", .show = TRUE) {

    # load the configuration file
    if (purrr::is_null(sits.env$config))
        sits_config()

    # tests if the coverage is available in the global list of coverages
    # if it does not exits, create an empty one to hold the metadata about the coverages
    coverage.tb <- .sits_get_coverage("WTSS", coverage)
    if (!purrr::is_null(coverage.tb))
        return(coverage.tb)

    # obtains information about the WTSS service
    URL <- .sits_get_server("WTSS")
    tryCatch({
        # create a WTSS object
        wtss.obj         <- wtss::WTSS(URL)

        # obtains information about the available coverages
        coverages.vec    <- wtss::listCoverages(wtss.obj)

        # is the coverage in the list of coverages?
        ensurer::ensure_that(coverage, (.) %in% coverages.vec,
                             err_desc = "sits_coverageWTSS: coverage is not available in the WTSS server")

        # describe the coverage
        cov.lst    <- wtss::describeCoverage(wtss.obj, coverage)
        cov        <- cov.lst[[coverage]]

        # temporal extent
        timeline <- cov$timeline

        # create a coverage metadata
        coverage.tb <- .sits_coverage_metadata(coverage, "WTSS", product, timeline)

        # if asked, show the coverage attributes
        if (.show)
            .sits_print_coverage_attrs(cov)

    }, error = function(e){
        msg <- paste0("WTSS service not available at URL ", URL)
        log4r::error(sits.env$logger, msg)
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
#' @param coverage           the name of the coverage ("terra", "aqua", "comb")
#' @param bands              the bands available in SATVEG
#' @param product            the SATVEG product
#' @param timeline           the timeline for the SATVEG product (optional)
#' @return coverage.tb       metadata about the coverage
#' @export
#'
sits_coverageSATVEG <- function(coverage, bands = c("ndvi", "evi"), product = "MOD13Q1", timeline = NULL) {

    # load the configuration file
    if (purrr::is_null(sits.env$config))
        sits_config()

    # tests if the coverage is available in the global list of coverages
    # if it does not exits, create an empty one to hold the metadata about the coverages
    coverage.tb <- .sits_get_coverage("WTSS", coverage)
    if (!purrr::is_null(coverage.tb))
        return(coverage.tb)

    coverage.tb <- .sits_coverage_metadata(coverage, "SATVEG", product, timeline)

    return(coverage)
}
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
#' raster.tb <- sits_coverageRaster(files, name = "Sinop-crop",
#'              timeline = timeline, bands = c("ndvi"))
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
#' @title Provides information about one coverage of the WTSS service
#' @name sits_coverage_metadata
#'
#' @description creates a tibble with metadata about a given coverage
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
#' @param coverage   the name of the coverage
#' @param service    the name of the time series service
#' @param product    the name of the product
#' @param r.objs     the list of the R objects associated with the coverage
#' @param timeline   (optional) the coverage timeline
#' @param file_name  name of raster files where the data is stored
#'
.sits_coverage_metadata <- function(coverage, service = "WTSS", product = "MOD13Q1",  r.objs = NA, timeline = NULL, file_names = NA) {

    if (purrr::is_null(bands))
        bands <- .sits_get_bands("WTSS", product)

    band_info <- sits_band_info(bands, product)

    if (purrr::is_null(timeline))
        timeline <- .sits_get_timeline(product)

    size <- .sits_get_size(service, product)

    bbox <- .sits_get_bbox(service, product)

    res <- .sits_get_resolution(service, product)

    crs <- .sits_get_projection(service, product)

    # create a tibble to store the metadata
    coverage.tb <- tibble::tibble(
        r.objs         = r.objs,
        coverage       = coverage,
        service        = service,
        product        = product,
        band_info      = list(band_info),
        start_date     = as.Date(timeline[1]),
        end_date       = as.Date(timeline[length(timeline)]),
        timeline       = list(timeline),
        nrows          = size["nrows"],
        ncols          = size["ncols"],
        xmin           = bbox["xmin"],
        xmax           = bbox["xmax"],
        ymin           = bbox["ymin"],
        ymax           = bbox["ymax"],
        xres           = res["xres"],
        yres           = res["yres"],
        crs            = crs,
        file_names     = list(file_names))

    .sits_add_coverage(coverage.tb)

    retrun (coverage.tb)
}
