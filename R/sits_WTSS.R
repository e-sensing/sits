
#
#' Set variables for WTSS access
#'
#' \code{sits_infoWTSS} obtains information about the WTSS server
#' and about the coverages. This information is used in many applications
#' which obtain data from the WTSS
#'
#' The Web Time Series Service is a lightweight web service the allow remote access to satellite
#'  image time series and provides three operations:
#'
#'  1. list coverages: this operation allows clients to retrieve the capabilities provided
#'     by any server that implements WTSS. It returns a list of coverage
#'     names available in a server instance.
#'
#'  2. describe coverage: this operation returns the metadata for a given coverage
#'     identified by its name. It includes its range in the spatial and temporal dimensions.
#'
#'  3. time series: this operation requests the time series of values of a coverage attribute
#'    at a given location.
#'
#'
#' @param URL             the URL for the WTSS time series service
#' @return wtss_info       a list containing the information about the WTSS server and coverage
#' @export
sits_infoWTSS <- function (URL      = "http://www.dpi.inpe.br/tws/wtss") {

     # obtains information about the WTSS service
     wtss.obj         <- wtss.R::WTSS(URL)
     cat (paste ("-----------------------------------------------------------", "\n",sep = ""))
     cat (paste ("The WTSS server URL is ", wtss.obj@serverUrl, "\n", sep = ""))

     # obtains information about the coverages
     coverages.obj    <- wtss.R::listCoverages(wtss.obj)
          cat (paste ("Available coverages: \n"))
          coverages.obj %>%
               purrr::map (function (c) cat (paste (c, "\n", sep = "")))
          cat (paste ("------------------------------------------------------------", "\n",sep = ""))

     return (invisible(wtss.obj))
}
#
#' Provides information about one coverage of the WTSS service
#'
#' \code{sits_coverageWTSS} uses the WTSS services to print information about a
#' chosen coverage:
#'
#'  start_date     - the start date for the time series data in the coverage
#'  end_date       - the end date for the time series data in the coverage
#'  res_x          - spatial resolution (x dimension)
#'  res_y          - spatial resolution (y dimension)
#'  start_date     - initial date of the coverage time series
#'  end_date       - final date of the coverage time series
#'  x_min          - spatial extent (xmin)
#'  y_min          - spatial extent (ymin)
#'  x_max          - spatial extent (xmax)
#'  y_max          - spatial extent (ymin)
#'  bands          - the bands of the data to be retrieved from the WTSS
#'
#' @param URL        the URL for the WTSS time series service
#' @param coverage   the name of the coverage
#' @export
#'
sits_coverageWTSS <- function (URL = "http://www.dpi.inpe.br/tws/wtss", coverage = NULL) {
     # obtains information about the WTSS service
     wtss.obj         <- wtss.R::WTSS(URL)
     # obtains information about the coverages
     coverages.vec    <- wtss.R::listCoverages(wtss.obj)
     # is the coverage in the list of coverages?
     ensurer::ensure_that (coverage, . %in%coverages.vec,
                           err_desc = "sits_coverageWTSS: coverage is not available in the WTSS server")
     # describe the coverage
     cov.lst    <- wtss.R::describeCoverage(wtss.obj, coverage)
     cov <- cov.lst[[coverage]]

     # name, description and source of coverage
     cat (paste ("----------------------------------------------------------------------------------", "\n",sep = ""))
     cat (paste ("Coverage: ", cov$name, "\n",sep = ""))
     cat (paste ("Description: ", cov$description, "\n", sep = ""))
     cat (paste ("Source: ", cov$detail, "\n", sep = ""))

     # information about the bands
     cat (paste ("Bands: ", "\n", sep = "", collapse=""))
     attr <- as.data.frame(cov$attributes)
     print (attr[1:2])

     # spatial extent and resolution, projection CRS
     cat (paste ("\nSpatial extent: ", "(",
                 cov$spatial_extent$xmin, ", ",
                 cov$spatial_extent$ymin, ") - (",
                 cov$spatial_extent$xmax, ", ",
                 cov$spatial_extent$ymax, ")", sep =""))
     cat (paste ("\nSpatial resolution: ", "(",
                 cov$spatial_resolution$x, ", ",
                 cov$spatial_resolution$y, ")", sep = ""))
     cat (paste ("\nProjection CRS: ", cov$crs$proj4, sep = ""))

     # temporal extent
     timeline <- cov$timeline
     start <- timeline[1]
     end <- timeline[length(timeline)]
     cat (paste ("\nTime range: ", start, " to ", end, "\n", sep = ""))

     # temporal resolution is approximate, taken as the difference between first and second date
     temporal_resolution <- as.integer ((lubridate::as_date(timeline[2])
          - lubridate::as_date(timeline[1]))/lubridate::ddays(1))
     cat (paste ("Temporal resolution: ", temporal_resolution, " days ", "\n", sep = ""))
     cat (paste ("----------------------------------------------------------------------------------", "\n",sep = ""))

     return (invisible(cov))
}
#
#' Provides information about one coverage of the WTSS service
#'
#' \code{sits_getcovWTSS} uses the WTSS services to retrieve information about a given coverage:
#'
#'  name            - the name of thee coverage
#'  description     - description
#'  detail          - more information (source)
#'  attributes      - spectral bands (name, description, datatype, valid_range.min, valid_range.max, scale_factor, missing_value)
#'  spatial_extent  - bounding box in space (xmin, ymin, xmax, ymax)
#'  crs             - Projection CRS
#'  timeline        - dates of images contained in the coverage
#'
#' @param URL        the URL for the WTSS time series service
#' @param coverage   the name of the coverage
#' @return
#' @export
#'
sits_getcovWTSS <- function (URL = "http://www.dpi.inpe.br/tws/wtss", coverage = NULL) {
     # is the coverage name provided?
     ensurer::ensure_that(coverage, !purrr::is_null (.), err_desc = "sits_getcovWTSS: Coverage name must be provided")

     # obtains information about the WTSS service
     wtss.obj         <- wtss.R::WTSS(URL)
     # obtains information about the coverages
     coverages.vec    <- wtss.R::listCoverages(wtss.obj)
     # is the coverage in the list of coverages?
     ensurer::ensure_that (coverage, . %in%coverages.vec,
                           err_desc = "sits_getcovWTSS: coverage is not available in the WTSS server")
     #retrive the coverage information
     cov.lst    <- wtss.R::describeCoverage(wtss.obj, coverage)
     cov <- cov.lst[[coverage]]

     return (cov)
}
