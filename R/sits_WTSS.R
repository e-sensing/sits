#' @title Provides information about WTSS service
#' @name sits_infoWTSS
#' @author Gilberto Camara
#'
#' @description obtains information about the WTSS server
#' and about the coverages.
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
#' @return wtss.obj       an R object containing the information about the WTSS server
#' @export

sits_infoWTSS <- function (URL = "http://www.dpi.inpe.br/tws/wtss") {
     # obtains information about the WTSS service
     wtss.obj         <- wtss::WTSS(URL)
     cat (paste ("-----------------------------------------------------------", "\n",sep = ""))
     cat (paste ("The WTSS server URL is ", wtss.obj@serverUrl, "\n", sep = ""))

     # obtains information about the coverages
     coverages.obj    <- wtss::listCoverages(wtss.obj)
          cat (paste ("Available coverages: \n"))
          coverages.obj %>%
               purrr::map (function (c) cat (paste (c, "\n", sep = "")))
          cat (paste ("------------------------------------------------------------", "\n",sep = ""))

     return (invisible(wtss.obj))
}
#' @title Provides information about one coverage of the WTSS service
#' @name sits_coverageWTSS
#'
#' @description uses the WTSS services to print information and save metadata about a
#' chosen coverage:
#'  bands          - the bands of the data to be retrieved from the WTSS
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
#'
#' @param URL        the URL for the WTSS time series service
#' @param coverage   the name of the coverage
#' @param .show      show information about the coverage (Default: FALSE)
#' @export
#'
sits_coverageWTSS <- function (URL = "http://www.dpi.inpe.br/tws/wtss", coverage = NULL, .show = FALSE) {
     # obtains information about the WTSS service
     wtss.obj         <- wtss::WTSS(URL)
     # obtains information about the coverages
     coverages.vec    <- wtss::listCoverages(wtss.obj)
     # is the coverage in the list of coverages?
     ensurer::ensure_that (coverage, . %in%coverages.vec,
                           err_desc = "sits_coverageWTSS: coverage is not available in the WTSS server")
     # describe the coverage
     cov.lst    <- wtss::describeCoverage(wtss.obj, coverage)
     cov <- cov.lst[[coverage]]

     # retrieve information about the bands
     band_info <- cov$attributes

     # temporal extent
     timeline <- cov$timeline
     start <- timeline[1]
     end <- timeline[length(timeline)]

     # temporal resolution is approximate, taken as the difference between first and second date
     temporal_resolution <- as.integer ((lubridate::as_date(timeline[2])
                                         - lubridate::as_date(timeline[1]))/lubridate::ddays(1))

     if (.show) {
         # name, description and source of coverage
         cat (paste ("----------------------------------------------------------------------------------", "\n",sep = ""))
         cat (paste ("Coverage: ", cov$name, "\n",sep = ""))
         cat (paste ("Description: ", cov$description, "\n", sep = ""))
         cat (paste ("Source: ", cov$detail, "\n", sep = ""))

         # information about the bands
         cat (paste ("Bands: ", "\n", sep = "", collapse=""))

         attr <- as.data.frame(band_info)
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

         cat (paste ("\nTime range: ", start, " to ", end, "\n", sep = ""))

         cat (paste ("Temporal resolution: ", temporal_resolution, " days ", "\n", sep = ""))
         cat (paste ("----------------------------------------------------------------------------------", "\n",sep = ""))
     }


     b <- tibble::as.tibble(band_info[, -(3:4)])
     coverage.tb <-  .sits_tibble_coverage()
     coverage.tb <-  tibble::add_row(coverage.tb,
                                 wtss.obj       = list(wtss.obj),
                                 name           = cov$name,
                                 bands          = list(b),
                                 start_date     = as.Date(cov$timeline[1]),
                                 end_date       = as.Date(cov$timeline[length(timeline)]),
                                 timeline       = list(cov$timeline),
                                 xmin           = cov$spatial_extent$xmin,
                                 xmax           = cov$spatial_extent$xmax,
                                 ymin           = cov$spatial_extent$ymin,
                                 ymax           = cov$spatial_extent$ymax,
                                 xres           = cov$spatial_resolution$x,
                                 yres           = cov$spatial_resolution$y,
                                 crs            = cov$crs$proj4
     )
     return (coverage.tb)
}

#' @title Obtain information about one coverage of the WTSS service
#' @name sits_getcovWTSS
#'
#' @description uses the WTSS services to retrieve information about a given coverage:
#'  name            - coverage name
#'  description     - description
#'  detail          - more information (source)
#'  attributes      - spectral bands (name, description, datatype, valid_range.min, valid_range.max, scale_factor, missing_value)
#'  spatial_extent  - bounding box in space (xmin, ymin, xmax, ymax)
#'  crs             - Projection CRS
#'  timeline        - dates of images contained in the coverage
#'
#' @param URL        the URL for the WTSS time series service
#' @param coverage   the name of the coverage
#' @return cov       a list with descriptive information about the coverage
#' @export
sits_getcovWTSS <- function (URL = "http://www.dpi.inpe.br/tws/wtss", coverage = NULL) {
     # is the coverage name provided?
     ensurer::ensure_that(coverage, !purrr::is_null (.), err_desc = "sits_getcovWTSS: Coverage name must be provided")

     # obtains information about the WTSS service
     wtss.obj         <- wtss::WTSS(URL)
     # obtains information about the coverages
     coverages.vec    <- wtss::listCoverages(wtss.obj)
     # is the coverage in the list of coverages?
     ensurer::ensure_that (coverage, . %in%coverages.vec,
                           err_desc = "sits_getcovWTSS: coverage is not available in the WTSS server")
     #retrive the coverage information
     cov.lst    <- wtss::describeCoverage(wtss.obj, coverage)
     cov <- cov.lst[[coverage]]

     return (cov)
}
