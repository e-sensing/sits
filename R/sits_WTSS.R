
#
#' Set important global variables for WTSS access
#'
#' \code{sits_configWTSS} stores the information about the WTSS server
#' and the coverage name to be retrieved in the global environment
#' The assumption is that in a SITS session, this information is defined one and
#' used in many applications
#'
#' This function should be run at the start of each SITS session.
#'
#' The Web Time Seris Service is a lightweight web service the allow remote access to satellite
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
#'  To allow for further processing of time series from a WTSS service, this operation
#'  creates a list with the following variables
#'
#'  URL            - the URL for the WTSS time series service
#'  ts_server      - an object that handles the WTSS connection
#'  coverage       - the name of the coverage that contains the time series for the analysis
#'  cov_desc       - a table with the parameters of the coverage
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
#' @param URL             the URL for the WTSS time series service
#' @param coverage        name of space-time coverage that contains the time series information
#' @param bands           a character vector with the names of the bands to be retrieved
#' @return wtss_info       a list containing the information about the WTSS server and coverage
#' @export
sits_configWTSS <- function (URL      = "http://www.dpi.inpe.br/tws/wtss",
                             coverage = "mod13q1_512",
                             bands    = c("ndvi", "evi", "nir")) {

     wtss_info                <- tibble::lst()
     class (wtss_info)        <- append (class(wtss_info), "wtss_info")
     # create a WTSS connection
     wtss_info$URL            <- URL
     # store the TWSS object
     wtss_info$wtss_obj       <- wtss.R::WTSS(URL)
     # store the name of the coverage
     wtss_info$coverage       <- coverage

     # describe the coverage
     cov_desc.lst      <- wtss.R::describeCoverage(wtss_info$wtss_obj, coverage)
     # retrieve the coverage
     cov <- cov_desc.lst[[coverage]]
     # store the attributes
     wtss_info$cov_desc      <- cov$attributes %>%
                              dplyr::select (name, scale_factor, missing_value) %>%
                              tibble::as_tibble() %>%
                              dplyr::rename (band = name)

     wtss_info$res_x     <- cov$geo_extent$spatial$resolution$x
     wtss_info$res_y     <- cov$geo_extent$spatial$resolution$y
     wtss_info$start_date <- cov$geo_extent$temporal$start
     wtss_info$end_date  <- cov$geo_extent$temporal$end
     wtss_info$x_min     <- cov$geo_extent$spatial$extent$xmin
     wtss_info$y_min     <- cov$geo_extent$spatial$extent$ymin
     wtss_info$x_max     <- cov$geo_extent$spatial$extent$xmax
     wtss_info$y_max     <- cov$geo_extent$spatial$extent$ymax
     wtss_info$bands     <- bands

     return (wtss_info)
}

#
#' Provides information about WTSS service
#'
#' \code{sits_info_WTSS} uses the WTSS services to provide information about the
#' coverages that are served
#'
#' @param URL     the URL for the WTSS time series service
#' @export
#'
sits_infoWTSS <- function (URL = "http://www.dpi.inpe.br/tws/wtss") {
     # obtains information about the WTSS service
     wtss.obj         <- wtss.R::WTSS(URL)
     # obtains information about the coverages
     coverages.obj    <- wtss.R::listCoverages(wtss.obj)
     # describe each coverage
     desc.obj         <- wtss.R::describeCoverage(wtss.obj, coverages.obj)
     cat (paste ("-----------------------------------------------------------", "\n",sep = ""))
     cat (paste ("The WTSS server URL is ", wtss.obj@serverUrl, "\n", sep = ""))
     cat (paste ("------------------------------------------------------------", "\n",sep = ""))

     desc.obj %>%
          purrr::map (function (cov) {
               cat (paste ("Coverage: ", cov$name, "\n",sep = ""))
               cat (paste ("Description: ", cov$description, "\n", sep = ""))
               cat (paste ("Source: ", cov$detail, "\n", sep = ""))
               cat (paste ("Bands: ", "\n", sep = "", collapse=""))
               attr <- as.data.frame(cov$attributes)
               print (attr[1:2])
               cat (paste ("\nSpatial extent: ", "(",
                           cov$geo_extent$spatial$extent$xmin, ", ",
                           cov$geo_extent$spatial$extent$ymin, ") - (",
                           cov$geo_extent$spatial$extent$xmax, ", ",
                           cov$geo_extent$spatial$extent$ymax, ")", sep =""))
               cat (paste ("\nTime range: ", cov$geo_extent$temporal$start, " to ", cov$geo_extent$temporal$end, "\n", sep = ""))
               cat (paste ("Temporal resolution: ", cov$geo_extent$temporal$resolution, " days ", "\n", sep = ""))
               cat (paste ("Spatial resolution: ", as.integer (cov$geo_extent$spatial$resolution$x), " metres ", "\n", sep = ""))

               cat (paste ("----------------------------------------------------------------------------------", "\n",sep = ""))
               return (invisible (cov))
          })
     return (invisible (desc.obj))
}
