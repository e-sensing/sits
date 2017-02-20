
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
#'  creates the following global variables:
#'
#'  WTSS_URL.gl    - the the URL for the WTSS time series service
#'  ts_server.gl   - an object that handles the WTSS connection
#'  cov_name.gl    - the name of the coverage that contains the time series for the analysis
#'  cov_desc.gl    - the table with the parameters of the coverage
#'  start_date.gl  - the start date for the time series data in the coverage
#'  end_date.gl    - the end date for the time series data in the coverage
#'  bands.gl       - the bands of the data to be retrieved from the WTSS
#'
#' @param URL             the URL for the WTSS time series service
#' @param coverage        name of space-time coverage that contains the time series information
#' @param bands           a character vector with the names of the bands to be retrieved
#' @export
sits_configWTSS <- function (URL      = "http://www.dpi.inpe.br/tws/wtss",
                             coverage = "mod13q1_512",
                             bands    = c("ndvi", "evi", "red", "nir", "blue", "mir")) {
     # assigns variables to the global enviroment
     # create a WTSS connection
     WTSS_URL.gl      <<- URL
     # store the TWSS object
     ts_server.gl     <<- wtss.R::WTSS(URL)
     # store the name of the coverage
     cov_name.gl      <<- coverage
     # describe the coverage
     cov_desc.lst      <- wtss.R::describeCoverage(ts_server.gl, cov_name.gl)
     # retrieve the coverage
     cov <- cov_desc.lst[[cov_name.gl]]
     cov_desc.gl      <<- cov$attributes %>%
          dplyr::select (name, scale_factor, missing_value) %>%
          as_tibble() %>%
          dplyr::rename (band = name)

     resolution_x.gl    <<- cov$geo_extent$spatial$resolution$x
     resolution_y.gl    <<- cov$geo_extent$spatial$resolution$y
     start_date.gl      <<- cov$geo_extent$temporal$start
     end_date.gl        <<- cov$geo_extent$temporal$end
     x_min.gl           <<- cov$geo_extent$spatial$extent$xmin
     y_min.gl           <<- cov$geo_extent$spatial$extent$ymin
     x_max.gl           <<- cov$geo_extent$spatial$extent$xmax
     y_max.gl           <<- cov$geo_extent$spatial$extent$ymax
     bands.gl           <<- bands
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
     wtss.obj         <- WTSS(URL)
     # obtains information about the coverages
     coverages.obj    <- listCoverages(wtss.obj)
     # describe each coverage
     desc.obj         <- describeCoverage(wtss.obj, coverages.obj)
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

#
#' Verify if the information concerning WTSS service has been configured
#'
#' \code{sits_testWTSS} tests if the WTSS service has been correctly configured
#'  The function verifies that the WTSS service is running.
#'
#'  The Web Time Seris Service (WTSS) is a lightweight web service the allow remote access to satellite
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
#' @export
sits_testWTSS <- function() {
     tryCatch (
          exists(WTSS_URL.gl),
          error = function (cond) {
               message (paste ("Missing WTSS service information!!","\n",
                               "Please configure the WTSS service","\n",
                               "using function sits_configWTSS(service_URL, coverage_name, bands)", "\n",
                               sep=""))
               stop(cond)
          }
     )
}
