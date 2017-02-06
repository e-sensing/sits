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
