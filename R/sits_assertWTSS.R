#
#' Verify if the information concerning WTSS service has been configured
#'
#' \code{sits_assert_WTSS} tests if the WTSS service has been correctly configured
#'  The function verifies that the WTSS service is running
#'
#'
sits_assertWTSS <- function() {
     tryCatch (
          exists(WTSS_URL.global),
          error = function (cond) {
               message (paste ("Missing WTSS service information!!","\n",
                               "Please configure the WTSS service","\n",
                               "using function sits_config_WTSS(service_URL, coverage_name, bands)", "\n",
                               sep=""))
               stop(cond)
          }
     )
}
