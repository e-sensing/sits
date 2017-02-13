
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
sits_configWTSS <- function (URL       = "http://www.dpi.inpe.br/tws/wtss",
                              coverage  = "mod13q1_512",
                              bands     = c("ndvi", "evi", "red", "nir", "blue", "mir")) {
     # assigns variables to the global enviroment
     # create a WTSS connection
     WTSS_URL.gl      <<- URL
     # store the TWSS object
     ts_server.gl     <<- wtss.R::WTSS(URL)
     # store the name of the coverage
     cov_name.gl      <<- coverage
     # describe the coverage
     cov_desc.lst      <- wtss.R::describeCoverage(ts_server.gl, cov_name.gl)
     cov_desc.gl      <<- cov_desc.lst[[cov_name.gl]]$attributes %>%
          dplyr::select (name, scale_factor, missing_value) %>%
          as_tibble() %>%
          dplyr::rename (band = name)

     resolution_x.gl    <<- cov_desc.lst[[cov_name.gl]]$geo_extent$spatial$resolution$x
     resolution_y.gl    <<- cov_desc.lst[[cov_name.gl]]$geo_extent$spatial$resolution$y
     start_date.gl      <<- cov_desc.lst[[cov_name.gl]]$geo_extent$temporal$start
     end_date.gl        <<- cov_desc.lst[[cov_name.gl]]$geo_extent$temporal$end
     bands.gl           <<- bands
}
