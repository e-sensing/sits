
#
#' Set important global variables for WTSS access
#'
#' \code{sits_configWTSS} stores the information about the WTSS server
#' and the coverage name to be retrieved in the global environment
#' The assumption is that in a SITS session, this information is defined one and
#' used in many applications
#'
#' This function should be run at the start of each SITS session
#'
#' @param URL             the URL for the WTSS time series service
#' @param coverage        name of space-time coverage that contains the time series information
#' @param bands           a character vector with the names of the bands to be retrieved
#' @export
sits_configWTSS <- function (URL       = "http://www.dpi.inpe.br/tws/wtss",
                              coverage  = "mod13q1_512",
                              bands     = c("ndvi", "evi")) {
     # assigns variables to the global enviroment
     # create a WTSS connection
     WTSS_URL.gl      <<- URL
     # store the TWSS object
     ts_server.gl     <<- WTSS(URL)
     # store the name of the coverage
     cov_name.gl      <<- coverage
     # describe the coverage
     cov_desc.lst      <- describeCoverage(ts_server.gl, cov_name.gl)
     cov_desc.tb      <<- cov_desc.lst[[cov_name.gl]]$attributes %>%
          dplyr::select (name, scale_factor, missing_value) %>%
          as_tibble()

     from.gl      <<- cov_desc.lst[[cov_name.gl]]$geo_extent$temporal$start
     to.gl        <<- cov_desc.lst[[cov_name.gl]]$geo_extent$temporal$end
     bands.gl     <<- bands
     bands_s.gl   <<- as.character(map (bands.gl,
                                                function (name) {
                                                     new_name <- paste(name,"_smooth", sep="")
                                                     return (new_name)
                                                }
     ))
}
