#
#' Provides information about WTSS service
#'
#' \code{sits_info_WTSS} uses the WTSS services to provide information about the
#' coverages that are served
#'
#' @param URL     the URL for the WTSS time series service
#' @export
#'
sits_info_WTSS <- function (URL    = "http://www.dpi.inpe.br/tws/wtss") {

     # obtains information about the WTSS service
     wtss.obj         <- WTSS(URL)
     # obtains information about the coverages
     coverages.obj    <- listCoverages(wtss.obj)
     # describe each coverage
     desc.obj         <- describeCoverage(wtss.obj, coverages.obj)
     cat (paste ("----", "\n",sep = ""))
     cat (paste ("The WTSS server URL is ", wtss.obj@serverUrl, "\n", sep = ""))
     cat (paste ("----", "\n",sep = ""))

     for (i in 1:length (desc.obj)){

          cov <- desc.obj[[i]]
          # bands <- cov$attributes$name[1:length (cov$attributes$name)]
          cat ("Coverage information\n")
          cat (paste ("Coverage : ", cov$name, "\n",sep = ""))
          cat (paste ("Description :", cov$description, "\n", sep = ""))
          cat (paste ("Source : ", cov$detail, "\n", sep = ""))
          cat (paste ("Bands : ", "\n", sep = "", collapse=""))
          cat (paste (cov$attributes$name[1:length (cov$attributes$name)], sep = ""))
          cat (paste ("\nTime range : ", cov$geo_extent$temporal$start, " to ", cov$geo_extent$temporal$end, "\n", sep = ""))
          cat (paste ("Temporal resolution : ", cov$geo_extent$temporal$resolution, " days ", "\n", sep = ""))
          cat (paste ("Spatial resolution : ", as.integer (cov$geo_extent$spatial$resolution$x), " metres ", "\n", sep = ""))

          cat (paste ("----", "\n",sep = ""))
     }
}
#
#' Set important global variables for WTSS access
#'
#' \code{sits_config} stores the information about the WTSS server
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
sits_config_WTSS <- function (URL       = "http://www.dpi.inpe.br/tws/wtss",
                              coverage  = "mod13q1_512",
                              bands     = c("ndvi", "evi")) {
     # assigns variables to the global enviroment
     # create a WTSS connection
     WTSS_URL.global  <<- URL
     ts_server.global <<- WTSS(URL)
     cov_name.global  <<- coverage
     from.global      <<- "2000-09-13"
     to.global        <<- "2001-08-29"
     bands.global     <<- bands
     bands_smooth.global  <<- as.character(map (bands.global,
                                                function (name) {
                                                     new_name <- paste(name,"_smooth", sep="")
                                                     return (new_name)
                                                }))
}

#'
#' Obtain one timeSeries from WTSS server and load it on a sits table
#'
#' \code{sits_fromWTSS} returns one set of time series provided by a WTSS server
#'
#' Given a location (lat/long), and from/to period, and the WTSS server information
#' retrieve a time series and include it on a stis table
#' A sits table has the metadata and data for each time series
#' <longitude, latitude, from, to, lable, time_series>
#'
#' @param longitude double - the longitude of the chosen location
#' @param latitude  double - the latitude of the chosen location
#' @param from      date - the start of the period
#' @param to        date - the end of the period
#' @param label     string - the label to attach to the time series (optional)
#' @return table    the table with the data
#' @keywords SITS
#' @family   SITS auxiliary functions
#' @examples sits_point_fromWTSS (-53.30, -14.24, "2000-02-18", "2016-09-24")
#' @export
sits_fromWTSS <- function  (longitude =      -54.2313,
                            latitude  =      -14.0482,
                            from      =   from.global,
                            to        =     to.global,
                            label     =     "NoClass") {
     # is the WTSS service running?
     sits_assert_WTSS()
     # get a time series from the WTSS server
     ts <- timeSeries (ts_server.global,
                       coverages  = cov_name.global,
                       attributes = bands.global,
                       longitude  = longitude,
                       latitude   = latitude,
                       start      = from,
                       end        = to
                    )

     # retrieve the time series information
     time_series <- ts[[cov_name.global]]$attributes
     # scale the time series
     time_series[,bands.global] <-  time_series[,bands.global]*0.0001

     # create a list to store the zoo time series coming from the WTSS service
     ts.lst <- list()
     # transform the zoo list into a tibble to store in memory
     ts.lst[[1]] <- as_tibble (fortify.zoo (time_series))

     # create a table to store the WTSS data
     table <- sits_table()
     # add one row to the table
     table <- add_row (table,
                       longitude    = longitude,
                       latitude     = latitude,
                       from         = as.Date(from),
                       to           = as.Date(to),
                       label        = label,
                       coverage     = cov_name.global,
                       time_series  = ts.lst
                    )

     # return the table with the time series
     return (table)
}
#
#' Verify if the information concerning WTSS service has been configured
#'
#' \code{sits_assert_WTSS} tests if the WTSS service has been correctly configured
#'
#'
#'
sits_assert_WTSS <- function() {
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
