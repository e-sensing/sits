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
     cat (paste ("----", "\n",sep = ""))
     cat (paste ("The WTSS server URL is ", wtss.obj@serverUrl, "\n", sep = ""))
     cat (paste ("----", "\n",sep = ""))

     desc.obj %>%
          purrr::map (function (cov) {
               cat (paste ("Coverage : ", cov$name, "\n",sep = ""))
               cat (paste ("Description :", cov$description, "\n", sep = ""))
               cat (paste ("Source : ", cov$detail, "\n", sep = ""))
               cat (paste ("Bands : ", "\n", sep = "", collapse=""))
               cat (paste (cov$attributes$name[1:length (cov$attributes$name)], sep = ""))
               cat (paste ("\nTime range : ", cov$geo_extent$temporal$start, " to ", cov$geo_extent$temporal$end, "\n", sep = ""))
               cat (paste ("Temporal resolution : ", cov$geo_extent$temporal$resolution, " days ", "\n", sep = ""))
               cat (paste ("Spatial resolution : ", as.integer (cov$geo_extent$spatial$resolution$x), " metres ", "\n", sep = ""))

               cat (paste ("----", "\n",sep = ""))

          })
     end <- cat (paste ("----", "\n",sep = ""))
     return (end)
}
