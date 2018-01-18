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
#' @return wtss.obj       an R object containing the information about the WTSS server
#'
#' @examples
#' \donttest{
#' # Obtain information about the coverages available
#' sits_infoWTSS()
#' }
#' @export

sits_infoWTSS <- function() {

    wtss.obj <- NULL
    # obtains information about the WTSS service
    services <- .sits_get_services(protocol = "WTSS")

    for (i in 1:length(services)) {
        URL       <- .sits_get_server(service = services[i])
        tryCatch({
            wtss.obj  <- wtss::WTSS(URL)
            cat(paste("-----------------------------------------------------------", "\n",sep = ""))
            cat(paste("The WTSS server URL is ", wtss.obj@serverUrl, "\n", sep = ""))

            # obtains information about the coverages
            coverages.obj    <- wtss::listCoverages(wtss.obj)
            cat(paste("Available coverages: \n"))
            coverages.obj %>%
                purrr::map(function(c) cat(paste(c, "\n", sep = "")))
            cat(paste("------------------------------------------------------------", "\n",sep = ""))

        }, error = function(e) {
            msg <- paste0("WTSS service not available at URL ", URL)
            .sits_log_error(msg)
            message(msg)
        }
        )

    }

    return(invisible(wtss.obj))
}




