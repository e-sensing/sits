#' @title Provides information about time series service
#' @name sits_info_services
#' @author Gilberto Camara
#'
#' @description Obtains information about the time series servers, their products and their coverages.
#'
#' @return A boolean value indicating success of failure.
#' @export
sits_info_services <- function() {
    services <- .sits_get_services()

    services %>%
        purrr::map(function(s) {
            # get the protocol associated with the service
            protocol <- .sits_get_protocol(s)

            if (protocol == "WTSS") {
                tryCatch({
                    URL  <- .sits_get_server(s)
                    # obtains information about the available coverages
                    wtss.obj   <- wtss::WTSS(URL)
                    # obtains information about the available coverages
                    names    <- wtss::listCoverages(wtss.obj)

                }, error = function(e){
                    msg <- paste0("WTSS service not available at URL ", URL)
                    .sits_log_error(msg)
                    message(msg)
                })

                cat(paste0("Service -- ", s))
                cat(paste0("---- Coverages -- ", names))
            }
            if (protocol == "SATVEG"){
                q <- paste0("SATVEG_coverages")
                names <- sits.env$config[[q]]
                cat(paste0("Service -- ", s))
                cat(paste0("---- Coverages -- ", names))
            }

        })
}
