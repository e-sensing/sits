
#' @title Provides information about the time series services available
#' @name sits_services
#'
#' @description Uses the configuration file to print information about the services, products and coverages.
#' @export
sits_services <- function() {
    services <- sits.env$config$ts_services

    for (s in services) {
        # get the protocol associated with the service
        protocol <- .sits_get_protocol(s)

        if (protocol == "WTSS") {
            tryCatch({
                URL  <- .sits_get_server(s)
                # obtains information about the available coverages
                wtss.obj  <- wtss::WTSS(URL)
            }, error = function(e){
                msg <- paste0("WTSS service not available at URL ", URL)
                .sits_log_error(msg)
                message(msg)
            })

            coverages <- wtss::listCoverages(wtss.obj)

            cat(paste0("Service: \"", s,"\"\n"))
            for (cov_name in coverages) {
                cat(paste0("   Coverage: \"", cov_name, "\"\n"))

                # describe the coverage
                cov <- wtss::describeCoverage(wtss.obj, cov_name)[[cov_name]]

                # retrieve information about the bands
                band_info <- cov$attributes
                attr <- as.data.frame(band_info)
                bands <- as.vector(attr[,"name"])
                cat(paste0("      Bands: \"", paste(bands, collapse = "\", \""), "\"\n"))
            }
        } else if (protocol == "SATVEG") {
            cat(paste0("Service: \"", s,"\"\n"))
            q <- paste0(s,"_coverages")
            coverages <- sits.env$config[[q]]

            for (cov in coverages) {
                cat(paste0("   Coverage: \"", cov, "\"\n"))
                q1 <- paste0(s, "_bands")
                bands <- sits.env$config[[q1]][[cov]]
                cat(paste0("      Bands: \"", paste(bands, collapse = "\", \""), "\"\n"))
            }
        } else if (protocol == "EOCUBES") {
            cat(paste0("Service: \"", s,"\"\n"))

            remote.obj   <- EOCubes::remote(name = "eocubes")
            coverages <- names(EOCubes::list_cubes(remote.obj))

            for (cov in coverages) {
                cat(paste0("   Coverage: \"", cov, "\"\n"))
                cub.obj <- EOCubes::cube(cov, remote.obj)
                cat(paste0("      Bands: \"", paste(EOCubes::cube_bands(cub.obj), collapse = "\", \""), "\"\n"))
            }
        }
    }
}
