
#' @title Provides information about the time series services available
#' @name sits_services
#'
#' @description Uses the configuration file to print information about the services, products and coverages.
#' @export
sits_services <- function() {
    services <- sits.env$config$ts_services

    for (s in services) {
        if (s != "RASTER") {
            cat(paste0("Service: \"", s,"\"\n"))
            q <- paste0(s,"_coverages")
            coverages <- sits.env$config[[q]]

            for (cov in coverages) {
                cat(paste0("   Coverage: \"", cov, "\"\n"))
                q1 <- paste0(s, "_bands")
                bands <- sits.env$config[[q1]][[cov]]
                cat(paste0("      Bands: \"", paste(bands, collapse = "\", \""), "\"\n"))
            }
        }
    }
}
