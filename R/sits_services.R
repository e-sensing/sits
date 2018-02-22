
#' @title Provides information about the time series services available
#' @name sits_services
#'
#' @description uses the configuration file to print information about the services, products and coverages
#' @export
sits_services <- function() {
    services <- sits.env$config$ts_services

    for (s in services) {
        if (s != "RASTER") {
            cat(paste0("Service - ", s,"\n"))
            q <- paste0(s,"_products")
            products <- sits.env$config[[q]]

            for (p in products) {
                cat(paste0("   Product - ", p,"\n"))
                q1 <- paste0(s,"_coverages")
                coverages <- sits.env$config[[q1]][[p]]
                    cat(paste0("      Coverages - ", paste0(coverages),"\n"))

            }
            cat("------------------\n")
        }
    }
}
