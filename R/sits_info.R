#' @title Provides information about time series service
#' @name sits_info_services
#' @author Gilberto Camara
#'
#' @description    obtains information about the time series servers, their products and their coverages
#'
#' @return bool    boolean value indicating success of failure
#'
sits_info_services <- function() {
    services <- .sits_get_services()

    for (s in services) {
        cat(paste0("Service -- ", service))
        products <- .sits_get_products (s)
        for (p in products) {
            cat(paste0("Product -- ", p))
        }
    }
}
