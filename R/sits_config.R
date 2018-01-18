#' @title Reads a configuration file and loads it in the main environment
#' @name sits_config
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Reads a user-specified configuration file, located in a "config.yml" file
#' in the working directory. If this file is not found, reads a default package configuration file.
#' By default, the SITS configuration file "config.yml" is located at the directory "extdata" of the
#' package. The configuration file is an YAML file that should provide at least the following parameters:
#'
#' default:
#'    ts_servers     :
#'        - "WTSS"
#'        - "SATVEG"
#'    WTSS_server    : "http://www.dpi.inpe.br/tws/wtss"
#'    SATVEG_server  : "https://www.satveg.cnptia.embrapa.br"
#'    SATVEG_account : "/satvegws/ws/perfil/ZW46IXzr4pRzJlX/"
#'
#' To see the contents of the configuration file, please use the function \code{link[sits]{sits_show_config()}}.
#'
#' @return config_sits  A list with the configuration parameters used by SITS
#' @examples
#' config_sits <- sits_config()
#' @export
#'
sits_config <- function() {

    if (purrr::is_null(sits.env$config)) {
        # run the default configuration file
        yml_file <- system.file("extdata", "config.yml", package = "sits")

        # check that the file is valid
        ensurer::ensure_that(yml_file, !purrr::is_null(.),
                             err_desc = "sits_config : Please provide a valid configuration file")

        # read the configuration parameters
        sits.env$config <- config::get(file = yml_file)

        # try to find a valid user configuration file
        WD <- getwd()
        if (file.exists(paste0(WD, "/config.yml"))) {
            user_yml_file <- paste0(WD, "/config.yml")
            config_user <- config::get(file = user_yml_file)
            sits.env$config <- config::merge(sits.env$config, config_user)
        }
        sits.env$config$coverages <- .sits_tibble_coverage()
    }
    return(TRUE)
}

#' @title Shows the contents of the SITS configuration file
#' @name sits_show_config
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Displays the contents of SITS configuration file. For more details
#' on how to set the configuration file, please use the function \code{link[sits]{sits_config()}}.
#'
#' @return config_sits  A list with the configuration parameters used by SITS
#' @examples
#' config_sits <- sits_config()
#' @export
#'
sits_show_config <- function() {

    # try to find a valid configuration file
        WD <- getwd()
        if (file.exists(paste0(WD, "/config.yml")))
            yml_file <- paste0(WD, "/config.yml")
        else
            yml_file <- system.file("extdata", "config.yml", package = "sits")

        # check that the file is valid
        ensurer::ensure_that(yml_file, !purrr::is_null(.),
                             err_desc = "sits_config : Please provide a valid configuration file")
    # read the configuration parameters
    cat(readLines(yml_file), sep = "\n")

    return(invisible())
}


#' @title Get an account to access a time series service
#' @name .sits_get_account
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param service        the name of the time series service
#' @param product        the name of the product
#' @return accountURL    the account for service access
#'
.sits_get_account <- function(service, product) {
    # pre-condition
    ensurer::ensure_that(service, (.) == "SATVEG",
                         err_desc = "Account only required for service SATVEG")
    s <- paste0(service,"_products")
    ensurer::ensure_that(product, (.) %in% sits.env$config[[s]],
                         err_desc = paste0("Product ", product, " not available for service ", service))

    i1      <- paste0(service,"_account")
    account <- sits.env$config[[i1]][[product]]

    # get the server URL from the configuration file
    s <- paste0(service,"_server")
    serverURL <- sits.env$config[[s]]

    accountURL <- paste0(serverURL, account)

    #post-condition
    ensurer::ensure_that(accountURL, length(.) > 0,
                         err_desc = paste0("accountURL not available for service ", service))

    return(accountURL)
}
.sits_get_adjustment_shift <- function() {

    return(sits.env$config$adjustment_shift)

}
#' @title Retrieve the bands avaliable for the product in the time series service
#' @name .sits_get_bands
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param service        the name of the time series service
#' @param product        the name of the product
#' @return bands         bands available
#'
.sits_get_bands <- function(service, product){
    # pre-condition
    ensurer::ensure_that(service, (.) %in% sits.env$config$ts_services,
                         err_desc = "Service not available - check configuration file")

    if (service != "RASTER") {
        s <- paste0(service,"_products")
        ensurer::ensure_that(product, (.) %in% sits.env$config[[s]],
                             err_desc = paste0("Product ", product, " not available for service ", service))

        # get the bands information from the configuration file
        b <- paste0(service,"_bands")
        bands <- sits.env$config[[b]][[product]]
    }
    #post-condition
    ensurer::ensure_that(bands, length(.) > 0,
                         err_desc = paste0("bands not available for ", product, " in service ", service))
    return(bands)
}
#' @title Retrieve the bounding box for the product available at service
#' @name .sits_get_bbox
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param service        name of the time series service
#' @param product        name of the product
#' @param r_obj          R object associated with the coverage
#' @return bbox          bounding box
#'
.sits_get_bbox <- function(service, product, r_obj = NA){

    bbox         <- vector(length = 4)
    names(bbox)  <- c("xmin", "xmax", "ymin", "ymax")

    if (service == "RASTER") {
        ensurer::ensure_that(r_obj, length(.) > 0,
                             err_desc = "raster objects have not been created")
        bbox["xmin"] <- r_obj@extent@xmin
        bbox["xmax"] <- r_obj@extent@xmax
        bbox["ymin"] <- r_obj@extent@ymin
        bbox["ymax"] <- r_obj@extent@ymax
    }
    else {
        # pre-condition
        s <- paste0(service,"_products")
        ensurer::ensure_that(product, (.) %in% sits.env$config[[s]],
                             err_desc = paste0("Product ", product, " not available for service ", service))

        # get the bounding box from the configuration file
        i1 <- paste0(service,"_bbox")

        for (c in names(bbox))
            bbox[c] <- sits.env$config[[i1]][[product]][[c]]
    }
    return(bbox)
}

#' @title Retrieve the coverage based in coverage name for a time series service
#' @name .sits_get_coverage
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param service        the name of the time series service
#' @param name           the name of the coverage
#' @return coverage.tb   metadata about the coverage
#'

.sits_get_coverage <- function(service, name) {

    if (purrr::is_null(sits.env$config$coverages))
        return(NULL)
    else {
        coverage.tb <- dplyr::filter(sits.env$config$coverages, name == name & service == service)
        if (NROW(coverage.tb) == 1)
            return(coverage.tb)
        else
            return(NULL)
    }
}

#' @title Retrieve the missing values for a given band for an image product
#' @name .sits_get_missing_value
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param product        the name of the product
#' @param band           the name of the band
#' @return mv            missing_value
#'
.sits_get_missing_value <- function(product, band) {

    # create a string to query for the missing values
    mv_name <- paste0(product,"_missing_value")
    mv <- as.numeric(sits.env$config[[mv_name]][[band]])
    #post-condition
    ensurer::ensure_that(mv, !purrr::is_null(.),
                         err_desc = paste0("Configuration file has no missing values for ", band, " of ", product))

    return(mv)
}
.sits_get_products <- function(service) {
    s <- paste0(service,"_products")
    return(sits.env$config[[s]])
}
#' @title Retrieve the projection for the product available at service
#' @name .sits_get_projection
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param service        the name of the time series service
#' @param product        the name of the product
#' @param r_obj          R object associated with the coverage
#' @return crs           CRS PROJ4 infomation

.sits_get_projection <- function(service, product, r_obj = NA) {

    # pre-condition
    ensurer::ensure_that(service, (.) %in% sits.env$config$ts_services,
                         err_desc = "Service not available - check configuration file")

    if (service != "RASTER") {
        s <- paste0(service,"_products")
        ensurer::ensure_that(product, (.) %in% sits.env$config[[s]],
                             err_desc = paste0("Product ", product, " not available for service ", service))

        # create a string to store the query
        s <- paste0(service, "_crs")
        crs <- sits.env$config[[s]][[product]]

        #post-condition
        ensurer::ensure_that(crs, length(.) > 0,
                             err_desc = paste0("Projection information for ", product, " of service ", service, " not available"))
    }
    else {
        ensurer::ensure_that(r_obj, length(.) > 0,
                             err_desc = "raster objects have not been created")
        crs <- r_obj@crs@projargs
    }

    return(crs)
}
#' @title Retrieve the protocol associated to the time series service
#' @name .sits_get_protocol
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#'
#' @param service         The name of the service
#' @return protocol       The protocol associated to the service
#'
.sits_get_protocol <- function(service) {
    # pre-condition
    ensurer::ensure_that(service, (.) %in% sits.env$config$ts_services,
                         err_desc = "Service not available - check configuration file")

    # get the server URL from the configuration file
    s <- paste0(service,"_protocol")
    protocol <- sits.env$config[[s]]

    return(protocol)
}
#' @title Retrieve the pixel resolution for an image product
#' @name .sits_get_resolution
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param product        the name of the product
#' @return res           vector of (xres, yres)
.sits_get_resolution <- function(product) {
    # create a string to query for the resolution
    s <- paste0(product,"_resolution")
    res          <- vector(length = 2)
    names(res)  <- c("xres", "yres")
    for (c in names(res))
        res[c] <- sits.env$config[[s]][[c]]

    #post-condition
    ensurer::ensure_that(res["xres"], as.numeric(.) > 0,
                         err_desc = paste0("Horizontal resolution not available for product ", product))
    ensurer::ensure_that(res["yres"], as.numeric(.) > 0,
                         err_desc = paste0("Vertical resolution not available for product ", product))

    return(res)
}

#' @title Retrieve the time series server for the product
#' @name .sits_get_server
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#'
#' @param service         The name of the service
#' @return serverURL      A string with the server URL that provides the service
#'
.sits_get_server <- function(service) {
    # pre-condition
    ensurer::ensure_that(service, (.) %in% sits.env$config$ts_services,
                         err_desc = "Service not available - check configuration file")

    # get the server URL from the configuration file
    s <- paste0(service,"_server")
    serverURL <- sits.env$config[[s]]

    return(serverURL)
}
#' @title List the time series services available
#' @name .sits_get_services
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @param protocol  The protocol used to assess the time series service
#'
#' @return size          vector of (nrows, ncols)
.sits_get_services <- function(protocol = NULL) {
    if (purrr::is_null(protocol))
        return(sits.env$config$ts_services)
    else{
        ensurer::ensure_that(protocol, (.) %in% sits.env$config$protocols)
        s <- paste0(protocol, "_services")
        return(sits.env$config[[s]])
    }
}

#' @title Retrieve the size of the product for a given time series service
#' @name .sits_get_size
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param service        the name of the time series service
#' @param product        the name of the product
#' @param r_obj          R object associated with the coverage
#' @return size          vector of (nrows, ncols)
.sits_get_size <- function(service, product, r_obj = NA) {

    size         <- vector(length = 2)
    names(size)  <- c("nrows", "ncols")

    if (service != "RASTER") {
        # pre-condition
        s <- paste0(service,"_products")
        ensurer::ensure_that(product, (.) %in% sits.env$config[[s]],
                             err_desc = paste0("Product ", product, " not available for service ", service))

        # get the size from the configuration file
        i1  <- paste0(service,"_size")

        for (c in names(size))
            size[c] <- sits.env$config[[i1]][[product]][[c]]
    }
    else {
        ensurer::ensure_that(r_obj, length(.) > 0,
                             err_desc = "raster objects have not been created")
        size["nrows"] <- r_obj@nrows
        size["ncols"] <- r_obj@ncols
    }

    #post-condition
    ensurer::ensure_that(size["nrows"], as.integer(.) > 0,
                         err_desc = paste0("Number of rows not available for product ", product, " for service ", service))
    ensurer::ensure_that(size["ncols"], as.integer(.) > 0,
                         err_desc = paste0("Number of cols not available for product ", product, " for service ", service))

    return(size)
}
#' @title Retrieve the scale factor for a given band for an image product
#' @name .sits_get_scale_factor
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param product        the name of the product
#' @param band           the name of the band
#' @return sf            scale factor
#'
#'
.sits_get_scale_factor <- function(product, band) {
    # create a string to query for the scale factors
    sf_name <- paste0(product,"_scale_factor")
    sf <- as.numeric(sits.env$config[[sf_name]][[band]])
    #post-condition
    ensurer::ensure_that(sf, !purrr::is_null(.),
                         err_desc = paste0("Configuration file has no scale factors for ", band, " of ", product))
    return(sf)
}

#' @title Retrieve the default timeline for a product for a given time series service
#' @name .sits_get_timeline
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param service        the name of the time series service
#' @param product        the name of the product
#' @param name           the name of the coverage
#' @return size          vector of (nrows, ncols)
.sits_get_timeline <- function(service, product, name){

    if (service == "RASTER")
        message("Please provide timeline for raster data: will use default timeline")

    protocol <- .sits_get_protocol(service)

    if (protocol == "WTSS") {
        URL  <- .sits_get_server(service)
        # obtains information about the available coverages
        wtss.obj         <- wtss::WTSS(URL)
        coverages.vec    <- wtss::listCoverages(wtss.obj)

        # is the coverage in the list of coverages?
        ensurer::ensure_that(name, (.) %in% coverages.vec,
                             err_desc = "sits_get_timeline: coverage is not available in the WTSS server")

        # describe the coverage
        cov.lst    <- wtss::describeCoverage(wtss.obj, name)
        cov        <- cov.lst[[name]]

        # temporal extent
        timeline <- cov$timeline

    }
    else {
        s <- paste0(service, "_timeline")
        timeline <- lubridate::as_date(sits.env$config[[s]][[product]])

        ensurer::ensure_that(timeline, length(.) > 0,
                             err_desc = paste0("Could not retrieve timeline for product ", product))
    }

    return(lubridate::as_date(timeline))
}
#' @title Check that the service is valid
#' @name .sits_check_service
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param service        the name of the time series service
.sits_check_service <- function(service){
    # Ensure that the service is available
    ensurer::ensure_that(service, (.) %in% sits.env$config$ts_services,
                         err_desc = "sits_getdata: Invalid time series service")
    return(TRUE)
}

