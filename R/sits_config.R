#' @title Reads a configuration file and loads it in the main environment
#' @name sits_config
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Reads a user-specified configuration file, located in a "config.yml" file
#' in the working directory. If this file is not found, reads a default package configuration file.
#' By default, the sits configuration file "config.yml" is located at the directory "extdata" of the
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
#' To see the contents of the configuration file, please use \code{\link[sits]{sits_show_config}}.
#'
#' @return A list with the configuration parameters used by sits.
#' @examples
#' config_sits <- sits_config()
#' @export
sits_config <- function() {
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

    return(invisible(sits.env$config))
}

#' @title Shows the contents of the sits configuration file
#' @name sits_show_config
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Displays the contents of sits configuration file. For more details
#' on how to set the configuration file, please use \code{\link[sits]{sits_config}}.
#'
#' @return List with the configuration parameters used by sits.
#' @examples
#' sits_show_config()
#' @export
sits_show_config <- function() {
    # retrieve the basic configuration file
    yml_file <- system.file("extdata", "config.yml", package = "sits")
    # check that the file is valid
    ensurer::ensure_that(yml_file, !purrr::is_null(.),
                         err_desc = "sits_config : Please provide a valid configuration file")
    # try to find a valid user configuration file
        WD <- getwd()
        if (file.exists(paste0(WD, "/config.yml")))
            yml_user_file <- paste0(WD, "/config.yml")
        else
            yml_user_file <- NULL

    # read the configuration parameters
    message("Default system configuration file")
    cat(readLines(yml_file), sep = "\n")
    if (!purrr::is_null(yml_user_file)) {
        message("User configuration file - overrides default config")
        cat(readLines(yml_user_file), sep = "\n")
    }

    return(invisible())
}

#' @title Get an account to access a time series service
#' @name .sits_get_account
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param service        Name of the time series service.
#' @param name           Name of the coverage.
#' @return The account for service access.
.sits_get_account <- function(service, name) {
    # pre-condition
    ensurer::ensure_that(service, (.) == "SATVEG",
                         err_desc = "Account only required for service SATVEG")

    i1      <- paste0(service,"_account")
    account <- sits.env$config[[i1]][[name]]

    # get the server URL from the configuration file
    s <- paste0(service,"_server")
    serverURL <- sits.env$config[[s]]

    accountURL <- paste0(serverURL, account)

    #post-condition
    ensurer::ensure_that(accountURL, length(.) > 0,
                         err_desc = paste0("accountURL not available for service ", service))

    return(accountURL)
}

#' @title Retrieve the value of the adjustment shift
#' @name sits_get_adjustment_shift
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description Retrieves the value of the shift to adjust entries to have only positive values.
.sits_get_adjustment_shift <- function() {
    return(sits.env$config$adjustment_shift)
}

#' @title Retrieve the bands avaliable for the product in the time series service
#' @name .sits_get_bands
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param service        Name of the time series service.
#' @param name           Name of the product.
#' @return The available bands.
.sits_get_bands <- function(service, name){
    # pre-condition
    ensurer::ensure_that(service, (.) %in% sits.env$config$ts_services,
                         err_desc = "Service not available - check configuration file")

    # get the bands information from the configuration file
    b <- paste0(service,"_bands")
    bands <- sits.env$config[[b]][[name]]
    #post-condition
    ensurer::ensure_that(bands, length(.) > 0,
                         err_desc = paste0("bands not available for coverage ", name, " in service ", service))
    return(bands)
}

#' @title Retrieve the bounding box for the product available at service
#' @name .sits_get_bbox
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param service        Name of the time series service.
#' @param name           Name of the coverage.
#' @param r_obj          R object associated with the coverage.
#' @return The bounding box.
.sits_get_bbox <- function(service, name, r_obj = NA){
    bbox        <- vector(length = 4)
    names(bbox) <- c("xmin", "xmax", "ymin", "ymax")

    if (service == "RASTER") {
        ensurer::ensure_that(r_obj, class(.) %in% c("RasterLayer", "RasterBrick", "RasterStack"),
                             err_desc = "r_obj is mandatory when using a RASTER service")
        bbox["xmin"] <- raster::xmin(r_obj)
        bbox["xmax"] <- raster::xmax(r_obj)
        bbox["ymin"] <- raster::ymin(r_obj)
        bbox["ymax"] <- raster::ymax(r_obj)
    }
    else {
        # pre-condition
        s <- paste0(service, "_bbox")

        names(bbox) %>%
            purrr::map(function(c) {
                bbox[c] <<- sits.env$config[[s]][[name]][[c]]
            })
    }

    return(bbox)
}

#' @title Retrieve the color associated to a class
#' @name sits_get_color
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description Retrieve the color associated a class label.
#' @param label  A class label.
.sits_get_color <- function(label) {
    rgb <- as.character(sits.env$config$colors[[label]])
    if(!(length(rgb) > 0))
        rgb <- "#737373"

    return(rgb)
}

#' @title Retrieve the estimated value of R memory bloat
#' @name sits_get_memory_bloat
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description Retrieve the expected memory bloat associated to R.
.sits_get_memory_bloat <- function() {
    return(sits.env$config$R_memory_bloat)
}

#' @title Retrieve the minimum values for a given band
#' @name .sits_get_minimum_values
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param service          Name of the product.
#' @param bands            Vector of bands.
#' @return The minimum values.
.sits_get_minimum_values <- function(service, bands) {
    # create a string to query for the missing values
    minimum_values <- vector()
    mv <- paste0(service,"_minimum_value")
    bands %>%
        purrr::map(function(b) {
            minimum_values[b] <<- as.numeric(sits.env$config[[mv]][[b]])
        })

    #post-condition
    ensurer::ensure_that(minimum_values, length(.) == length(bands),
                         err_desc = paste0("Configuration file has failed to find minimum values for ", service))

    names(minimum_values) <- bands
    return(minimum_values)
}
#' @title Retrieve the maximum values for a given band
#' @name .sits_get_maximum_values
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param service          Name of the product.
#' @param bands            Vector of bands.
#' @return The minimum values.
.sits_get_maximum_values <- function(service, bands) {
    # create a string to query for the maximum values
    maximum_values <- vector()
    mv <- paste0(service,"_maximum_value")
    bands %>%
        purrr::map(function(b) {
            maximum_values[b] <<- as.numeric(sits.env$config[[mv]][[b]])
        })

    #post-condition
    ensurer::ensure_that(maximum_values, length(.) == length(bands),
                         err_desc = paste0("Configuration file has failed to find maximum values for ", service))

    names(maximum_values) <- bands
    return(maximum_values)
}

#' @title Retrieve the missing values for a given band for an image product
#' @name .sits_get_missing_values
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param service          Name of the product.
#' @param name             Name of the coverage.
#' @param bands            Vector of bands.
#' @return The missing values.
.sits_get_missing_values <- function(service, name, bands) {
    # create a string to query for the missing values
    missing_values <- vector()
    mv <- paste0(service,"_missing_value")
    bands %>%
        purrr::map(function (b) {
            missing_values[b] <<- as.numeric(sits.env$config[[mv]][[name]][[b]])
        })
    #post-condition
    ensurer::ensure_that(missing_values, length(.) == length(bands),
                         err_desc = paste0("Configuration file has no missing values for service ", service))

    names(missing_values) <- bands
    return(missing_values)
}

#' @title Retrieve the projection for the product available at service
#' @name .sits_get_projection
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param service        Name of the time series service.
#' @param name           Name of the coverage.
#' @return CRS PROJ4 infomation.
.sits_get_projection <- function(service, name) {
    # pre-condition
    ensurer::ensure_that(service, (.) %in% sits.env$config$ts_services,
                         err_desc = "Service not available - check configuration file")
    # create a string to store the query
    s <- paste0(service, "_crs")
    crs <- sits.env$config[[s]][[name]]

    #post-condition
    ensurer::ensure_that(crs, length(.) > 0,
                         err_desc = paste0("Projection information for coverage ", name, " of service ", service, " not available"))
    return(crs)
}

#' @title Retrieve the protocol associated to the time series service
#' @name .sits_get_protocol
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param service         The name of the service.
#' @return The protocol associated to the service.
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
#' @param service        Name of the service.
#' @param name           Name of the coverage.
#' @return Vector of (xres, yres).
.sits_get_resolution <- function(service, name) {
    # create a string to query for the resolution
    s <- paste0(service,"_resolution")
    res          <- vector(length = 2)
    names(res)  <- c("xres", "yres")

    names(res) %>%
        purrr::map(function(c){
            res[c] <<- sits.env$config[[s]][[name]][[c]]
        })

    #post-condition
    ensurer::ensure_that(res["xres"], as.numeric(.) > 0,
                         err_desc = paste0("Horizontal resolution not available for coverage ", name))
    ensurer::ensure_that(res["yres"], as.numeric(.) > 0,
                         err_desc = paste0("Vertical resolution not available for coverage ", name))

    return(res)
}

#' @title Retrieve the scale factor for a given band for an image product
#' @name .sits_get_scale_factors
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param service        Name of the service.
#' @param name           Name of the coverage.
#' @param bands          Vector of bands.
#' @return Vector of scale factors.
.sits_get_scale_factors <- function(service, name, bands) {
    scale_factors <- vector()
    # create a string to query for the scale factors
    sfq <- paste0(service,"_scale_factor")
    bands %>%
        purrr::map(function (b) {

            scale_factors[b] <<- as.numeric(sits.env$config[[sfq]][[name]][[b]])
    })
    names(scale_factors) <- bands
    #post-condition
    ensurer::ensure_that(scale_factors, !purrr::is_null(.),
                         err_desc = paste0("Configuration file has no scale factors for ", name, " of ", service))
    return(scale_factors)
}

#' @title Retrieve the time series server for the product
#' @name .sits_get_server
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param service         Name of the service.
#' @return A string with the server URL that provides the service.
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
#'
#' @param protocol  A string with the protocol used to assess the time series service.
#' @return Vector of (nrows, ncols).
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
#' @param service        Name of the time series service.
#' @param name           Name of the coverage.
#' @param r_obj          R object associated with the coverage.
#' @return Vector of (nrows, ncols).
.sits_get_size <- function(service, name, r_obj = NA) {
    size         <- vector(length = 2)
    names(size)  <- c("nrows", "ncols")

    if (service != "RASTER") {

        # get the size from the configuration file
        i1  <- paste0(service,"_size")

        names(size) %>%
            purrr::map(function (c){
                size[c] <<- sits.env$config[[i1]][[name]][[c]]
            })
    }
    else {
        ensurer::ensure_that(r_obj, length(.) > 0,
                             err_desc = "raster objects have not been created")
        size["nrows"] <- raster::nrow(r_obj)
        size["ncols"] <- raster::ncol(r_obj)
    }

    #post-condition
    ensurer::ensure_that(size["nrows"], as.integer(.) > 0,
                         err_desc = paste0("Number of rows not available for coverage ", name, " for service ", service))
    ensurer::ensure_that(size["ncols"], as.integer(.) > 0,
                         err_desc = paste0("Number of cols not available for coverage ", name, " for service ", service))

    return(size)
}

#' @title Retrieve the default timeline for a product for a given time series service
#' @name .sits_get_timeline
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param service        Name of the time series service.
#' @param name           Name of the coverage.
#' @return Vector of (nrows, ncols).
.sits_get_timeline <- function(service, name){
    if (service == "RASTER") {
        message("Please provide timeline for raster data: will use default timeline")
        s <- paste0("RASTER_timeline")
        timeline <- lubridate::as_date(sits.env$config[[s]]["MOD13Q1"])
        return(timeline)
    }

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
        timeline <- lubridate::as_date(sits.env$config[[s]][[name]])

        ensurer::ensure_that(timeline, length(.) > 0,
                             err_desc = paste0("Could not retrieve timeline for coverage ", name))
    }

    return(lubridate::as_date(timeline))
}

#' @title Retrieve the vector of coeficientes for brightness component of tasseled cap
#' @name .sits_get_tcap_brightness
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param satellite      Name of satellite (or sensor).
#' @return Named vector of brightness coefficients.
.sits_get_tcap_brightness <- function(satellite = "MODIS"){
    if (satellite == "MODIS")
        bands <- c("blue", "green", "red", "nir", "nir2", "mir1", "mir")
    else {
        if (satellite == "LANDSAT8")
            bands <- c("blue", "green", "red", "nir", "swir1", "swir2")
        else {
            message("unable to retrieve tasseled cap coefficients")
            return(invisible(FALSE))
        }
    }

    coef <- vector()

    for (i in 1:length(bands)) {
        coef[length(coef) + 1] <- as.double(sits.env$config$tasseled_cap_coef[[satellite]]$brightness[[bands[i]]])
    }


    names(coef) <- bands
    return(coef)
}

#' @title Retrieve the vector of coeficientes for brightness component of tasseled cap
#' @name .sits_get_tcap_greenness
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param satellite      Name of ssatellite (or sensor).
#' @return Named vector of greenness coefficients.
.sits_get_tcap_greenness <- function(satellite = "MODIS"){
    if (satellite == "MODIS")
        bands <- c("blue", "green", "red", "nir", "nir2", "mir1", "mir")
    else {
        if (satellite == "LANDSAT8")
            bands <- c("blue", "green", "red", "nir", "swir1", "swir2")
        else {
            message("unable to retrieve tasseled cap coefficients")
            return(invisible(FALSE))
        }
    }

    coef <- vector()

    for (i in 1:length(bands)) {
        c <- as.double(sits.env$config$tasseled_cap_coef[[satellite]]$greenness[[bands[i]]])
        coef[length(coef) + 1] <- c
    }
    names(coef) <- bands
    return(coef)
}

#' @title Retrieve the vector of coeficientes for wetness component of tasseled cap
#' @name .sits_get_tcap_wetness
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param satellite      Name of ssatellite (or sensor).
#' @return Named vector of greenness coefficients.
.sits_get_tcap_wetness <- function(satellite = "MODIS"){
    if (satellite == "MODIS")
        bands <- c("blue", "green", "red", "nir", "nir2", "mir1", "mir")
    else {
        if (satellite == "LANDSAT8")
            bands <- c("blue", "green", "red", "nir", "swir1", "swir2")
        else {
            message("unable to retrieve tasseled cap coefficients")
            return(invisible(FALSE))
        }
    }

    coef <- vector()

    for (i in 1:length(bands)) {
        c <- as.double(sits.env$config$tasseled_cap_coef[[satellite]]$wetness[[bands[i]]])
        coef[length(coef) + 1] <- c
    }
    names(coef) <- bands
    return(coef)
}

#' @title Check that the service is valid
#' @name .sits_check_service
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param service        Name of the time series service.
.sits_check_service <- function(service){
    # Ensure that the service is available
    ensurer::ensure_that(service, (.) %in% sits.env$config$ts_services,
                         err_desc = "sits_getdata: Invalid time series service")
    return(TRUE)
}
