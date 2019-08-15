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
#' @title Check that the service is valid
#' @name .sits_check_service
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param service        Name of the time series service.
.sits_check_service <- function(service){
    # Ensure that the service is available
    ensurer::ensure_that(service, (.) %in% sits.env$config$services,
                         err_desc = "sits_get_data: Invalid data service")
    return(TRUE)
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

#' @title Retrieve the bands associated to a service
#' @name sits_bands_service
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description Retrieve the cubes associated a service.
#' @param service  Name of a service.
#' @param name     Name of a cube
.sits_bands_service <- function(service,name) {
    ensurer::ensure_that(service, (.) == "SATVEG",
                         err_desc = "sits_bands_service only works for SATVEG")

    q <- paste0(service,"_bands")
    return(sits.env$config[[q]][[name]])
}

#' @title Retrieve the bounding box for the product available at service
#' @name .sits_bbox_service
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param service        Name of the time series service.
#' @param name           Name of the cube.
#' @return The bounding box.
.sits_bbox_service <- function(service, name){

    ensurer::ensure_that(service, (.) == "SATVEG",
                         err_desc = "sits_bbox_service only works for SATVEG")

    bbox        <- vector(length = 4)
    names(bbox) <- c("xmin", "xmax", "ymin", "ymax")

    # pre-condition
    s <- paste0(service, "_bbox")

    names(bbox) %>%
        purrr::map(function(c) {
            bbox[c] <<- sits.env$config[[s]][[name]][[c]]
        })

    return(bbox)
}

#' @title Retrieve the color associated to a class
#' @name sits_color
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description Retrieve the color associated a class label.
#' @param label  A class label.
.sits_color <- function(label) {
    rgb <- as.character(sits.env$config$colors[[label]])
    if (!(length(rgb) > 0))
        rgb <- "#737373"

    return(rgb)
}

#' @title Retrieve the cubes associated to a service
#' @name sits_cubes_service
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description Retrieve the cubes associated a service.
#' @param service  Name of a service.
.sits_cubes_service <- function(service) {
    providers <- .sits_providers(service)

    cubes.lst <-
        providers %>%
        purrr::map(function(p){
            q <- paste0(p,"_cubes")
            c <- sits.env$config[[q]]
        })
    return(unlist(cubes.lst))
}
#' @title Retrieve the maximum values for a given band
#' @name .sits_maximum_values
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param sensor         Name of the sensor
#' @param bands          Vector of bands.
#' @return The maximum values.
.sits_maximum_values <- function(sensor, bands) {

    # create a string to query for the maximum values
    maximum_values <- vector()
    bands %>%
        purrr::map(function(b) {
            maximum_values[b] <<- as.numeric(sits.env$config[[sensor]][["maximum_value"]][[b]])
        })

    #post-condition
    ensurer::ensure_that(maximum_values, !purrr::is_null(.),
                         err_desc = paste0("Configuration file has failed to find maximum values for ", sensor))

    names(maximum_values) <- bands
    return(maximum_values)
}

#' @title Retrieve the estimated value of R memory bloat
#' @name sits_memory_bloat
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description Retrieve the expected memory bloat associated to R.
.sits_memory_bloat <- function() {
    return(sits.env$config$R_memory_bloat)
}

#' @title Retrieve the minimum values for a given band
#' @name .sits_minimum_values
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param sensor           Name of the sensor
#' @param bands            Bands provided by the sensor
#' @return The minimum values.
.sits_minimum_values <- function(sensor, bands) {

    # create a string to query for  values
    minimum_values <- vector()
    bands %>%
        purrr::map(function(b) {
            minimum_values[b] <<- as.numeric(sits.env$config[[sensor]][["minimum_value"]][[b]])
        })

    #post-condition
    ensurer::ensure_that(minimum_values, !purrr::is_null(.),
                         err_desc = paste0("Configuration file has failed to find minimum values for ", sensor))

    names(minimum_values) <- bands
    return(minimum_values)
}


#' @title Retrieve the missing values for bands of a sensor
#' @name .sits_missing_values
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param sensor         Name of the sensor
#' @param bands          Vector of bands.
#' @return The missing values.
.sits_missing_values <- function(sensor, bands) {
    # create a string to query for the missing values
    missing_values <- vector()
    bands %>%
        purrr::map(function(b) {
            missing_values[b] <<- as.numeric(sits.env$config[[sensor]][["missing_value"]][[b]])
        })
    #post-condition
    ensurer::ensure_that(missing_values, !purrr::is_null(.),
                         err_desc = paste0("Configuration file has no missing values for sensor ", sensor))

    names(missing_values) <- bands
    return(missing_values)
}

#' @title Retrieve the projection for the product available at service
#' @name .sits_projection_service
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param service        Name of the time series service.
#' @param name           Name of the cube.
#' @return CRS PROJ4 infomation.
.sits_projection_service <- function(service, name) {
    # pre-condition
    ensurer::ensure_that(service, (.) == "SATVEG",
                         err_desc = "sits_projection_service only works for SATVEG")
    # create a string to store the query
    s <- paste0(service, "_crs")
    crs <- sits.env$config[[s]][[name]]

    #post-condition
    ensurer::ensure_that(crs, length(.) > 0,
                         err_desc = paste0("Projection information for cube ", name, " of service ", service, " not available"))
    return(crs)
}
#' @title List the data services available
#' @name .sits_providers
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @param service Name of the web service
#'
#' @return List of providers associated to a service
.sits_providers <- function(service) {
    p <- paste0(service,"_providers")
    return(sits.env$config[[p]])
}

#' @title Retrieve the pixel spatial resolution for a data cube
#' @name .sits_resolution
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param sensor         Name of the sensor.
#' @return Vector of (xres, yres).
.sits_resolution <- function(sensor) {

    # create a string to query for the resolution
    res          <- vector(length = 2)
    names(res)  <- c("xres", "yres")

    names(res) %>%
        purrr::map(function(c){
            res[c] <<- sits.env$config[[sensor]][["resolution"]][[c]]
        })

    #post-condition
    ensurer::ensure_that(res["xres"], as.numeric(.) > 0,
                         err_desc = paste0("Horizontal resolution not available for sensor ", sensor))
    ensurer::ensure_that(res["yres"], as.numeric(.) > 0,
                         err_desc = paste0("Vertical resolution not available for sensor ", sensor))

    return(res)
}
#' @title List the satellites supported
#' @name .sits_satellites
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @return List of satellites supported by SITS
.sits_satellites <- function() {
    return(sits.env$config[["supported_satellites"]])
}

#' @title retrieve the satellite associated to a given product
#' @name .sits_satellite_product
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @param product Name of the product
#'
#' @return List of providers associated to a service
.sits_satellite_product <- function(product) {
    p <- paste0(product,"_satellite")
    s <- sits.env$config[[p]]
    #post-condition
    ensurer::ensure_that(s, !purrr::is_null(.),
                         err_desc = paste0("Could not find satellite for product ", product))
    return(s)
}
#' @title Retrieve the scale factor for a given band for a data cube
#' @name .sits_scale_factors
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param sensor         Name of the sensor.
#' @param bands          Vector of bands.
#' @return Vector of scale factors.
.sits_scale_factors <- function(sensor, bands) {
    scale_factors <- vector()
    bands %>%
        purrr::map(function(b) {
            scale_factors[b] <<- as.numeric(sits.env$config[[sensor]][["scale_factor"]][[b]])
        })
    names(scale_factors) <- bands
    #post-condition
    ensurer::ensure_that(scale_factors, !purrr::is_null(.),
                         err_desc = paste0("Configuration file has no scale factors for sensor", sensor))
    return(scale_factors)
}

#' @title retrieve the sensor associated to a product
#' @name .sits_sensor_product
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @param product Name of the product
#'
#' @return List of providers associated to a service
.sits_sensor_product <- function(product) {
    p <- paste0(product,"_sensor")
    s <- sits.env$config[[p]]
    #post-condition
    ensurer::ensure_that(s, !purrr::is_null(.),
                         err_desc = paste0("Could not find sensor for product ", product))
    return(s)
}

#' @title List the sensors supported per satellite
#' @name .sits_sensors_satellite
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @param satellite  Name of the satellite
#'
#' @return List of sensors associated to a satellite that are supported by SITS
.sits_sensors_satellite <- function(satellite) {
    q <- paste0(satellite, "_sensors")
    return(sits.env$config[[q]])
}

#' @title Retrieve the time series server for the product
#' @name .sits_server
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param service        Name of the data service
#' @param provider       URL of the service or name of the provider
#' @return A string with the server URL that provides the service.
.sits_server <- function(service, provider = NULL) {
    # pre-condition
    ensurer::ensure_that(service, (.) %in% sits.env$config$services,
                         err_desc = "Service not available - check configuration file")

    # Provider must be consistent

    # if provider is not given, take the first one as default
    if (purrr::is_null(provider)) {
        p <- paste0(service,"_providers")
        provider  <- sits.env$config[[p]][[1]]
    }

    # try to see if user gave a URL or a the name of a provider
    if (length(grep("http", provider)) != 0)
        return(provider)
    else {
        # get the server URL for the provider from the configuration file
        s <- paste0(provider,"_server")
        serverURL <- sits.env$config[[s]]
        return(serverURL)
    }
}

#' @title List the data services available
#' @name .sits_services
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @return List of services supported by SITS
.sits_services <- function() {
        return(sits.env$config$services)
}

#' @title Retrieve the size of the product for a given service
#' @name .sits_size_service
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param service        Name of the time series service.
#' @param name           Name of the cube.
#' @param r_obj          R object associated with the cube.
#' @return Vector of (nrows, ncols).
.sits_size_service <- function(service, name, r_obj = NA) {

    # pre-condition
    ensurer::ensure_that(service, (.) == "SATVEG",
                         err_desc = "sits_size_service only works for SATVEG")
    size         <- vector(length = 2)
    names(size)  <- c("nrows", "ncols")

    # get the size from the configuration file
    i1  <- paste0(service,"_size")

    names(size) %>%
        purrr::map(function(c){
            size[c] <<- sits.env$config[[i1]][[name]][[c]]
        })

    #post-condition
    ensurer::ensure_that(size["nrows"], as.integer(.) > 0,
                         err_desc = paste0("Number of rows not available for cube ",
                                           name, " for service ", service))
    ensurer::ensure_that(size["ncols"], as.integer(.) > 0,
                         err_desc = paste0("Number of cols not available for cube ",
                                           name, " for service ", service))

    return(size)
}

#' @title List all the sensors supported
#' @name .sits_supported_sensors
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @return List of sensors supported by SITS
.sits_supported_sensors <- function() {
    return(sits.env$config[["supported_sensors"]])
}



#' @title Retrieve the default timeline for a product for a given service
#' @name .sits_guess_timeline
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param service        Name of the web service.
#' @param name           Name of the product (a cube).
#' @return The default timeline associated to the service
.sits_guess_timeline <- function(service, name){
    if (service == "LOCALHOST") {
        message("Please provide timeline for raster data: will use default timeline which might be wrong")
        s <- paste0("RASTER_timeline")
        timeline <- lubridate::as_date(sits.env$config[[s]]["MOD13Q1"])
        return(timeline)
    }

    if (service == "WTSS") {
        URL  <- .sits_server(service)
        # obtains information about the available cubes
        wtss.obj     <- wtss::WTSS(URL)
        cubes.vec    <- wtss::listCoverages(wtss.obj)

        # is the cube in the list of cubes?
        ensurer::ensure_that(name, (.) %in% cubes.vec,
                             err_desc = "sits_guess_timeline: cube is not available in the WTSS server")

        # describe the cube
        cov.lst    <- wtss::describeCoverage(wtss.obj, name)
        cov        <- cov.lst[[name]]

        # temporal extent
        timeline <- cov$timeline

    }
    else {
        s <- paste0(service, "_timeline")
        timeline <- lubridate::as_date(sits.env$config[[s]][[name]])

        ensurer::ensure_that(timeline, length(.) > 0,
                             err_desc = paste0("Could not retrieve timeline for cube ", name))
    }

    return(lubridate::as_date(timeline))
}

#' @title Retrieve the vector of coeficientes for brightness component of tasseled cap
#' @name .sits_tcap_brightness
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param sensor     Name of sensor
#' @return Named vector of brightness coefficients.
.sits_tcap_brightness <- function(sensor = "MODIS"){
    if (sensor == "MODIS")
        bands <- c("blue", "green", "red", "nir", "nir2", "mir1", "mir")
    else {
        if (sensor == "OLI")
            bands <- c("blue", "green", "red", "nir", "swir1", "swir2")
        else {
            stop("Unable to retrieve tasseled cap coefficients")
        }
    }
    coef <- vector()

    for (i in 1:length(bands))
        coef[length(coef) + 1] <- as.double(sits.env$config$tasseled_cap_coef[[sensor]]$brightness[[bands[i]]])

    names(coef) <- bands
    return(coef)
}

#' @title Retrieve the vector of coeficientes for brightness component of tasseled cap
#' @name .sits_tcap_greenness
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param sensor      Name of ssatellite (or sensor).
#' @return Named vector of greenness coefficients.
.sits_tcap_greenness <- function(sensor = "MODIS"){
    if (sensor == "MODIS")
        bands <- c("blue", "green", "red", "nir", "nir2", "mir1", "mir")
    else {
        if (sensor == "OLI")
            bands <- c("blue", "green", "red", "nir", "swir1", "swir2")
        else {
            stop("Unable to retrieve tasseled cap coefficients")
        }
    }
    coef <- vector()

    for (i in 1:length(bands)) {
        c <- as.double(sits.env$config$tasseled_cap_coef[[sensor]]$greenness[[bands[i]]])
        coef[length(coef) + 1] <- c
    }
    names(coef) <- bands
    return(coef)
}

#' @title Retrieve the vector of coeficientes for wetness component of tasseled cap
#' @name .sits_tcap_wetness
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param sensor     Name of sensor.
#' @return Named vector of wetness coefficients.
.sits_tcap_wetness <- function(sensor = "MODIS"){
    if (sensor == "MODIS")
        bands <- c("blue", "green", "red", "nir", "nir2", "mir1", "mir")
    else {
        if (sensor == "OLI")
            bands <- c("blue", "green", "red", "nir", "swir1", "swir2")
        else {
            stop("Unable to retrieve tasseled cap coefficients")
        }
    }
    coef <- vector()

    for (i in 1:length(bands)) {
        c <- as.double(sits.env$config$tasseled_cap_coef[[sensor]]$wetness[[bands[i]]])
        coef[length(coef) + 1] <- c
    }
    names(coef) <- bands
    return(coef)
}
#' @title Try a best guess for the type of sensor/satellite
#' @name .sits_guess_satellite
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description    Based on the projection, tries to guess what is the satellite.
#'
#' @param r_obj      The R object that describes the file
#' @return Name of the satellite .
.sits_guess_satellite <- function(r_obj) {

    crs   = as.character(raster::crs(r_obj))
    # if the projection is UTM, guess it's a LANDSAT data set
    if (stringr::str_detect(crs, "utm")) {
        satellite <- "LANDSAT"
    }
    # if the projection is sinusoidal, guess it's a TERRA data set
    else if (stringr::str_detect(crs, "sinu")) {
        satellite <- "TERRA"
    }
    else {
        satellite <- "UNKNOWN"
    }

    return(satellite)
}
#' @title Try a best guess for the type of sensor
#' @name .sits_guess_sensor
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description    Based on the satellite, tries to guess what is the sensor
#'
#' @param satellite      Name of the sensor
#' @return               A best guess for the sensor
#'
.sits_guess_sensor <- function(satellite) {

    ensurer::ensure_that(satellite, (.) %in% .sits_satellites(),
                         err_desc = "satellite not supported by SITS - please edit configuration file")

    q <- paste0(satellite,"_sensors")
    sensor <- sits.env$config[[q]][1]

    ensurer::ensure_that(sensor, !purrr::is_null(.),
                         err_desc = "could not find default sensor for satellite - please edit configuration file")
    return(sensor)
}

