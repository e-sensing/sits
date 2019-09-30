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
#' To see the contents of the configuration file, please use \code{\link[sits]{sits_config_show}}.
#'
#' @return A list with the configuration parameters used by sits.
#' @examples
#' # create configurtion file
#' config_sits <- sits_config()
#' # show configuration file
#' sits_config_show()
#' @export
sits_config <- function() {
    # run the default configuration file
    yml_file <- system.file("extdata", "config.yml", package = "sits")

    # check that the file is valid
    ensurer::ensure_that(yml_file, !purrr::is_null(.),
        err_desc = "sits_config : invalid configuration file")

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
#' @name sits_config_show
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Displays the contents of sits configuration file. For more details
#' on how to set the configuration file, please use \code{\link[sits]{sits_config}}.
#'
#' @return List with the configuration parameters used by sits.
#' @examples
#' sits_config_show()
#' @export
sits_config_show <- function() {
    # retrieve the basic configuration file
    yml_file <- system.file("extdata", "config.yml", package = "sits")
    # check that the file is valid
    ensurer::ensure_that(yml_file, !purrr::is_null(.),
        err_desc = "sits_config: Invalid configuration file")
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

#' @title Retrieve the bands associated to a service in the configuration file
#' @name sits_config_bands
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description Retrieve the cubes associated a service.
#' @param service  Name of a service.
#' @param name     Name of a cube
.sits_config_bands <- function(service,name) {
    ensurer::ensure_that(service, (.) == "SATVEG",
                         err_desc = "sits_config_bands only works for SATVEG")

    q <- paste0(service,"_bands")
    return(sits.env$config[[q]][[name]])
}

#' @title Retrieve the bounding box for the product available at service based on the configuration file
#' @name .sits_config_bbox
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param service        Name of the time series service.
#' @param name           Name of the cube.
#' @return The bounding box.
.sits_config_bbox <- function(service, name){

    ensurer::ensure_that(service, (.) == "SATVEG",
                         err_desc = "sits_config_bbox only works for SATVEG")

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
#' @title Check that the service is valid, based on the configuration file
#' @name .sits_config_check
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param service        Name of the time series service.
.sits_config_check <- function(service){

    # find out which services are available
    services <- sits.env$config$services
    # Ensure that the service is available
    ensurer::ensure_that(service, (.) %in% services,
                         err_desc = "sits_get_data: Invalid data service")
    return(TRUE)
}
#' @title Retrieve the color associated to a class in the configuration file
#' @name sits_config_color
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description Retrieve the color associated a class label.
#' @param label  A class label.
.sits_config_color <- function(label) {
    rgb <- as.character(sits.env$config$colors[[label]])
    if (!(length(rgb) > 0))
        rgb <- "#737373"

    return(rgb)
}

#' @title Retrieve the cubes associated to a service based on the configuration file
#' @name sits_config_cubes
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description Retrieve the cubes associated a service.
#' @param service  Name of a service.
.sits_config_cubes <- function(service) {
    providers <- .sits_config_providers(service)

    cubes.lst <-
        providers %>%
        purrr::map(function(p){
            q <- paste0(p,"_cubes")
            c <- sits.env$config[[q]]
        })
    return(unlist(cubes.lst))
}
#' @title Retrieve the default sensor for the satellite, based on the configuration file
#' @name .sits_config_default_sensor
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description    Based on the satellite, find the default sensor based on the configuration file
#'
#' @param satellite      Name of the satellite
#' @return               A best guess for the sensor
#'
.sits_config_default_sensor <- function(satellite) {

    ensurer::ensure_that(satellite, (.) %in% .sits_config_satellites(),
        err_desc = "satellite not supported by SITS - edit configuration file")

    q <- paste0(satellite,"_sensors")
    sensor <- sits.env$config[[q]][1]

    ensurer::ensure_that(sensor, !purrr::is_null(.),
        err_desc = "unknown sensor - edit configuration file")
    return(sensor)
}
#' @title Retrieve the maximum values for a given band, based on the configuration file
#' @name .sits_config_maximum_values
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param sensor         Name of the sensor
#' @param bands          Vector of bands.
#' @return The maximum values.
.sits_config_maximum_values <- function(sensor, bands) {

    # create a string to query for the maximum values
    maximum_values <- vector()
    bands %>%
        purrr::map(function(b) {
            maximum_values[b] <<- as.numeric(sits.env$config[[sensor]][["maximum_value"]][[b]])
        })

    #post-condition
    ensurer::ensure_that(maximum_values, !purrr::is_null(.),
        err_desc = paste0("Missing maximum values for ", sensor,
                          " edit configuration file"))

    names(maximum_values) <- bands
    return(maximum_values)
}

#' @title Retrieve the estimated value of R memory bloat, based on the configuration file
#' @name .sits_config_memory_bloat
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description Retrieve the expected memory bloat associated to R.
.sits_config_memory_bloat <- function() {
    return(sits.env$config$R_memory_bloat)
}

#' @title Retrieve the minimum values for a given band, based on the configuration file
#' @name .sits_config_minimum_values
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param sensor           Name of the sensor
#' @param bands            Bands provided by the sensor
#' @return The minimum values.
.sits_config_minimum_values <- function(sensor, bands) {

    # create a string to query for  values
    min_val <- vector()
    bands %>%
        purrr::map(function(b) {
        min_val[b] <<- as.numeric(sits.env$config[[sensor]][["minimum_value"]][[b]])
    })

    #post-condition
    ensurer::ensure_that(min_val, !purrr::is_null(.),
        err_desc = paste0("No minimum values for ", sensor,
                          " edit configuration files"))

    names(min_val) <- bands
    return(min_val)
}


#' @title Retrieve the missing values for bands of a sensor, based on the configuration file
#' @name .sits_config_missing_values
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param sensor         Name of the sensor
#' @param bands          Vector of bands.
#' @return The missing values.
.sits_config_missing_values <- function(sensor, bands) {
    # create a string to query for the missing values
    mis_val <- vector()
    bands %>%
      purrr::map(function(b) {
        mis_val[b] <<- as.numeric(sits.env$config[[sensor]][["missing_value"]][[b]])
    })
    #post-condition
    ensurer::ensure_that(mis_val, !purrr::is_null(.),
        err_desc = paste0("No missing values for sensor ", sensor,
                          " edit configuration file"))

    names(mis_val) <- bands
    return(mis_val)
}

#' @title Retrieve the estimated value of R memory bloat, based on the configuration file
#' @name .sits_config_processing_bloat
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description Retrieve the expected memory bloat associated to R.
.sits_config_processing_bloat <- function() {
    return(sits.env$config$R_processing_bloat)
}

#' @title Retrieve the projection for the product available at service
#' @name .sits_config_projection
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param service        Name of the time series service.
#' @param name           Name of the cube.
#' @return CRS PROJ4 infomation.
.sits_config_projection <- function(service, name) {
    # pre-condition
    ensurer::ensure_that(service, (.) == "SATVEG",
        err_desc = "sits_config_projection only works for SATVEG")
    # create a string to store the query
    s <- paste0(service, "_crs")
    crs <- sits.env$config[[s]][[name]]

    #post-condition
    ensurer::ensure_that(crs, length(.) > 0,
        err_desc = paste0("Projection information for cube ", name,
                          " of service ", service, " not available"))
    return(crs)
}
#' @title List the data providers available in the configuration file
#' @name .sits_config_providers
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @param service Name of the web service
#'
#' @return List of providers associated to a service
.sits_config_providers <- function(service) {
    p <- paste0(service,"_providers")
    return(sits.env$config[[p]])
}

#' @title Retrieve the pixel spatial resolution for a data cube, based on the configuration file
#' @name .sits_config_resolution
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param sensor         Name of the sensor.
#' @return Vector of (xres, yres).
.sits_config_resolution <- function(sensor) {

    # create a string to query for the resolution
    res          <- vector(length = 2)
    names(res)  <- c("xres", "yres")

    names(res) %>%
        purrr::map(function(c){
            res[c] <<- sits.env$config[[sensor]][["resolution"]][[c]]
        })

    #post-condition
    ensurer::ensure_that(res["xres"], as.numeric(.) > 0,
        err_desc = paste0("Horizontal resolution unavailable for ", sensor,
                          " edit configuration file"))
    ensurer::ensure_that(res["yres"], as.numeric(.) > 0,
        err_desc = paste0("Vertical resolution unavailable for ", sensor,
                          " edit configuration file"))

    return(res)
}
#' @title List the satellites supported by the configuration file
#' @name .sits_config_satellites
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @return List of satellites supported by SITS
.sits_config_satellites <- function() {
    return(sits.env$config[["supported_satellites"]])
}

#' @title retrieve the satellite associated to a given cube
#' @name .sits_config_satellite
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @param cube Name of the data cube
#'
#' @return List of providers associated to a service
.sits_config_satellite <- function(cube) {
    p <- paste0(cube,"_satellite")
    s <- sits.env$config[[p]]
    #post-condition
    ensurer::ensure_that(s, !purrr::is_null(.),
        err_desc = paste0("Could not find satellite for cube ", cube))
    return(s)
}

#' @title Get the URL to be used to test for SATVEG access
#' @name .sits_config_satveg_access
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @return URL to test SATVEG access
.sits_config_satveg_access <- function() {
    q <- "SATVEG_EMBRAPA_test"
    return(sits.env$config[[q]])
}

#' @title Retrieve the scale factor for a given band for a data cube
#' @name .sits_config_scale_factors
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param sensor         Name of the sensor.
#' @param bands          Vector of bands.
#' @return Vector of scale factors.
.sits_config_scale_factors <- function(sensor, bands) {
    scale_f <- vector()
    bands %>%
      purrr::map(function(b) {
        scale_f[b] <<- as.numeric(sits.env$config[[sensor]][["scale_factor"]][[b]])
    })
    names(scale_f) <- bands
    #post-condition
    ensurer::ensure_that(scale_f, !purrr::is_null(.),
        err_desc = paste0("No scale factors for sensor", sensor,
                          " edit configuration file"))
    return(scale_f)
}

#' @title retrieve the sensor associated to a data cube
#' @name .sits_config_sensor
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @param cube Name of the data cube
#'
#' @return List of providers associated to a service
.sits_config_sensor <- function(cube) {
    p <- paste0(cube,"_sensor")
    s <- sits.env$config[[p]]
    #post-condition
    ensurer::ensure_that(s, !purrr::is_null(.),
                         err_desc = paste0("Could not find sensor for cube ", cube))
    return(s)
}

#' @title List the sensors supported per satellite, based on the configuration file
#' @name .sits_config_sensors
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @param satellite  Name of the satellite
#'
#' @return List of sensors associated to a satellite that are supported by SITS
.sits_config_sensors <- function(satellite) {
    q <- paste0(satellite, "_sensors")
    return(sits.env$config[[q]])
}

#' @title Retrieve the time series server for the product
#' @name .sits_config_server
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param service        Name of the data service
#' @param provider       URL of the service or name of the provider
#' @return A string with the server URL that provides the service.
.sits_config_server <- function(service, provider = NULL) {
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

#' @title List the data services available in the configuration file
#' @name .sits_config_services
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @return List of services supported by SITS
.sits_config_services <- function() {
        return(sits.env$config$services)
}

#' @title Retrieve the size of the cube for a given service, based on the configuration file
#' @name .sits_config_size
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param service        Name of the time series service.
#' @param name           Name of the cube.
#' @param r_obj          R object associated with the cube.
#' @return Vector of (nrows, ncols).
.sits_config_size <- function(service, name, r_obj = NA) {

    # pre-condition
    ensurer::ensure_that(service, (.) == "SATVEG",
                         err_desc = "sits_config_size only works for SATVEG")
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



#' @title Retrieve the default timeline based on the configuration file
#' @name .sits_config_timeline
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param service        Name of the web service.
#' @param name           Name of the product (a cube).
#' @return The default timeline associated to the service
.sits_config_timeline <- function(service, name){
    if (service == "BRICK" || service == "STACK") {
        message("Please provide timeline for raster data:
            will use default timeline from MOD13Q1
            which might be wrong")
        s <- paste0("RASTER_timeline")
        timeline <- lubridate::as_date(sits.env$config[[s]]["MOD13Q1"])
        return(timeline)
    }

    if (service == "WTSS") {
        URL  <- .sits_config_server(service)
        # obtains information about the available cubes
        wtss.obj     <- wtss::WTSS(URL)
        cubes.vec    <- wtss::listCoverages(wtss.obj)

        # is the cube in the list of cubes?
        ensurer::ensure_that(name, (.) %in% cubes.vec,
            err_desc = "sits_config_timeline: cube unavailable in WTSS server")

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
#' @name .sits_config_tcap_brightness
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param sensor     Name of sensor
#' @return Named vector of brightness coefficients.
.sits_config_tcap_brightness <- function(sensor = "MODIS"){
    if (sensor == "MODIS")
        bands <- c("blue", "green", "red", "nir", "nir2", "mir1", "mir")
    else {
        if (sensor == "OLI")
            bands <- c("blue", "green", "red", "nir", "swir1", "swir2")
        else {
            stop("Unable to retrieve tasseled cap coefficients")
        }
    }

    coef.lst <- purrr::map(bands, function(b) {
      c <- as.double(sits.env$config$tasseled_cap_coef[[sensor]]$brightness[[b]])
    })

    coef <- unlist(coef.lst)

    names(coef) <- bands
    return(coef)
}

#' @title Retrieve the vector of coeficientes for brightness component of tasseled cap
#' @name .sits_config_tcap_greenness
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param sensor      Name of ssatellite (or sensor).
#' @return Named vector of greenness coefficients.
.sits_config_tcap_greenness <- function(sensor = "MODIS"){
    if (sensor == "MODIS")
        bands <- c("blue", "green", "red", "nir", "nir2", "mir1", "mir")
    else {
        if (sensor == "OLI")
            bands <- c("blue", "green", "red", "nir", "swir1", "swir2")
        else {
            stop("Unable to retrieve tasseled cap coefficients")
        }
    }

    coef.lst <- purrr::map(bands, function(b)  {
      c <- as.double(sits.env$config$tasseled_cap_coef[[sensor]]$greenness[[b]])
    })
    coef <- unlist(coef.lst)
    names(coef) <- bands
    return(coef)
}

#' @title Retrieve the vector of coeficientes for wetness component of tasseled cap
#' @name .sits_config_tcap_wetness
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param sensor     Name of sensor.
#' @return Named vector of wetness coefficients.
.sits_config_tcap_wetness <- function(sensor = "MODIS"){
    if (sensor == "MODIS")
        bands <- c("blue", "green", "red", "nir", "nir2", "mir1", "mir")
    else {
        if (sensor == "OLI")
            bands <- c("blue", "green", "red", "nir", "swir1", "swir2")
        else {
            stop("Unable to retrieve tasseled cap coefficients")
        }
    }

    coef.lst <- purrr::map(bands, function(b)  {
      c <- as.double(sits.env$config$tasseled_cap_coef[[sensor]]$wetness[[b]])
    })
    coef <- unlist(coef.lst)
    names(coef) <- bands
    return(coef)
}



