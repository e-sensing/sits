#' @title Reads a configuration file and loads it in the main environment
#' @name sits_config
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Reads a user-specified configuration file,
#' located in a "config.yml" file in the working directory.
#' If this file is not found, reads a default package configuration file.
#' By default, the sits configuration file "config.yml" is located at
#' the directory "extdata" of the
#' package. The configuration file is an YAML file that
#' should provide at least the following parameters:
#'
#' default:
#'    ts_servers     :
#'        - "WTSS"
#'        - "SATVEG"
#'    WTSS_server    : "http://www.dpi.inpe.br/tws/wtss"
#'    SATVEG_server  : "https://www.satveg.cnptia.embrapa.br"
#'    SATVEG_account : "/satvegws/ws/perfil/ZW46IXzr4pRzJlX/"
#'
#' To see the contents of the configuration file,
#' please use \code{\link[sits]{sits_config_show}}.
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
    assertthat::assert_that(!purrr::is_null(yml_file),
        msg = "sits_config : invalid configuration file")

    # read the configuration parameters
    sits.env$config <- config::get(file = yml_file)

    # try to find a valid user configuration file
    # check if we are running in Windows
    if (.Platform$OS.type != "unix")
        user_yml_file   <- c("~/sits/config.yml")
    else
        user_yml_file   <- c("~/.sits/config.yml")

    if (file.exists(user_yml_file)) {
        config_user     <- config::get(file = user_yml_file)
        sits.env$config <- config::merge(sits.env$config, config_user)
    }

    return(invisible(sits.env$config))
}


#' @title Shows the contents of the sits configuration file
#' @name sits_config_show
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Displays the contents of sits configuration file. For details
#' on how to set the configuration file, use \code{\link[sits]{sits_config}}.
#'
#' @return List with the configuration parameters used by sits.
#' @examples
#' sits_config_show()
#' @export
sits_config_show <- function() {
    # retrieve the basic configuration file
    yml_file <- system.file("extdata", "config.yml", package = "sits")

    # check that the file is valid
    assertthat::assert_that(!purrr::is_null(yml_file),
        msg = "sits_config: Invalid configuration file")

    # try to find a valid user configuration file
    if (file.exists("~/.sits/config.yml"))
        yml_user_file <- c("~/.sits/config.yml")
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
    assertthat::assert_that(service == "SATVEG",
                         msg = "sits_config_bands only works for SATVEG")

    q <- paste0(service,"_bands")
    return(sits.env$config[[q]][[name]])
}

#' @title Retrieve the bounding box for the product available at service
#' @name .sits_config_bbox
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param service        Name of the time series service.
#' @param name           Name of the cube.
#' @return The bounding box.
.sits_config_bbox <- function(service, name){

    assertthat::assert_that(service == "SATVEG",
                         msg = "sits_config_bbox only works for SATVEG")

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
    assertthat::assert_that(service %in% services,
                         msg = "sits_get_data: Invalid data service")
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

#' @title Retrieve the cubes associated to a service
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
#' @title Retrieve the default sensor for the satellite
#' @name .sits_config_default_sensor
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description    Based on the satellite, find the default sensor
#'
#' @param satellite      Name of the satellite
#' @return               A best guess for the sensor
#'
.sits_config_default_sensor <- function(satellite) {

    assertthat::assert_that(satellite %in% .sits_config_satellites(),
        msg = "satellite not supported by SITS - edit configuration file")

    q <- paste0(satellite,"_sensors")
    sensor <- sits.env$config[[q]][1]

    assertthat::assert_that(!purrr::is_null(sensor),
        msg = "unknown sensor - edit configuration file")
    return(sensor)
}
#' @title Retrieve the maximum values for a given band
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
            maximum_values[b] <<-
              as.numeric(sits.env$config[[sensor]][["maximum_value"]][[b]])
        })

    #post-condition
    assertthat::assert_that(!purrr::is_null(maximum_values),
        msg = paste0("Missing maximum values for ", sensor,
                          " edit configuration file"))

    names(maximum_values) <- bands
    return(maximum_values)
}

#' @title Retrieve the estimated value of R memory bloat
#' @name .sits_config_memory_bloat
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description Retrieve the expected memory bloat associated to R.
.sits_config_memory_bloat <- function() {
    return(sits.env$config$R_memory_bloat)
}

#' @title Retrieve the minimum values for a given band
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
        min_val[b] <<-
          as.numeric(sits.env$config[[sensor]][["minimum_value"]][[b]])
    })

    #post-condition
    assertthat::assert_that(!purrr::is_null(min_val),
        msg = paste0("No minimum values for ", sensor,
                          " edit configuration files"))

    names(min_val) <- bands
    return(min_val)
}


#' @title Retrieve the missing values for bands of a sensor
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
        mis_val[b] <<-
          as.numeric(sits.env$config[[sensor]][["missing_value"]][[b]])
    })
    #post-condition
    assertthat::assert_that(!purrr::is_null(mis_val),
        msg = paste0("No missing values for sensor ", sensor,
                          " edit configuration file"))

    names(mis_val) <- bands
    return(mis_val)
}

#' @title Retrieve the estimated value of R memory bloat
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
    assertthat::assert_that(service == "SATVEG",
        msg = "sits_config_projection only works for SATVEG")
    # create a string to store the query
    s <- paste0(service, "_crs")
    crs <- sits.env$config[[s]][[name]]

    #post-condition
    assertthat::assert_that(length(crs) > 0,
        msg = paste0("Projection information for cube ", name,
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

#' @title Retrieve the pixel spatial resolution for a data cube
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
    assertthat::assert_that(as.numeric(res["xres"]) > 0,
        msg = paste0("Horizontal resolution unavailable for ", sensor,
                          " edit configuration file"))
    assertthat::assert_that(as.numeric(res["yres"]) > 0,
        msg = paste0("Vertical resolution unavailable for ", sensor,
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
    assertthat::assert_that(!purrr::is_null(s),
        msg = paste0("Could not find satellite for cube ", cube))
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
        scale_f[b] <<-
          as.numeric(sits.env$config[[sensor]][["scale_factor"]][[b]])
    })
    names(scale_f) <- bands
    #post-condition
    assertthat::assert_that(!purrr::is_null(scale_f),
        msg = paste0("No scale factors for sensor", sensor,
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
    assertthat::assert_that(!purrr::is_null(s),
                         msg = paste0("Could not find sensor for cube ", cube))
    return(s)
}

#' @title List the sensors supported per satellite
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
    assertthat::assert_that(service %in% sits.env$config$services,
        msg = "Service not available - check configuration file")

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

#' @title Retrieve the size of the cube for a given service
#' @name .sits_config_size
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param service        Name of the time series service.
#' @param name           Name of the cube.
#' @param r_obj          R object associated with the cube.
#' @return Vector of (nrows, ncols).
.sits_config_size <- function(service, name, r_obj = NA) {

    # pre-condition
    assertthat::assert_that(service == "SATVEG",
                         msg = "sits_config_size only works for SATVEG")
    size         <- vector(length = 2)
    names(size)  <- c("nrows", "ncols")

    # get the size from the configuration file
    i1  <- paste0(service,"_size")

    names(size) %>%
        purrr::map(function(c){
            size[c] <<- sits.env$config[[i1]][[name]][[c]]
        })

    #post-condition
    assertthat::assert_that(as.integer(size["nrows"]) > 0,
        msg = paste0("Number of rows not available for cube ",
                          name, " for service ", service))
    assertthat::assert_that(as.integer(size["ncols"]) > 0,
        msg = paste0("Number of cols not available for cube ",
                          name, " for service ", service))

    return(size)
}




#' @title Retrieve the vector of coeficientes for brightness of tasseled cap
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

#' @title Retrieve the vector of coeficientes for greeness of tasseled cap
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

#' @title Retrieve the vector of coeficientes for wetness of tasseled cap
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



