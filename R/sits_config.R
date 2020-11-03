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
    user_yml_file   <- "~/.sits/config.yml"

    if (file.exists(user_yml_file)) {
        config_user     <- config::get(file = user_yml_file)
        sits.env$config <- config::merge(sits.env$config, config_user)
    }

    return(invisible(sits.env$config))
}

#' @title Information about configuration file
#' @name sits_config_info
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Displays the local of sits configuration file. For details
#' on how to set the configuration file, use \code{\link[sits]{sits_config}}.
#'
#' @return a message
#' @examples
#' sits_config_info()
#' @export

sits_config_info <- function() {
    # the default configuration file
    yml_file <- system.file("extdata", "config.yml", package = "sits")

    msg1 <- paste0("Using configuration file: ", yml_file)

    # try to find a valid user configuration file
    user_yml_file   <- "~/.sits/config.yml"
    if (file.exists(user_yml_file))
        msg2 <- paste0("Additional configurations found in ", user_yml_file)
    else
        msg2 <- paste0("Users can provide additional configurations in ",
                   user_yml_file)

    return(message(msg1, "\n", msg2))
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
#' @title Convert bands names from cube to SITS
#' @name .sits_config_band_names_convert
#' @keywords internal
#'
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Convert the name of the band used by the origin data cube
#'              to the name used by SITS
#' @param satellite      Name of the satellite
#' @param sensor         Name of sensor
#' @param bands_files    Bands available in the files
#' @return               Name of the bands used in SITS (named vector)
#'
.sits_config_band_names_convert <- function(satellite, sensor, bands_files){

    # Precondition
    .sits_raster_satellite_sensor(satellite, sensor)

    # bands used by SITS
    bands_sits <- sits.env$config[[sensor]][["bands"]][["SITS"]]
    # Are these the right names?
    if (all(bands_files %in% bands_sits)) {
        bands_sits <- bands_sits[match(bands_files, bands_sits)]
        names(bands_sits) <- bands_files
      return(bands_sits)
    }

    # bands used by BDC
    bands_bdc <- sits.env$config[[sensor]][["bands"]][["BDC_TILE"]]
    # are the names those used by BDC?
    if (all(bands_files %in% bands_bdc)) {
        idx <- match(bands_files, bands_bdc)
        bands_bdc <- bands_bdc[idx]
        bands_sits <- bands_sits[idx]
        names(bands_sits) <- bands_bdc
        return(bands_sits)
    }
    # bands used by AWS
    bands_aws <- sits.env$config[[sensor]][["bands"]][["AWS"]]
    # are the names those used by AWS?
    if (all(bands_files %in% bands_aws)) {
        idx <- match(bands_files, bands_aws)
        bands_aws <- bands_aws[idx]
        bands_sits <- bands_sits[idx]
        names(bands_sits) <- bands_aws
        return(bands_sits)
    }

    stop("band names unknown by SITS configuration file. Please fix it")
    return(NULL)
}
#' @title Convert bands names from BDC to SITS
#' @name .sits_config_band_names_convert_bdc
#' @keywords internal
#'
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Convert the name of the band used by BDC
#'              to the name used by SITS
#' @param satellite      Name of the satellite
#' @param sensor         Name of sensor
#' @return               Name of the bands used in SITS (named vector)
#'
.sits_config_band_names_convert_bdc <- function(satellite, sensor){

  # Precondition
  .sits_raster_satellite_sensor(satellite, sensor)

  # bands used by SITS
  bands_sits <- sits.env$config[[sensor]][["bands"]][["SITS"]]

  # bands used by BDC
  bands_bdc <- sits.env$config[[sensor]][["bands"]][["BDC_TILE"]]

  names(bands_sits) <- bands_bdc
  return(bands_sits)

}
#' @title Directory to read the BDC STAC catalogue
#' @name .sits_config_bdc_stac
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @return directory where BDC is accessible on the web
.sits_config_bdc_stac <- function() {

  return(sits.env$config$bdc_stac)
}
#' @title Test if BDC STAC catalogue is available
#' @name .sits_config_bdc_stac_access
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @param url  URL for access to the BDC STAC
#'
#' @return directory where BDC is accessible on the web
.sits_config_bdc_stac_access <- function(url) {

  if(purrr::is_null(url))
    url <- .sits_config_bdc_stac()

  return(RCurl::url.exists(url))
}
#' @title Directory to read the BDC information on the web
#' @name .sits_config_bdc_web
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @return directory where BDC is accessible on the web
.sits_config_bdc_web <- function() {

    return(sits.env$config$bdc_web)
}
#' @title Test if the BDC is working
#' @name .sits_config_bdc_web_access
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @param url  URL for access to the BDC
#'
#' @return TRUE/FALSE if BDC can be accessed
.sits_config_bdc_web_access <- function(url = NULL) {

  if (purrr::is_null(url))
    url <- .sits_config_bdc_web()

  return(RCurl::url.exists(url))
}
#' @title Directory to read the BDC information as local file
#' @name .sits_config_bdc_local
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @return directory where BDC is accessible on the web
.sits_config_bdc_local <- function() {

  return(sits.env$config$bdc_local)
}
#' @title File extension used by BDC
#' @name .sits_config_bdc_extension
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @return extension of BDC files
.sits_config_bdc_extension <- function() {
  return(sits.env$config$bdc_extension)
}


#' @title Get the name of the band used for cloud information
#' @name .sits_config_cloud_band
#' @keywords internal
#' @param satellite            satellite
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @return vector with bands available in AWS for a given resolution
.sits_config_cloud_band <- function(cube) {

  cb <- paste0(cube$sensor[1],"_CLD_BAND")
  cloud_band <- sits.env$config[["CLOUD"]][[cube$type[1]]][[cb]]
  assertthat::assert_that(!purrr::is_null(cloud_band),
                          msg = "Cloud band information not available")
  return(cloud_band)
}

#' @title Get the name of the band used for cloud information
#' @name .sits_config_cloud_valid_values
#' @keywords internal
#' @param cube          data cube
#' @author Gilberto Camara \email{gilberto.camara@@inpe.br}
#'
#' @return vector with bands available in AWS for a given resolution
.sits_config_cloud_valid_values <- function(cube) {

  cv <- paste0(cube$sensor[1],"_cld_vls")
  cloud_values <- sits.env$config[["CLOUD"]][[cube$type[1]]][[cv]]
  assertthat::assert_that(!purrr::is_null(cloud_values),
                          msg = "Cloud band values information not available")
  return(cloud_values)
}



#' @title Retrieve the color associated to a class in the configuration file
#' @name sits_config_color
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description Retrieve the color associated a class label.
#' @param label  A class label.
.sits_config_color <- function(label) {
    rgb <- as.character(sits.env$config$colors[[label]])
    if (!(length(rgb) > 0))
        rgb <- "#737373"

    return(rgb)
}

#' @title Retrieve the classes associated to data cubes known to SITS
#' @name .sits_config_cube_generic_class
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description Retrieve the class name associated to a cube type
#' @param type  Data cube type
#' @return      Class of data cube
#'
.sits_config_cube_generic_class <- function(type) {
    # check that the cube is correct
    type <- toupper(type)
    # find out which cube types are supported
    types   <- sits.env$config$cube_types
    assertthat::assert_that(type %in% types,
                            msg = "unsupported cube type")
    classes <-  sits.env$config$cube_classes_generic
    names(classes) <- types
    return(unname(classes[type]))
}
#' @title Retrieve the generic class associated to data cubes known to SITS
#' @name .sits_config_cube_specific_class
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description Retrieve the metadata class name associated to a cube type
#' @param type  Data cube type
#' @return      Class of data cube metadata
#'
.sits_config_cube_specific_class <- function(type) {
    # check that the cube is correct
    type <- toupper(type)
    # find out which cube types are supported
    types   <- sits.env$config$cube_types
    assertthat::assert_that(type %in% types,
                            msg = "unsupported cube type")
    classes <-  sits.env$config$cube_classes_specific
    names(classes) <- types
    return(unname(classes[type]))
}
#' @title Check that the type is valid, based on the configuration file
#' @name .sits_config_cube_check
#' @keywords internal
#' @author Gilberto Camara \email{gilberto.camara@@inpe.br}
#'
#' @param cube       Data cube
#' @return           true/false is the type is valid
#'
.sits_config_cube_check <- function(cube){
  # precondition
  assertthat::assert_that(!purrr::is_null(cube),
                          msg = "invalid cube")

  assertthat::assert_that(!purrr::is_null(cube[1,]$type),
                          msg = "invalid cube type")

  # find out which cube types are available
  types <- sits.env$config$cube_types
  assertthat::assert_that(cube[1,]$type %in% types,
                          msg = "sits_get_data: Invalid cube type")
  return(TRUE)
}

#' @title meta-type for data
#' @name .sits_config_data_meta_type
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @param  data    tibble (time series or cube)
#'
#' @return file path to the appended to data_dir
.sits_config_data_meta_type <- function(data) {

  if (grepl("sits", class(data)[[1]]) | grepl("patterns", class(data)[[1]])
      | grepl("predicted", class(data)[[1]]))
    return(data)
  else {
    assertthat::assert_that(!purrr::is_null(data[1,]$type),
                            msg = "data is not valid")
    #check if data is a cube
    .sits_config_cube_check(data)

    class(data) <- c("cube", class(data))
  }
    return(data)
}

#' @title Standard files for data directory for cube type
#' @name .sits_config_data_dir_path
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @param  type    cube_type
#'
#' @return file path to the appended to data_dir
.sits_config_data_dir_path <- function(type) {

  return(sits.env$config[[type]][["data_dir_path"]])
}

#' @title Standard files for data directory for cube type
#' @name .sits_config_data_parse_info
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @param  type    cube_type
#'
#' @return parsing information
.sits_config_data_parse_info <- function(type) {
  return(sits.env$config[[type]][["parse_info"]])
}
#' @title Delimiter for data type
#' @name .sits_config_data_delim
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @param  type    cube_type
#'
#' @return delimiter information
.sits_config_data_delim <- function(type){
  return(sits.env$config[[type]][["delim"]])
}
#' @title Retrieve the default sensor for the satellite
#' @name .sits_config_sensors
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description    Based on the satellite, find the default sensor
#'
#' @param satellite      Name of the satellite
#' @return               Sensors associated to the satellite
#'
.sits_config_sensors <- function(satellite) {

    assertthat::assert_that(satellite %in% .sits_config_satellites(),
        msg = "satellite not supported by SITS - edit configuration file")

    q <- paste0(satellite,"_sensors")
    sensors <- sits.env$config[[q]]

    return(sensors)
}

#' @title Retrieve the maximum values for a given band
#' @name .sits_config_maximum_values
#' @keywords internal
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
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description Retrieve the expected memory bloat associated to R.
.sits_config_memory_bloat <- function() {
    return(sits.env$config$R_memory_bloat)
}

#' @title Retrieve the minimum values for a given band
#' @name .sits_config_minimum_values
#' @keywords internal
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
#' @keywords internal
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
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description Retrieve the expected memory bloat associated to R.
.sits_config_processing_bloat <- function() {
    return(sits.env$config$R_processing_bloat)
}


#' @title Retrieve the pixel spatial resolution for a data cube
#' @name .sits_config_resolution
#' @keywords internal
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
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @return List of satellites supported by SITS
.sits_config_satellites <- function() {
    return(sits.env$config[["supported_satellites"]])
}


#' @title Get the URL to be used to test for SATVEG access
#' @name .sits_config_satveg_access
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @return URL to test SATVEG access
.sits_config_satveg_access <- function() {
  q <- "SATVEG_EMBRAPA_test"
  return(sits.env$config[[q]])
}

#' @title Retrieve the bands associated to SATVEG
#' @name sits_config_satveg_bands
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description Retrieve the cubes associated to the SATVEG service
#' @return         Names of SATVEG bands
.sits_config_satveg_bands <- function() {

    q <- paste0("SATVEG_bands")
    return(sits.env$config[[q]][["terra"]])
}

#' @title Retrieve the cubes associated to SATVEG
#' @name sits_config_satveg_cubes
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description Retrieve the cubes associated to SATVEG.
.sits_config_satveg_cubes <- function() {

    c <- sits.env$config[["SATVEG-EMBRAPA_cubes"]]

  return(c)
}
#' @title Retrieve the bounding box for SATVEG
#' @name .sits_config_satveg_bbox
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param name           Name of the cube.
#' @return The bounding box.
.sits_config_satveg_bbox <- function(name){

    bbox        <- vector(length = 4)
    names(bbox) <- c("xmin", "xmax", "ymin", "ymax")

    names(bbox) %>%
        purrr::map(function(c) {
        bbox[c] <<- sits.env$config[["SATVEG_bbox"]][[name]][[c]]
        })

    return(bbox)
}

#' @title Retrieve the projection for SATVEG service
#' @name .sits_config_satveg_projection
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param name           Name of the cube.
#' @return               CRS PROJ4 information.
.sits_config_satveg_projection <- function(name) {

    crs <- sits.env$config[["SATVEG_crs"]][[name]]

    #post-condition
    assertthat::assert_that(length(crs) > 0,
                      msg = paste0("Projection information for cube ", name,
                                  " of service SATVEG not available"))
    return(crs)
}

#' @title Retrieve the size of the cube for SATVEG
#' @name .sits_config_satveg_size
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param name           Name of the cube.
#' @return Vector of (nrows, ncols).
.sits_config_satveg_size <- function(name) {

    size         <- vector(length = 2)
    names(size)  <- c("nrows", "ncols")

    names(size) %>%
      purrr::map(function(c){
          size[c] <<- sits.env$config[["SATVEG_size"]][[name]][[c]]
      })

    #post-condition
    assertthat::assert_that(as.integer(size["nrows"]) > 0,
                          msg = paste0("Number of rows not available for cube ",
                                       name))
    assertthat::assert_that(as.integer(size["ncols"]) > 0,
                          msg = paste0("Number of cols not available for cube ",
                                       name))

    return(size)
}

#' @title Get the URL to be used for SATVEG access
#' @name .sits_config_satveg_url
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @return URL to test SATVEG access
.sits_config_satveg_url <- function() {
  q <- "SATVEG-EMBRAPA_server"
  return(sits.env$config[[q]])
}

#' @title Obtain the name of the bands used by a cube or by SITS
#' @name .sits_config_band_names
#' @keywords internal
#'
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Obtain the name of the bands used by a cube or by SITS
#' @param sensor         Name of the sensor
#' @param type           Type of the data cube (or "SITS")
#'
#' @return               Name of the bands used in that data cube or by SITS
#'
.sits_config_band_names <- function(sensor, type){

    bands <- sits.env$config[[sensor]][["bands"]][[type]]
    assertthat::assert_that(!purrr::is_null(bands),
                      msg = "band names inconsistent with cube type")

    return(bands)
}

#' @title Convert bands names from cube to SITS
#' @name .sits_config_band_names_convert
#' @keywords internal
#'
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Convert the name of the band used by the origin data cube
#'              to the name used by SITS
#' @param satellite      Name of the satellite
#' @param sensor         Name of sensor
#' @param bands_files    Bands available in the files
#' @return               Name of the bands used in SITS (named vector)
#'
.sits_config_band_names_convert <- function(satellite, sensor, bands_files){

  # Precondition
  .sits_raster_satellite_sensor(satellite, sensor)

  # bands used by SITS
  bands_sits <- sits.env$config[[sensor]][["bands"]][["SITS"]]
  # Are these the right names?
  if (all(bands_files %in% bands_sits)) {
    bands_sits <- bands_sits[match(bands_files, bands_sits)]
    names(bands_sits) <- bands_files
    return(bands_sits)
  }

  # bands used by BDC
  bands_bdc <- sits.env$config[[sensor]][["bands"]][["BDC_TILE"]]
  # are the names those used by BDC?
  if (all(bands_files %in% bands_bdc)) {
      idx <- match(bands_files, bands_bdc)
      bands_bdc <- bands_bdc[idx]
      bands_sits <- bands_sits[idx]
      names(bands_sits) <- bands_bdc
      return(bands_sits)
  }
  # bands used by AWS
  bands_aws <- sits.env$config[[sensor]][["bands"]][["AWS"]]
  # are the names those used by AWS?
  if (all(bands_files %in% bands_aws)) {
      idx <- match(bands_files, bands_aws)
      bands_aws  <- bands_aws[idx]
      bands_sits <- bands_sits[idx]
      names(bands_sits) <- bands_aws
      return(bands_sits)
  }

  stop("band names unknown by SITS configuration file. Please fix it")
  return(NULL)
}


#' @title Get the bucket where Sentinel-2 level 2A images are available in AWS
#' @name .sits_config_sentinel_aws_bucket
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @return name of the bucket
.sits_config_sentinel_aws_bucket <- function() {
  s <- "S2_L2A_AWS"
  return(sits.env$config[[s]][["bucket"]])
}
#' @title File to test access to Sentinel-2 level 2A images in AWS
#' @name .sits_config_sentinel_aws_test_file
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @return name of the bucket
.sits_config_sentinel_aws_test_file <- function() {
    s <- "S2_L2A_AWS"
   return(sits.env$config[[s]][["test_file"]])
}
#' @title Get the the resolutions for Sentinel-2 ARD in AWS
#' @name .sits_config_sentinel_aws_resolutions
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @return vector with names of the resolutions available in AWS for S2 L2A
.sits_config_sentinel_aws_resolutions <- function() {
    return(sits.env$config[["S2_L2A_AWS"]][["resolutions"]])
}
#' @title Get the the bands in AWS for Sentinel-2 ARD given the resolution
#' @name .sits_config_sentinel_bands
#' @keywords internal
#' @param resolution       Resolution of the bands
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @return vector with bands available in AWS for a given resolution
.sits_config_sentinel_bands <- function(resolution) {
    s <- "S2_L2A_AWS"
    assertthat::assert_that(resolution %in% sits.env$config[[s]][["resolutions"]],
                          msg = "Sentinel-2 in AWS - wrong resolution")

    r <- paste0(resolution,"_bands")
    return(sits.env$config[[s]][[r]])
}


#' @title Retrieve the scale factor for a given band for a data cube
#' @name .sits_config_scale_factors
#' @keywords internal
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
