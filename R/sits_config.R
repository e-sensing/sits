#' @title Reads a configuration file and loads it in the main environment
#' @name sits_config
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Reads a user-specified configuration file,
#' located in a "config.yml" file in the working directory.
#' If this file is not found, reads a default package configuration file.
#' By default, the sits configuration file "config.yml" is located at
#' the directory "extdata" of the package.
#'
#' Users can provide additional configuration files, by specifying the
#' location of their file in the environmental variable
#' SITS_USER_CONFIG_FILE
#'
#' To see the contents of the configuration file,
#' please use \code{\link[sits]{sits_config_show}}.
#'
#' @return A list with the configuration parameters used by sits.
#' @examples
#' # create configuration file
#' config_sits <- sits_config()
#' # show configuration file
#' sits_config_show()
#' @export
sits_config <- function() {
    # run the default configuration file
    yml_file <-
        system.file("extdata", "config.yml", package = "sits")

    # check that the file is valid
    assertthat::assert_that(
        !purrr::is_null(yml_file),
        msg = "sits_config: invalid configuration file"
    )

    # read the configuration parameters
    sits_env$config <- config::get(file = yml_file)

    # try to find a valid user configuration file
    user_yml_file <- Sys.getenv("SITS_USER_CONFIG_FILE")

    if (file.exists(user_yml_file)) {
        config_user <- config::get(file = user_yml_file)
        sits_env$config <-
            config::merge(sits_env$config, config_user)
    }

    return(invisible(sits_env$config))
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

    message(paste0("Using configuration file: ", yml_file))

    # try to find a valid user configuration file
    user_yml_file <- Sys.getenv("SITS_USER_CONFIG_FILE")
    if (file.exists(user_yml_file)) {
        message(paste("Additional configurations found in", user_yml_file))
    } else {
        message(paste("To provide additional configurations, create an",
                      "yml file and set environment variable",
                      "SITS_USER_CONFIG_FILE to point to it"))
    }

    return(invisible(TRUE))
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
    yml_file <-
        system.file("extdata", "config.yml", package = "sits")

    # check that the file is valid
    assertthat::assert_that(
        !purrr::is_null(yml_file),
        msg = "sits_config: Invalid configuration file"
    )

    # try to find a valid user configuration file
    user_yml_file <- Sys.getenv("SITS_USER_CONFIG_FILE")

    # read the configuration parameters
    message("Default system configuration file")
    cat(readLines(yml_file), sep = "\n")
    if (file.exists(user_yml_file)) {
        message("User configuration file - overrides default config")
        cat(readLines(user_yml_file), sep = "\n")
    }

    return(invisible(TRUE))
}
#' @title Read the AWS default region from configuration file
#' @name .sits_config_aws_default_region
#' @param source  Source of data cube
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @return directory where BDC is accessible on the web
.sits_config_aws_default_region <- function(source) {
    return(sits_env$config[["AWS_DEFAULT_REGION"]][[source]])
}
#' @title Read the AWS end point from configuration file
#' @name .sits_config_aws_endpoint
#' @param source  Source of data cube
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @return directory where BDC is accessible on the web
.sits_config_aws_endpoint <- function(source) {
    return(sits_env$config[["AWS_ENDPOINT"]][[source]])
}
#' @title Read the AWS end point from configuration file
#' @name .sits_config_aws_request_payer
#' @param source  Source of data cube
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @return directory where BDC is accessible on the web
.sits_config_aws_request_payer <- function(source) {
    return(sits_env$config[["AWS_REQUEST_PAYER"]][[source]])
}
#' @title Directory to read the DEAFRICA STAC catalogue
#' @name .sits_config_deafrica_stac
#' @keywords internal
#'
#' @return directory where DEAFRICA is accessible on the web
.sits_config_deafrica_stac <- function() {
  return(sits_env$config$deafrica_stac)
}
#' @title Directory to read the AWS STAC catalogue
#' @name .sits_config_aws_stac
#' @keywords internal
#'
#' @return directory where DEAFRICA is accessible on the web
.sits_config_aws_stac <- function() {
  return(sits_env$config$aws_stac)
}
#' @title Retrieve the bands associated to DEAfrica STAC
#' @name .sits_config_sensor_bands
#' @param sensor   Type of sensor of cube
#' @param source  Data source
#' @keywords internal
#' @description Retrieve the cubes associated to the STAC service
#'
#' @return         Names of bands available for sensor in data source
.sits_config_sensor_bands <- function(sensor, source) {
  return(sits_env$config[[sensor]][["bands"]][[source]])
}

#' @title Convert bands names from SITS to cube
#' @name .sits_config_bands_stac_read
#' @keywords internal
#'
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Convert the name of the band used by SITS to
#'     the names used by the STAC provider
#'
#' @param stac_provider     Name of the STAC provider
#' @param sensor         Name of sensor
#' @param bands          Bands requested to be read
#' @return               Name of the bands used in the STAC provider
#'
#'
.sits_config_bands_stac_read <- function(stac_provider, sensor, bands) {

    bands_sits <- .sits_config_sensor_bands(sensor, "SITS")
    bands_stac <- .sits_config_sensor_bands(sensor, stac_provider)

    # are the bands specified as cloud provider bands or as sits bands?
    assertthat::assert_that(
        all(bands %in% bands_stac) || all(bands %in% bands_sits),
        msg = paste(".sits_config_bands_stac_read: required bands not",
                    "available in", stac_provider))
    if (all(bands %in% bands_stac))
        return(bands)
    else {
        bands_stac <- bands_stac[match(bands, bands_sits)]
        return(bands_stac)
    }
}

#' @title Convert bands names from cube to SITS
#' @name .sits_config_bands_stac_write
#' @keywords internal
#'
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Convert the name of the band used by the origin data cube
#'              to the name used by SITS
#' @param tile      Data cube tile
#' @return          Data cube tile with SITS bands
#'
#'
.sits_config_bands_stac_write <- function(tile) {

  bands_sits <- .sits_config_sensor_bands(tile$sensor, "SITS")
  bands_stac <- .sits_config_sensor_bands(tile$sensor, tile$source)
  # create a named vector
  names(bands_sits) <- bands_stac
  # are the bands specified as cloud provider bands or as sits bands?
  bands_tile <- tile$bands[[1]]
  assertthat::assert_that(
      all(bands_tile %in% bands_stac) || all(bands_tile %in% bands_sits),
      msg = paste(".sits_config_bands_stac_write: required bands not",
                  "available in", .sits_cube_source(tile))
  )

  if (!all(bands_tile %in% bands_sits)) {
      tile$bands[[1]] <- unname(bands_sits[tile$bands[[1]]])
      tile$file_info[[1]]$band <- unname(bands_sits[tile$file_info[[1]]$band])
  }

  return(tile)
}

#' @title Convert bands names from cube to SITS
#' @name .sits_config_bands_convert
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
#'
.sits_config_bands_convert <- function(satellite, sensor, bands_files) {
    # Precondition
    .sits_config_satellite_sensor(satellite, sensor)

    # bands used by SITS
    bands_sits <- sits_env$config[[sensor]][["bands"]][["SITS"]]
    # Are these the right names?
    if (all(bands_files %in% bands_sits)) {
        bands_sits <- bands_sits[match(bands_files, bands_sits)]
        names(bands_sits) <- bands_files
        return(bands_sits)
    }
    # bands used by BDC
    bands_bdc <-
        sits_env$config[[sensor]][["bands"]][["BDC"]]
    # are the names those used by BDC?
    if (all(bands_files %in% bands_bdc)) {
        idx <- match(bands_files, bands_bdc)
        bands_bdc <- bands_bdc[idx]
        bands_sits <- bands_sits[idx]
        names(bands_sits) <- bands_bdc
        return(bands_sits)
    }
    # bands used by AWS
    bands_aws <- sits_env$config[[sensor]][["bands"]][["AWS"]]
    # are the names those used by AWS?
    if (all(bands_files %in% bands_aws)) {
        idx <- match(bands_files, bands_aws)
        bands_aws <- bands_aws[idx]
        bands_sits <- bands_sits[idx]
        names(bands_sits) <- bands_aws
        return(bands_sits)
    }
    # bands used by LOCAL
    bands_local <- sits_env$config[[sensor]][["bands"]][["LOCAL"]]
    # are the names those used by LOCAL?
    if (all(bands_files %in% bands_local)) {
      idx <- match(bands_files, bands_local)
      bands_local <- bands_local[idx]
      bands_sits <- bands_sits[idx]
      names(bands_sits) <- bands_local
      return(bands_sits)
    }
    stop("band names unknown by SITS configuration file. Please fix it")
    return(NULL)
}
#' @title Retrieve all but except the cloud band
#' @name .sits_config_bands_no_cloud
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @param cube   A data cube
#'
#' @return vector with bands that are not cloud bands
.sits_config_bands_no_cloud <- function(cube) {

    bands <- sits_bands(cube[1, ])
    cld_band <- .sits_config_cloud_band(cube[1, ])
    if (cld_band %in% bands) {
        bands <- bands[bands != cld_band]
    }
  return(bands)
}
#' @title Directory to read the BDC STAC catalogue
#' @name .sits_config_bdc_stac
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @return directory where BDC is accessible on the web
.sits_config_bdc_stac <- function() {
    return(sits_env$config$bdc_stac)
}

#' @title Test if BDC STAC catalogue is available
#' @name .sits_config_bdc_stac_access
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @param url  URL for access to the BDC STAC
#'
#' @return directory where BDC is accessible on the web
.sits_config_bdc_stac_access <- function(url = NULL) {
    if (purrr::is_null(url)) {
          url <- .sits_config_bdc_stac()
      }
    return(RCurl::url.exists(url))
}

#' @title Get the name of the band used for cloud information
#' @name .sits_config_cloud_band
#' @keywords internal
#' @param cube          data cube.
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @return vector with bands available in AWS for a given resolution
.sits_config_cloud_band <- function(cube) {
    cb <- paste0(cube$sensor[[1]], "_CLD_BAND")
    cloud_band <- sits_env$config[["CLOUD"]][[cube$source[1]]][[cb]]
    assertthat::assert_that(
        !purrr::is_null(cloud_band),
        msg = ".sits_config_cloud_band: cloud band information not available"
    )
    return(cloud_band)
}

#' @title Get the name of the band used for cloud information
#' @name .sits_config_cloud_values
#' @keywords internal
#' @param cube          data cube
#' @author Gilberto Camara \email{gilberto.camara@@inpe.br}
#'
#' @return vector with bands available in AWS for a given resolution
.sits_config_cloud_values <- function(cube) {
    cv <- paste0(cube$sensor[1], "_cld_vls")
    cloud_values <- sits_env$config[["CLOUD"]][[cube$source[1]]][[cv]]
    assertthat::assert_that(
        !purrr::is_null(cloud_values),
        msg = paste(".sits_config_cloud_values: cloud band values",
                    "information not available")
    )
    return(cloud_values)
}

#' @title Retrieve the color associated to a class in the configuration file
#' @name sits_config_color
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description Retrieve the color associated a class label.
#' @param label  A class label.
.sits_config_color <- function(label) {
    rgb <- as.character(sits_env$config$colors[[label]])
    if (!(length(rgb) > 0)) {
          rgb <- "#737373"
      }

    return(rgb)
}

#' @title Retrieve the colors associated to classes
#' @name sits_config_colors
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description Retrieve the color associated a class label.
#' @param labels  The class labels.
#' @return list of colors.
.sits_config_colors <- function(labels) {
    colors <- purrr::map(labels, function(l) {
        color <- .sits_config_color(l)
        return(color)
    })
    return(unlist(colors))
}

#' @title Retrieve the class associated to data cubes known to SITS
#' @name .sits_config_cube_class
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description Retrieve the class name associated to a cube type
#' @param source  Data cube source
#' @return        Class of data cube
#'
.sits_config_cube_class <- function(source) {

    # check that the cube is correct
    source <- toupper(source)

    # find out which cube types are supported
    sources <- sits_env$config$data_sources
    assertthat::assert_that(
        source %in% sources,
        msg = ".sits_config_cube_class: unsupported data source"
    )
    cube_classes <- sits_env$config$cube_classes
    names(cube_classes) <- sits_env$config$data_sources
    return(unname(cube_classes[source]))
}

#' @title Check that the cube data source is valid, based on the
#' configuration file
#' @name .sits_config_cube_check
#' @keywords internal
#' @author Gilberto Camara \email{gilberto.camara@@inpe.br}
#'
#' @param cube       Data cube
#' @return A logical value
#'
.sits_config_cube_check <- function(cube) {
    # precondition
    assertthat::assert_that(
        !purrr::is_null(cube),
        msg = "invalid cube"
    )

    assertthat::assert_that(
        !purrr::is_null(cube$source),
        msg = "invalid data source"
    )

    # find out which data sources are available
    sources <- sits_env$config$data_sources
    assertthat::assert_that(
        .sits_cube_source(cube) %in% sources,
        msg = ".sits_config_cube_check: Invalid data source"
    )
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

    if (inherits(data, c("sits", "patterns", "predicted"))) {
        return(data)

    } else {

        assertthat::assert_that(
            !purrr::is_null(data$source),
            msg = ".sits_config_data_meta_type: data is not valid"
        )

        # check if data is a cube
        .sits_config_cube_check(data)

        class(data) <- c("cube", class(data))
    }
    return(data)
}
#' @title Resolution for S2 bands in DEAFRICA
#' @name .sits_config_defrica_bands_res
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @param  source   cube source
#'
#' @return resolution information
.sits_config_defrica_bands_res <- function(sensor, bands) {
  assertthat::assert_that(
      all(bands %in% sits_env$config[["DEAFRICA"]][[sensor]][["bands"]]),
      msg = "Bands not available in DEAFRICA")
  res <- as.numeric(sits_env$config[["DEAFRICA"]][[sensor]][["resolution"]])
  names(res) <- sits_env$config[["DEAFRICA"]][[sensor]][["bands"]]
  return(unname(res[bands]))
}

#' @title Returns the file extensions known to SITS
#' @name .sits_config_img_file_ext
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @return vector of image file extensions known to SITS
#'
.sits_config_img_file_ext <- function() {
    return(sits_env$config[["img_file_extensions"]])
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
                as.numeric(sits_env$config[[sensor]][["maximum_value"]][[b]])
        })

    # post-condition
    assertthat::assert_that(
        !purrr::is_null(maximum_values),
        msg = paste0(
            "Missing maximum values for ",
            sensor,
            " edit configuration file"
        )
    )

    names(maximum_values) <- bands
    return(maximum_values)
}

#' @title Retrieve the estimated value of R memory bloat
#' @name .sits_config_memory_bloat
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description Retrieve the expected memory bloat associated to R.
.sits_config_memory_bloat <- function() {
    return(sits_env$config$R_memory_bloat)
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
                as.numeric(sits_env$config[[sensor]][["minimum_value"]][[b]])
        })

    # post-condition
    assertthat::assert_that(
        !purrr::is_null(min_val),
        msg = paste0(
            "No minimum values for ", sensor,
            " edit configuration files"
        )
    )

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
                as.numeric(sits_env$config[[sensor]][["missing_value"]][[b]])
        })
    # post-condition
    assertthat::assert_that(
        !purrr::is_null(mis_val),
        msg = paste0(
            "No missing values for sensor ",
            sensor,
            " edit configuration file"
        )
    )

    names(mis_val) <- bands
    return(mis_val)
}

#' @title Retrieve the resmapling method for bands of a sensor
#' @name .sits_config_resampling
#' @keywords internal
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param sensor         Name of the sensor
#' @param bands          Vector of bands.
#' @return The resampling methods.
.sits_config_resampling <- function(sensor, bands) {

  # pre-condition
  assertthat::assert_that(
    all(bands %in% names(sits_env$config[[sensor]][["resampling"]])),
    msg = paste(".sits_config_resampling: some bands not found.",
                "Edit configuration file.")
  )

  res <- sits_env$config[[sensor]][["resampling"]][bands]

  return(res)
}

#' @title Retrieve the scale factor for a label cube
#' @name .sits_config_label_scale_factor
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
.sits_config_label_scale_factor  <- function() {
    return(sits_env$config[["CLASSIFIED"]][["scale_factor"]])
}

#' @title Retrieve the missing value for a label cube
#' @name .sits_config_label_missing_value
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
.sits_config_label_missing_value  <- function() {
    return(sits_env$config[["CLASSIFIED"]][["missing_value"]])
}

#' @title Retrieve the minimum value for a label cube
#' @name .sits_config_label_minimum_value
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
.sits_config_label_minimum_value  <- function() {
    return(sits_env$config[["CLASSIFIED"]][["minimum_value"]])
}

#' @title Retrieve the maximum value for a label cube
#' @name .sits_config_label_maximum_value
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
.sits_config_label_maximum_value  <- function() {
    return(sits_env$config[["CLASSIFIED"]][["maximum_value"]])
}
#' @title Retrieve the scale factor for a probs cube
#' @name .sits_config_probs_scale_factor
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
.sits_config_probs_scale_factor  <- function() {
    return(sits_env$config[["PROBS"]][["scale_factor"]])
}

#' @title Retrieve the missing value for a probs cube
#' @name .sits_config_probs_missing_value
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
.sits_config_probs_missing_value  <- function() {
  return(sits_env$config[["PROBS"]][["missing_value"]])
}

#' @title Retrieve the minimum value for a probs cube
#' @name .sits_config_probs_minimum_value
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
.sits_config_probs_minimum_value  <- function() {
  return(sits_env$config[["PROBS"]][["minimum_value"]])
}

#' @title Retrieve the maximum value for a probs cube
#' @name .sits_config_probs_maximum_value
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
.sits_config_probs_maximum_value  <- function() {
  return(sits_env$config[["PROBS"]][["maximum_value"]])
}

#' @title Retrieve the estimated value of R memory bloat
#' @name .sits_config_processing_bloat
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description Retrieve the expected memory bloat associated to R.
.sits_config_processing_bloat <- function() {
    return(as.numeric(sits_env$config$R_processing_bloat))
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
    res <- vector(length = 2)
    names(res) <- c("xres", "yres")

    names(res) %>%
        purrr::map(function(c) {
            res[c] <<- as.numeric(
                sits_env$config[[sensor]][["resolution"]][[c]]
            )
        })

    # post-condition
    assertthat::assert_that(
        res["xres"] > 0,
        msg = paste0(
            "Horizontal resolution unavailable for ",
            sensor,
            " edit configuration file"
        )
    )
    assertthat::assert_that(
        res["yres"] > 0,
        msg = paste0(
            "Vertical resolution unavailable for ",
            sensor,
            " edit configuration file"
        )
    )

    return(res)
}
#' @title List the satellites supported by the configuration file
#' @name .sits_config_satellites
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @return List of satellites supported by SITS
.sits_config_satellites <- function() {
    return(sits_env$config[["supported_satellites"]])
}

#' @title Get pagination limit
#' @name .sits_config_rstac_limit
#' @keywords internal
#'
#' @return number of items to be returned in each page
.sits_config_rstac_limit <- function() {
    return(sits_env$config[["rstac_pagination_limit"]])
}

#' @title Get the URL to be used to test for SATVEG access
#' @name .sits_config_satveg_access
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @return URL to test SATVEG access
.sits_config_satveg_access <- function() {
    q <- "SATVEG_EMBRAPA_test"
    return(sits_env$config[[q]])
}

#' @title Retrieve the bands associated to SATVEG
#' @name sits_config_satveg_bands
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description Retrieve the cubes associated to the SATVEG service
#' @return         Names of SATVEG bands
.sits_config_satveg_bands <- function() {
    q <- paste0("SATVEG_bands")
    return(sits_env$config[[q]][["terra"]])
}

#' @title Retrieve the cubes associated to SATVEG
#' @name sits_config_satveg_cubes
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description Retrieve the cubes associated to SATVEG.
.sits_config_satveg_cubes <- function() {
    c <- sits_env$config[["SATVEG-EMBRAPA_cubes"]]

    return(c)
}
#' @title Retrieve the bounding box for SATVEG
#' @name .sits_config_satveg_bbox
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param name           Name of the cube.
#' @return The bounding box.
.sits_config_satveg_bbox <- function(name) {
    bbox <- vector(length = 4)
    names(bbox) <- c("xmin", "xmax", "ymin", "ymax")

    names(bbox) %>%
        purrr::map(function(c) {
            bbox[c] <<- sits_env$config[["SATVEG_bbox"]][[name]][[c]]
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
    crs <- sits_env$config[["SATVEG_crs"]][[name]]

    # post-condition
    assertthat::assert_that(
        length(crs) > 0,
        msg = paste0(
            "Projection information for cube ",
            name,
            " of service SATVEG not available"
        )
    )
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
    size <- vector(length = 2)
    names(size) <- c("nrows", "ncols")

    names(size) %>%
        purrr::map(function(c) {
            size[c] <<- sits_env$config[["SATVEG_size"]][[name]][[c]]
        })

    # post-condition
    assertthat::assert_that(
        as.integer(size["nrows"]) > 0,
        msg = paste("Number of rows not available for cube", name)
    )
    assertthat::assert_that(
        as.integer(size["ncols"]) > 0,
        msg = paste("Number of cols not available for cube", name)
    )

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
    return(sits_env$config[[q]])
}
#' @title Tests is satellite and sensor are known to SITS
#' @name .sits_config_satellite_sensor
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param satellite      Name of the satellite
#' @param sensor         Name of the sensor
#' @return A logical value
#'
.sits_config_satellite_sensor <- function(satellite, sensor) {
    assertthat::assert_that(
        satellite %in% .sits_config_satellites(),
        msg = paste(".sits_config_satellite_sensor: satellite not supported",
                    "by SITS - edit configuration file")
    )

    assertthat::assert_that(
        sensor %in% .sits_config_sensors(satellite),
        msg = paste(".sits_config_satellite_sensor: sensor not supported",
                    "by SITS - edit configuration file")
    )
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

    assertthat::assert_that(
        satellite %in% .sits_config_satellites(),
        msg = paste(".sits_config_sensors: satellite not supported",
                    "by SITS - edit configuration file")
    )

    q <- paste0(satellite, "_sensors")
    sensors <- sits_env$config[[q]]

    return(sensors)
}

#' @title Get the the bands in AWS for Sentinel-2 ARD given the resolution
#' @name .sits_config_s2_bands
#' @keywords internal
#' @param resolution       Resolution of the bands
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @return vector with bands available in AWS for a given resolution
.sits_config_s2_bands <- function(resolution) {

    sensor <- "MSI"
    assertthat::assert_that(
        resolution %in% sits_env$config[[sensor]][["resolutions"]],
        msg = ".sits_config_s2_bands: Sentinel-2 in AWS - wrong resolution"
    )

    r <- paste0("bands_", resolution, "m")
    return(sits_env$config[[sensor]][[r]])
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
                as.numeric(sits_env$config[[sensor]][["scale_factor"]][[b]])
        })
    names(scale_f) <- bands
    # post-condition
    assertthat::assert_that(
        !purrr::is_null(scale_f),
        msg = paste0(
            "No scale factors for sensor",
            sensor,
            " edit configuration file"
        )
    )
    return(scale_f)
}
#' @title File to test access to cloud services
#' @name .sits_config_test_file
#' @keywords internal
#' @param  source    Data source
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @return name of the bucket
.sits_config_test_file <- function(source) {
  return(sits_env$config[[source]][["test_file"]])
}
#' @title Number of parallel data access
#' @name .sits_config_download_maxcores
#' @keywords internal
#' @param  type    Data cube type
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @return name of the bucket
.sits_config_access_maxcores <- function(source) {
  return(sits_env$config[[source]][["access_maxcores"]])
}
#' @title Raster package to be used
#' @name .sits_config_raster_package
#' @keywords internal
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @return name of the raster package
.sits_config_raster_package <- function() {

  # read config value
  pkg_name <- sits_env$config[["R_raster_pkg"]]

  # default raster package
  if (purrr::is_null(pkg_name)) pkg_name <- "raster"

  return(pkg_name)
}
#' @title GDAL GTiff creation options
#' @name .sits_config_gtiff_default_options
#' @keywords internal
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @return character vector with GDAL creation options
.sits_config_gtiff_default_options <- function() {

  # return
  return(sits_env$config[["GTiff_default_options"]])
}
