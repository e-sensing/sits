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
    sits_env$config <- yaml::yaml.load_file(input = yml_file,
                                            merge.precedence = "override")

    # try to find a valid user configuration file
    user_yml_file <- Sys.getenv("SITS_USER_CONFIG_FILE")

    if (file.exists(user_yml_file)) {
        config_user <- yaml::yaml.load_file(input = user_yml_file,
                                            merge.precedence = "override")

        sits_env$config <- utils::modifyList(sits_env$config, config_user)
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
    message(paste0("Using raster package: ", .config_raster_pkg()))

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
    return(sits_env$config$sources[[source]][["AWS_DEFAULT_REGION"]])
}

#' @title Read the AWS end point from configuration file
#' @name .sits_config_aws_endpoint
#' @param source  Source of data cube
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @return directory where BDC is accessible on the web
.sits_config_aws_endpoint <- function(source) {
    return(sits_env$config$sources[[source]][["AWS_S3_ENDPOINT"]])
}

#' @title Read the AWS end point from configuration file
#' @name .sits_config_aws_request_payer
#' @param source  Source of data cube
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @return directory where BDC is accessible on the web
.sits_config_aws_request_payer <- function(source) {
    return(sits_env$config$sources[[source]][["AWS_REQUEST_PAYER"]])
}

#' @title Test if cube is available via URL
#' @name .sits_config_cube_access
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @param url  URL for access to the cube
#' @param name service name
#'
#' @return TRUE/FALSE
.sits_config_cube_access <- function(url, name) {

    access <- httr::GET(url)

    # did we get the data?
    if (httr::http_error(access)) {
        message(paste0(name, " is not accessible using URL ", url))
        return(FALSE)
    }
    return(TRUE)
}
#' @title Test if files in a raster cube are
#' @name .sits_config_cube_file_access
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @param cube  cube
#'
#' @return TRUE/FALSE
.sits_config_cube_file_access <- function(cube) {

    f <- cube$file_info[[1]]$path[[1]]
    access <- tryCatch({
        r <- .raster_open_rast(f)
        return(TRUE)
    }, error = function(e){
        message(paste0("raster file ", f, " is not accessible"))
        return(FALSE)
    })
    return(access)
}

#' @title Get the name of the band used for cloud information
#' @name .sits_config_cloud_values
#' @keywords internal
#' @param cube          data cube
#' @author Gilberto Camara \email{gilberto.camara@@inpe.br}
#'
#' @return vector with bands available in AWS for a given resolution
.sits_config_cloud_values <- function(cube) {

    s <- cube$source[[1]]
    col <- cube$collection[[1]]

    cloud_values <-
        sits_env$config$sources[[s]]$collections[[col]]$cloud_band$interp_values

    assertthat::assert_that(
        !purrr::is_null(cloud_values),
        msg = paste(".sits_config_cloud_values: cloud band values",
                    "information not available")
    )
    return(cloud_values)
}

#' @title Get the flag of bitmask in cloud information
#' @name .sits_config_cloud_bitmask
#' @keywords internal
#' @param cube          data cube
#' @author Gilberto Camara \email{gilberto.camara@@inpe.br}
#'
#' @return vector with bands available in AWS for a given resolution
.sits_config_cloud_bitmask <- function(cube) {

    source <- cube$source[[1]]
    col <- cube$collection[[1]]

    cloud_bitmask <-
        sits_env$config$sources[[source]]$collections[[col]]$cloud_band$bit_mask


    assertthat::assert_that(
        !purrr::is_null(cloud_bitmask),
        msg = paste(".sits_config_cloud_bitmask: cloud bitmask flag",
                    "is not available")
    )
    return(cloud_bitmask)
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
    sources <- names(sits_env$config$sources)
    assertthat::assert_that(
        source %in% sources,
        msg = ".sits_config_cube_class: unsupported data source"
    )

    return(sits_env$config$sources[[source]]$s3_class)
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
    sources <- names(sits_env$config$sources)
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

    if (inherits(data, c("sits", "patterns", "predicted", "sits_model"))) {
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
#' @name .sits_config_bands_res
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @param  source   cube source
#'
#' @return resolution information
.sits_config_bands_res <- function(source, collection, bands) {

    bands_collection <- .config_bands_band_name(source, collection)
    col <- sits_env$config$sources[[source]]$collections[[collection]]

    assertthat::assert_that(
        all(bands %in% bands_collection),
        msg = "Bands not available in collection.")

    if (col$cloud_band$band_name %in% bands) {
        res <- c(col$cloud_band$resolutions)

        bands_no_cloud <- bands[!bands == col$cloud_band$band_name]

        res <- c(res,
                 purrr::map_dbl(bands_no_cloud, function(band) {
                     col$bands[[band]]$resolutions
                 })
        )
    } else {
        res <- purrr::map_dbl(bands, function(band) {
            col$bands[[band]]$resolutions
        })
    }

    return(res)
}

#' @title Retrieve the scale factor for a probs cube
#' @name .sits_config_probs_scale_factor
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
.sits_config_probs_scale_factor  <- function() {
    return(sits_env$config$sources[["PROBS"]][["scale_factor"]])
}

#' @title Retrieve the pixel spatial resolution for a data cube
#' @name .sits_config_resolution
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param sensor         Name of the sensor.
#' @return Vector of (xres, yres).
.sits_config_resolution <- function(source, collection, band) {

    col <- sits_env$config$sources[[source]]$collections[[collection]]

    # create a string to query for the resolution
    res <- rep(col$bands[[band]]$resolutions, 2)
    names(res) <- c("resolutions_x", "resolutions_y")

    # post-condition
    assertthat::assert_that(
        res["resolutions_x"] > 0,
        msg = paste0(
            "Horizontal resolution unavailable for ",
            source,
            " edit configuration file"
        )
    )
    assertthat::assert_that(
        res["resolutions_y"] > 0,
        msg = paste0(
            "Vertical resolution unavailable for ",
            source,
            " edit configuration file"
        )
    )

    return(res)
}

#' @title Get the URL to be used to test for SATVEG access
#' @name .sits_config_satveg_access
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @return URL to test SATVEG access
.sits_config_satveg_access <- function() {
    q <- "url_test"
    return(sits_env$config$sources[["SATVEG"]][[q]])
}

#' @title Retrieve the bands associated to SATVEG
#' @name sits_config_satveg_bands
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description Retrieve the cubes associated to the SATVEG service
#' @return         Names of SATVEG bands
.sits_config_satveg_bands <- function() {
    return(
        names(sits_env$config$sources[["SATVEG"]]$collections[["terra"]]$bands)
    )
}

#' @title Retrieve the cubes associated to SATVEG
#' @name sits_config_satveg_cubes
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description Retrieve the cubes associated to SATVEG.
.sits_config_satveg_cubes <- function() {
    return(names(sits_env$config$sources[["SATVEG"]]$collections))
}
#' @title Retrieve the bounding box for SATVEG
#' @name .sits_config_satveg_bbox
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param name Name of the cube.
#' @return The bounding box.
.sits_config_satveg_bbox <- function(name) {
    return(unlist(sits_env$config$sources[["SATVEG"]]$collections[[name]]$bbox))
}

#' @title Retrieve the projection for SATVEG service
#' @name .sits_config_satveg_projection
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param name Name of the cube.
#' @return CRS PROJ4 information.
.sits_config_satveg_projection <- function(name) {
    crs <- sits_env$config$sources[["SATVEG"]]$collections[[name]]$crs

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

    size <- unlist(sits_env$config$sources[["SATVEG"]]$collections[[name]]$size)

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

#' @title Tests is satellite and sensor are known to SITS
#' @name .sits_config_satellite_sensor
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param satellite      Name of the satellite
#' @param sensor         Name of the sensor
#' @return A logical value
#'
.sits_config_local_satellite_sensor <- function(satellite, sensor) {

    sat_sensors <- names(sits_env$config$sources[["LOCAL"]]$collections)
    satellites <- purrr::map_chr(strsplit(sat_sensors, "/"), function(x){x[[1]]})

    assertthat::assert_that(
        satellite %in% satellites,
        msg = paste(".sits_config_satellite_sensor: satellite not supported",
                    "by SITS - edit configuration file")
    )

    # using satellite to create regex pattern
    reg_pattern <- paste0(satellite, "/")
    assertthat::assert_that(
        any(grepl(pattern = reg_pattern, x = sat_sensors, fixed = TRUE)),
        msg = paste(".sits_config_satellite_sensor: sensor not supported",
                    "by SITS - edit configuration file")
    )
}
