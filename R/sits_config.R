#' #'
#' #' @title Information about configuration file
#' #' @name sits_
#' #' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' #'
#' #' @description Displays the local of sits configuration file. For details
#' #' on how to set the configuration file, use \code{\link[sits]{sits_config}}.
#' #'
#' #' @return a message
#' #' @examples
#' #' sits_config_info()
#' #' @export
#' sits_config_info <- function() {
#'
#'     # the default configuration file
#'     yml_file <- system.file("extdata", "config.yml", package = "sits")
#'
#'     message(paste0("Using configuration file: ", yml_file))
#'     message(paste0("Using raster package: ", .config_raster_pkg()))
#'
#'     # try to find a valid user configuration file
#'     user_yml_file <- Sys.getenv("SITS_USER_CONFIG_FILE")
#'     if (file.exists(user_yml_file)) {
#'         message(paste("Additional configurations found in", user_yml_file))
#'     } else {
#'         message(paste("To provide additional configurations, create an",
#'                       "yml file and set environment variable",
#'                       "SITS_USER_CONFIG_FILE to point to it"))
#'     }
#'
#'     return(invisible(TRUE))
#' }

#'
#' #' @title Read the AWS default region from configuration file
#' #' @name .sits_config_aws_default_region
#' #' @param source  Source of data cube
#' #' @keywords internal
#' #' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' #'
#' #' @return directory where BDC is accessible on the web
#' .sits_config_aws_default_region <- function(source) {
#'     return(sits_env$config$sources[[source]][["AWS_DEFAULT_REGION"]])
#' }
#'
#' #' @title Read the AWS end point from configuration file
#' #' @name .sits_config_aws_endpoint
#' #' @param source  Source of data cube
#' #' @keywords internal
#' #' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' #'
#' #' @return directory where BDC is accessible on the web
#' .sits_config_aws_endpoint <- function(source) {
#'     return(sits_env$config$sources[[source]][["AWS_S3_ENDPOINT"]])
#' }
#'
#' #' @title Read the AWS end point from configuration file
#' #' @name .sits_config_aws_request_payer
#' #' @param source  Source of data cube
#' #' @keywords internal
#' #' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' #'
#' #' @return directory where BDC is accessible on the web
#' .sits_config_aws_request_payer <- function(source) {
#'     return(sits_env$config$sources[[source]][["AWS_REQUEST_PAYER"]])
#' }
#'
#' #' @title Test if cube is available via URL
#' #' @name .sits_config_cube_access
#' #' @keywords internal
#' #' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' #' @param url  URL for access to the cube
#' #' @param name service name
#' #'
#' #' @return TRUE/FALSE
#' .sits_config_cube_access <- function(url, name) {
#'
#'     access <- httr::GET(url)
#'
#'     # did we get the data?
#'     if (httr::http_error(access)) {
#'         message(paste0(name, " is not accessible using URL ", url))
#'         return(FALSE)
#'     }
#'     return(TRUE)
#' }
#' #' @title Test if files in a raster cube are
#' #' @name .sits_config_cube_file_access
#' #' @keywords internal
#' #' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' #' @param cube  cube
#' #'
#' #' @return TRUE/FALSE
#' .sits_config_cube_file_access <- function(cube) {
#'
#'     f <- cube$file_info[[1]]$path[[1]]
#'     access <- tryCatch({
#'         r <- .raster_open_rast(f)
#'         return(TRUE)
#'     }, error = function(e){
#'         message(paste0("raster file ", f, " is not accessible"))
#'         return(FALSE)
#'     })
#'     return(access)
#' }
#'
#' #' @title Get the name of the band used for cloud information
#' #' @name .sits_config_cloud_values
#' #' @keywords internal
#' #' @param cube          data cube
#' #' @author Gilberto Camara \email{gilberto.camara@@inpe.br}
#' #'
#' #' @return vector with bands available in AWS for a given resolution
#' .sits_config_cloud_values <- function(cube) {
#'
#'     s <- cube$source[[1]]
#'     col <- cube$collection[[1]]
#'
#'     cloud_values <-
#'         sits_env$config$sources[[s]]$collections[[col]]$cloud_band$interp_values
#'
#'     assertthat::assert_that(
#'         !purrr::is_null(cloud_values),
#'         msg = paste(".sits_config_cloud_values: cloud band values",
#'                     "information not available")
#'     )
#'     return(cloud_values)
#' }
#'
#' #' @title Get the flag of bitmask in cloud information
#' #' @name .sits_config_cloud_bitmask
#' #' @keywords internal
#' #' @param cube          data cube
#' #' @author Gilberto Camara \email{gilberto.camara@@inpe.br}
#' #'
#' #' @return vector with bands available in AWS for a given resolution
#' .sits_config_cloud_bitmask <- function(cube) {
#'
#'     source <- cube$source[[1]]
#'     col <- cube$collection[[1]]
#'
#'     cloud_bitmask <-
#'         sits_env$config$sources[[source]]$collections[[col]]$cloud_band$bit_mask
#'
#'
#'     assertthat::assert_that(
#'         !purrr::is_null(cloud_bitmask),
#'         msg = paste(".sits_config_cloud_bitmask: cloud bitmask flag",
#'                     "is not available")
#'     )
#'     return(cloud_bitmask)
#' }
#'
#' #' @title Retrieve the class associated to data cubes known to SITS
#' #' @name .sits_config_cube_class
#' #' @keywords internal
#' #' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' #' @description Retrieve the class name associated to a cube type
#' #' @param source  Data cube source
#' #' @return        Class of data cube
#' #'
#' .sits_config_cube_class <- function(source) {
#'
#'     # check that the cube is correct
#'     source <- toupper(source)
#'
#'     # find out which cube types are supported
#'     sources <- names(sits_env$config$sources)
#'     assertthat::assert_that(
#'         source %in% sources,
#'         msg = ".sits_config_cube_class: unsupported data source"
#'     )
#'
#'     return(sits_env$config$sources[[source]]$s3_class)
#' }
#'
#' #' @title Check that the cube data source is valid, based on the
#' #' configuration file
#' #' @name .sits_config_cube_check
#' #' @keywords internal
#' #' @author Gilberto Camara \email{gilberto.camara@@inpe.br}
#' #'
#' #' @param cube       Data cube
#' #' @return A logical value
#' #'
#' .sits_config_cube_check <- function(cube) {
#'     # precondition
#'     assertthat::assert_that(
#'         !purrr::is_null(cube),
#'         msg = "invalid cube"
#'     )
#'
#'     assertthat::assert_that(
#'         !purrr::is_null(cube$source),
#'         msg = "invalid data source"
#'     )
#'
#'     # find out which data sources are available
#'     sources <- names(sits_env$config$sources)
#'     assertthat::assert_that(
#'         .sits_cube_source(cube) %in% sources,
#'         msg = ".sits_config_cube_check: Invalid data source"
#'     )
#'     return(TRUE)
#' }
#'
#' #' @title meta-type for data
#' #' @name .sits_config_data_meta_type
#' #' @keywords internal
#' #' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' #' @param  data    tibble (time series or cube)
#' #'
#' #' @return file path to the appended to data_dir
#' .sits_config_data_meta_type <- function(data) {
#'
#'     if (inherits(data, c("sits", "patterns", "predicted", "sits_model"))) {
#'         return(data)
#'
#'     } else {
#'
#'         assertthat::assert_that(
#'             !purrr::is_null(data$source),
#'             msg = ".sits_config_data_meta_type: data is not valid"
#'         )
#'
#'         # check if data is a cube
#'         .sits_config_cube_check(data)
#'
#'         class(data) <- c("cube", class(data))
#'     }
#'     return(data)
#' }
#'
#' #' @title Resolution for S2 bands in DEAFRICA
#' #' @name .sits_config_bands_res
#' #' @keywords internal
#' #' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' #' @param  source   cube source
#' #'
#' #' @return resolution information
#' .sits_config_bands_res <- function(source, collection, bands) {
#'
#'     bands_collection <- .config_bands_band_name(source, collection)
#'     col <- sits_env$config$sources[[source]]$collections[[collection]]
#'
#'     assertthat::assert_that(
#'         all(bands %in% bands_collection),
#'         msg = "Bands not available in collection.")
#'
#'     if (col$cloud_band$band_name %in% bands) {
#'         res <- c(col$cloud_band$resolutions)
#'
#'         bands_no_cloud <- bands[!bands == col$cloud_band$band_name]
#'
#'         res <- c(res,
#'                  purrr::map_dbl(bands_no_cloud, function(band) {
#'                      col$bands[[band]]$resolutions
#'                  })
#'         )
#'     } else {
#'         res <- purrr::map_dbl(bands, function(band) {
#'             col$bands[[band]]$resolutions
#'         })
#'     }
#'
#'     return(res)
#' }
#'

#'
#' #' @title Tests is satellite and sensor are known to SITS
#' #' @name .sits_config_satellite_sensor
#' #' @keywords internal
#' #' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' #'
#' #' @param satellite      Name of the satellite
#' #' @param sensor         Name of the sensor
#' #' @return A logical value
#' #'
#' .sits_config_local_satellite_sensor <- function(satellite, sensor) {
#'
#'     sat_sensors <- names(sits_env$config$sources[["LOCAL"]]$collections)
#'     satellites <- purrr::map_chr(
#'         strsplit(sat_sensors, "/"), function(x){x[[1]]}
#'     )
#'
#'     assertthat::assert_that(
#'         satellite %in% satellites,
#'         msg = paste(".sits_config_satellite_sensor: satellite not supported",
#'                     "by SITS - edit configuration file")
#'     )
#'
#'     # using satellite to create regex pattern
#'     reg_pattern <- paste0(satellite, "/")
#'     assertthat::assert_that(
#'         any(grepl(pattern = reg_pattern, x = sat_sensors, fixed = TRUE)),
#'         msg = paste(".sits_config_satellite_sensor: sensor not supported",
#'                     "by SITS - edit configuration file")
#'     )
#' }
