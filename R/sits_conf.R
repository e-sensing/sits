#' @title Reads a configuration file and loads it in the main environment
#' @name .config
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
#' @name .config_info
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Displays the local of sits configuration file. For details
#' on how to set the configuration file, use \code{\link[sits]{sits_config}}.
#'
#' @return a message
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
#' @name config_show
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Displays the contents of sits configuration file. For details
#' on how to set the configuration file, use \code{\link[sits]{sits_config}}.
#'
#' @return List with the configuration parameters used by sits.
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

#' @title
#' @description
#' @keywords
#' @

.config_get <- function(key, default = NULL, simplify = FALSE) {

    value <- tryCatch({
        sits_env$config[[key]]
    },
    error = function(e) {
        return(default)
    })

    if (is.null(value) && is.null(default))
        stop(paste(".config_get:", paste0(key, collapse = "$"),
                   "not found.\nPlease, check config file."),
             call. = FALSE)

    if (simplify)
        return(unlist(value))
    return(value)
}

.config_names <- function(key) {

    values <- tryCatch({
        names(sits_env$config[[key]])
    },
    error = function(e) {
        return(NULL)
    })

    if (is.null(values))
        stop(paste(".config_names: key", paste0(key, collapse = "$"),
                   "not found in config or not have names."), call. = FALSE)

    return(values)
}

.config_aws_default_region <- function(source,
                                       collection) {

    .config_get(key = c("sources", source, "collections", collection,
                        "AWS", "AWS_DEFAULT_REGION"),
                default = NA)
}

.config_aws_endpoint <- function(source,
                                 collection) {

    .config_get(key = c("sources", source, "collections", collection,
                        "AWS", "AWS_S3_ENDPOINT"),
                default = NA)
}

.config_aws_request_payer <- function(source,
                                      collection) {

    .config_get(key = c("sources", source, "collections", collection,
                        "AWS", "AWS_REQUEST_PAYER"),
                default = NA)
}

.config_bands <- function(source,
                          collection, ...,
                          fn_filter = NULL,
                          add_cloud = TRUE) {


    bands <- .config_names(key = c("sources", source, "collections",
                                   collection, "bands"))

    if (!add_cloud)
        bands <- bands[bands != "CLOUD"]

    if (!is.null(fn_filter)) {
        select <- vapply(bands, function(band) {
            fn_filter(.config_get(key = c("sources", source, "collections",
                                          collection, "bands", band)))
        }, logical(1))
        return(bands[select])
    }

    bands
}

.config_bands_reap <- function(source,
                               collection,
                               key, ...,
                               bands = NULL,
                               fn_filter = NULL,
                               add_cloud = TRUE,
                               default = NULL) {

    if (is.null(bands))
        bands <- .config_bands(source = source,
                               collection = collection,
                               fn_filter = fn_filter,
                               add_cloud = add_cloud)

    values <- lapply(bands, function(band) {
        .config_get(key = c("sources", source, "collections",
                            collection, "bands", band, key),
                    default = default)

    })

    if (length(values) > 0 && is.atomic(values[[1]]))
        return(unlist(unname(values)))

    return(unname(values))
}

.config_bands_band_name <- function(source,
                                    collection, ...,
                                    bands = NULL,
                                    fn_filter = NULL,
                                    add_cloud = TRUE) {

    .config_bands_reap(source = source,
                       collection = collection,
                       key = "band_name",
                       bands = bands,
                       fn_filter = fn_filter,
                       add_cloud = add_cloud)
}

.config_bands_resolutions <- function(source,
                                      collection, ...,
                                      bands = NULL,
                                      fn_filter = NULL,
                                      add_cloud = TRUE) {

    values <- .config_bands_reap(source = source,
                                 collection = collection,
                                 key = "resolutions",
                                 bands = bands,
                                 fn_filter = fn_filter,
                                 add_cloud = add_cloud)

    assertthat::assert_that(
        all(values > 0),
        msg = ".config_bands_resolutions: invalid resolution."
    )

    return(values)
}

.config_cloud <- function() {

    return("CLOUD")
}

.config_cloud_bit_mask <- function(source,
                                   collection) {

    .config_get(key = c("sources", source, "collections", collection,
                        "bands", "CLOUD", "bit_mask"))
}

.config_cloud_values <- function(source,
                                 collection) {

    .config_get(key = c("sources", source, "collections", collection,
                        "bands", "CLOUD", "values"))
}

.config_cloud_interp_values <- function(source,
                                        collection) {

    .config_get(key = c("sources", source, "collections", collection,
                        "bands", "CLOUD", "interp_values"))
}

.config_collections <- function(source) {

    .config_names(c("sources", source, "collections"))
}

.config_gtiff_default_options <- function() {

    .config_get(key = c("GTiff_default_options"))
}

.config_local_file_extensions <- function() {

    .config_get(key = c("sources", "LOCAL", "file_extensions"))
}


.config_memory_bloat <- function() {

    .config_get(key = c("R_memory_bloat"))
}

#' @title meta-type for data
#' @name .sits_config_data_meta_type
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @param  data    tibble (time series or cube)
#'
#' @return file path to the appended to data_dir
.config_data_meta_type <- function(data) {

    if (inherits(data, c("sits", "patterns", "predicted", "sits_model"))) {
        return(data)

    } else {

        assertthat::assert_that(
            !purrr::is_null(data$source),
            msg = ".sits_config_data_meta_type: data is not valid"
        )

        # check if data is a cube
        # TODO: where this function will be implemented?
        #.sits_config_cube_check(data)

        class(data) <- c("cube", class(data))
    }
    return(data)
}

.config_palettes <- function() {

    .config_names(c("palettes"))
}

.config_palette_colors <- function(labels, ...,
                                   palette = "default") {

    values <- .config_get(key = c("palettes", palette))[labels]
    names(values) <- labels

    if (any(is.na(values))) {

        random <- grDevices::colors()
        random <- random[!random %in% values]
        values[is.na(values)] <- sample(random, sum(is.na(values)))
    }

    values
}

.config_processing_bloat <- function() {

    .config_get(key = c("R_processing_bloat"))
}

.config_rstac_limit <- function() {

    .config_get(key = c("rstac_pagination_limit"))
}

.config_raster_pkg <- function() {

    .config_get(key = c("R_raster_pkg"))
}

.config_sources <- function() {

    .config_names(c("sources"))
}

.config_source_url <- function(source) {

    .config_get(key = c("sources", source, "url"))
}

.config_source_service <- function(source) {

    .config_get(key = c("sources", source, "service"))
}

.config_source_s3class <- function(source) {

    .config_get(key = c("sources", source, "s3_class"))
}
