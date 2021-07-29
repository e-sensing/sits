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

    # get and check the default configuration file path
    yml_file <- .config_file()

    # read the configuration parameters
    sits_env$config <- yaml::yaml.load_file(
        input = yml_file,
        merge.precedence = "override"
    )

    # try to find a valid user configuration file
    user_yml_file <- .config_user_file()

    if (file.exists(user_yml_file)) {
        config_user <- yaml::yaml.load_file(input = user_yml_file,
                                            merge.precedence = "override")
        sits_env$config <- utils::modifyList(x = sits_env$config,
                                             val = config_user)
    }

    return(invisible(NULL))
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

    # get and check the default configuration file path
    yml_file <- .config_file()

    message(paste0("Using configuration file: ", yml_file))
    message(paste0("Using raster package: ", .config_raster_pkg()))

    # try to find a valid user configuration file
    user_yml_file <- .config_user_file()

    if (file.exists(user_yml_file)) {
        message(paste("Additional configurations found in", user_yml_file))
    } else {
        message(paste("To provide additional configurations, create an",
                      "yml file and set environment variable",
                      "SITS_USER_CONFIG_FILE to point to it."))
    }

    return(invisible(NULL))
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

    # get and check the default configuration file path
    yml_file <- .config_file()

    # read the configuration parameters
    message("Default system configuration file")
    cat(readLines(yml_file), sep = "\n")

    # try to find a valid user configuration file
    user_yml_file <- .config_user_file()

    if (file.exists(user_yml_file)) {
        message("User configuration file - overrides default config")
        cat(readLines(user_yml_file), sep = "\n")
    }

    return(invisible(NULL))
}

.config_file <- function() {

    # load the default configuration file
    file <- system.file("extdata", "config.yml", package = "sits")

    # check that the file is valid
    .check_chr(file, allow_na = FALSE, allow_empty = FALSE,
               min_len = 1, max_len = 1, msg = "invalid configuration file")

    assertthat::assert_that(
        !purrr::is_null(file),
        msg = ".config_file: invalid configuration file"
    )

    # check if the file exists
    assertthat::assert_that(
        file.exists(file),
        msg = paste(".config_file: file", file, "does not exists.")
    )

    return(file)
}

.config_user_file <- function() {

    # load the default configuration file
    file <- Sys.getenv("SITS_USER_CONFIG_FILE")

    # check if the file exists
    if (nchar(file) > 0) {
        assertthat::assert_that(
            file.exists(file),
            msg = paste(".config_user_file: file", file, "does not exists.")
        )
    }

    return(file)
}

.config_get <- function(key, default = NULL, simplify = FALSE) {

    res <- tryCatch({
        sits_env$config[[key]]
    },
    error = function(e) {
        return(default)
    })

    assertthat::assert_that(
        !is.null(res),
        msg = paste(".config_get:", paste0(key, collapse = "$"),
                    "not found.\nPlease, check config file.")
    )

    if (simplify)
        return(unlist(res))

    return(res)
}

.config_names <- function(key) {

    res <- tryCatch({
        names(sits_env$config[[key]])
    },
    error = function(e) {
        return(NULL)
    })

    assertthat::assert_that(
        !is.null(res),
        msg = paste(".config_names:", paste0(key, collapse = "$"),
                    "not found.\nPlease, check config file.")
    )

    return(res)
}

.config_aws_default_region <- function(source,
                                       collection) {

    res <- .config_get(key = c("sources", source, "collections", collection,
                               "AWS", "AWS_DEFAULT_REGION"),
                       default = NA)

    assertthat::assert_that(
        is.character(res) && length(res) == 1,
        msg = paste(".config_aws_default_region: value must be character.")
    )

    return(res)
}

.config_aws_endpoint <- function(source,
                                 collection) {

    res <- .config_get(key = c("sources", source, "collections", collection,
                               "AWS", "AWS_S3_ENDPOINT"),
                       default = NA)

    assertthat::assert_that(
        is.character(res) && length(res) == 1,
        msg = paste(".config_aws_endpoint: value must be character.")
    )

    return(res)
}

.config_aws_request_payer <- function(source,
                                      collection) {

    res <- .config_get(key = c("sources", source, "collections", collection,
                               "AWS", "AWS_REQUEST_PAYER"),
                       default = NA)

    assertthat::assert_that(
        is.character(res) && length(res) == 1,
        msg = paste(".config_aws_request_payer: value must be character.")
    )

    return(res)
}

.config_bands <- function(source,
                          collection, ...,
                          fn_filter = NULL,
                          add_cloud = TRUE) {


    res <- .config_names(key = c("sources", source, "collections",
                                   collection, "bands"))

    if (!add_cloud)
        res <- res[res != "CLOUD"]

    if (!is.null(fn_filter)) {
        select <- vapply(res, function(band) {
            fn_filter(.config_get(key = c("sources", source, "collections",
                                          collection, "bands", band)))
        }, logical(1))
        return(res[select])
    }

    return(res)
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

    res <- lapply(bands, function(band) {
        .config_get(key = c("sources", source, "collections",
                            collection, "bands", band, key),
                    default = default)

    })

    if (length(res) > 0 && is.atomic(res[[1]]))
        return(unlist(unname(res)))

    return(unname(res))
}

.config_bands_band_name <- function(source,
                                    collection, ...,
                                    bands = NULL,
                                    fn_filter = NULL,
                                    add_cloud = TRUE) {

    res <- .config_bands_reap(source = source,
                              collection = collection,
                              key = "band_name",
                              bands = bands,
                              fn_filter = fn_filter,
                              add_cloud = add_cloud)

    return(res)
}

.config_bands_resolutions <- function(source,
                                      collection, ...,
                                      bands = NULL,
                                      fn_filter = NULL,
                                      add_cloud = TRUE) {

    res <- .config_bands_reap(source = source,
                                 collection = collection,
                                 key = "resolutions",
                                 bands = bands,
                                 fn_filter = fn_filter,
                                 add_cloud = add_cloud)

    assertthat::assert_that(
        all(res > 0),
        msg = ".config_bands_resolutions: invalid resolution."
    )

    return(res)
}

.config_cloud <- function() {

    return("CLOUD")
}

.config_cloud_bit_mask <- function(source,
                                   collection) {

    res <- .config_get(key = c("sources", source, "collections", collection,
                               "bands", "CLOUD", "bit_mask"))

    return(res)
}

.config_cloud_values <- function(source,
                                 collection) {

    res <- .config_get(key = c("sources", source, "collections", collection,
                               "bands", "CLOUD", "values"))

    return(res)
}

.config_cloud_interp_values <- function(source,
                                        collection) {

    res <- .config_get(key = c("sources", source, "collections", collection,
                               "bands", "CLOUD", "interp_values"))

    return(res)
}

.config_collections <- function(source) {

    res <- .config_names(c("sources", source, "collections"))

    return(res)
}

.config_gtiff_default_options <- function() {

    res <- .config_get(key = c("GTiff_default_options"))

    return(res)
}

.config_local_file_extensions <- function() {

    res <- .config_get(key = c("sources", "LOCAL", "file_extensions"))

    return(res)
}


.config_memory_bloat <- function() {

    res <- .config_get(key = c("R_memory_bloat"))

    return(res)
}

#' @title meta-type for data
#' @name .config_data_meta_type
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

        random <- colors()
        random <- random[!random %in% values]
        values[is.na(values)] <- sample(random, sum(is.na(values)))
    }

    return(values)
}

.config_processing_bloat <- function() {

    res <- .config_get(key = c("R_processing_bloat"))

    assertthat::assert_that(
        is.numeric(res) && length(res) == 1,
        msg = ".config_processing_bloat: value must be numeric."
    )

    assertthat::assert_that(
        res > 0,
        msg = ".config_processing_bloat: value must be positive."
    )

    return(res)
}

.config_rstac_limit <- function() {

    res <- .config_get(key = c("rstac_pagination_limit"))

    assertthat::assert_that(
        is.numeric(res) && length(res) == 1,
        msg = ".config_rstac_limit: value must be numeric"
    )

    assertthat::assert_that(
        res > 0,
        msg = ".config_rstac_limit: value must be positive."
    )

    return(res)
}

.config_raster_pkg <- function() {

    res <- .config_get(key = c("R_raster_pkg"))

    assertthat::assert_that(
        res %in% c("terra", "raster") && length(res) == 1,
        msg = ".config_raster_pkg: invalid raster package."
    )

    return(res)
}

.config_sources <- function() {

    res <- .config_names(c("sources"))

    assertthat::assert_that(
        length(res) > 0,
        msg = ".config_sources: invalid sources."
    )

    return(res)
}

.config_source_url <- function(source) {

    res <- .config_get(key = c("sources", source, "url"))

    assertthat::assert_that(
        is.character(res) && length(res) == 1,
        msg = ".config_source_url: url must be a character value."
    )

    return(res)
}

.config_source_service <- function(source) {

    res <- .config_get(key = c("sources", source, "service"))

    assertthat::assert_that(
        is.character(res) && length(res) == 1,
        msg = ".config_source_url: service must be a character value."
    )

    return(res)
}

.config_source_s3class <- function(source) {

    res <- .config_get(key = c("sources", source, "s3_class"))

    assertthat::assert_that(
        is.character(res) && length(res) > 1,
        msg = ".config_source_s3class: s3_class must be a character value."
    )

    return(res)
}
