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
    sits_env$config <- yaml::yaml.load_file(input = yml_file,
                                            merge.precedence = "override")

    # try to find a valid user configuration file
    user_yml_file <- .config_user_file()

    if (file.exists(user_yml_file)) {
        config_user <- yaml::yaml.load_file(input = user_yml_file,
                                            merge.precedence = "override")
        sits_env$config <- utils::modifyList(x = sits_env$config,
                                             val = config_user)
    }

    sits_config_info()

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
                      "YAML file and inform its path to environment variable",
                      "'SITS_USER_CONFIG_FILE'."))
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

#' @title Get values from config file
#' @name config_functions
#'
#' @description Functions that get values from config file.
#'
#' @keywords internal
#'
#' @param add_cloud  A logical parameter that indicates the addition of cloud
#'  band information in the return of a function.
#' @param collection Collection to be searched in the data source.
#' @param data       A sits data cube.
#' @param default    Default value if the specified key is not found.
#' @param fn_filter  Filter function that will be applied in one key from config
#'  file.
#' @param key        Character that represents which key is to be fetched from
#'  the config file.
#' @param labels     Vector with labels.
#' @param pallete    The palette that should be chosen based on the
#'  configuration file.
#' @param simplify   A logical value that specifies whether the return should be
#'  in vector form, if true, or list form, if false. Default value is FALSE.
#' @param source     Source of data cube
#' @param ...        Additional parameters.
#'
#' @return Functions that search for values from a key or collection
#'  return atomic values. Check functions return invisible null values or give
#'  an error.
NULL

#' @rdname config_functions
.config_file <- function() {

    # load the default configuration file
    yml_file <- system.file("extdata", "config.yml", package = "sits")

    # check that the file name is valid
    .check_file(yml_file, msg = "invalid configuration file")

    return(yml_file)
}

#' @rdname config_functions
.config_user_file <- function() {

    # load the default configuration file
    yml_file <- Sys.getenv("SITS_USER_CONFIG_FILE")

    # check if the file exists
    if (nchar(yml_file) > 0) {
        .check_file(
            yml_file,
            msg = sprintf(paste(
                "invalid configuration file informed in",
                "SITS_USER_CONFIG_FILE"),
                yml_file)
        )
    }

    return(yml_file)
}

#' @rdname config_functions
.config_get <- function(key, default = NULL) {

    res <- tryCatch({
        sits_env$config[[key]]
    },
    error = function(e) {
        return(default)
    })

    # post-condition
    .check_that(
        !is.null(res),
        local_msg = paste("key", paste0(key, collapse = "$"),
                          "not found. Please, check the config file.")
    )

    return(res)
}

#' @rdname config_functions
.config_names <- function(key) {

    res <- tryCatch({
        names(sits_env$config[[key]])
    },
    error = function(e) {
        return(NULL)
    })

    # post-condition
    .check_that(
        !is.null(res),
        local_msg = paste("key", paste0(key, collapse = "$"),
                          "not found. Please, check the config file.")
    )

    .check_chr(res, allow_empty = FALSE,
               msg = "invalid names")

    return(res)
}

#' @rdname config_functions
.config_aws_default_region <- function(source,
                                       collection) {

    # TO-DO: pre-condition
    # check source and collection

    res <- .config_get(key = c("sources", source, "collections", collection,
                               "AWS", "AWS_DEFAULT_REGION"),
                       default = NA)

    # post-condition
    .check_chr(res, allow_empty = TRUE,
               len_min = 1, len_max = 1,
               msg = "invalid AWS_DEFAULT_REGION")

    return(res)
}

#' @rdname config_functions
.config_aws_endpoint <- function(source,
                                 collection) {
    # TO-DO: pre-condition
    # check source and collection

    res <- .config_get(key = c("sources", source, "collections", collection,
                               "AWS", "AWS_S3_ENDPOINT"),
                       default = NA)

    # post-condition
    .check_chr(res, allow_empty = TRUE,
               len_min = 1, len_max = 1,
               msg = "invalid AWS_S3_ENDPOINT")

    return(res)
}

#' @rdname config_functions
.config_aws_request_payer <- function(source,
                                      collection) {
    # TO-DO: pre-condition
    # check source and collection

    res <- .config_get(key = c("sources", source, "collections", collection,
                               "AWS", "AWS_REQUEST_PAYER"),
                       default = NA)

    # post-condition
    .check_chr(res, allow_empty = TRUE,
               len_min = 1, len_max = 1,
               msg = "invalid AWS_REQUEST_PAYER")

    return(res)
}

#' @rdname config_functions
.config_bands <- function(source,
                          collection, ...,
                          fn_filter = NULL,
                          add_cloud = TRUE) {

    # TO-DO: pre-condition
    # check source and collection

    res <- .config_names(key = c("sources", source, "collections",
                                 collection, "bands"))

    if (!add_cloud)
        res <- res[res != "CLOUD"]

    if (!is.null(fn_filter)) {
        select <- vapply(res, function(band) {
            fn_filter(.config_get(key = c("sources", source, "collections",
                                          collection, "bands", band)))
        }, logical(1))

        res <- res[select]
    }

    # TO-DO: post-condition
    # check bands are non-NA character


    return(res)
}

#' @rdname config_functions
.config_bands_reap <- function(source,
                               collection,
                               key, ...,
                               bands = NULL,
                               fn_filter = NULL,
                               add_cloud = TRUE,
                               default = NULL) {

    # TO-DO: pre-condition
    # check source and collection

    if (is.null(bands))
        bands <- .config_bands(source = source,
                               collection = collection,
                               fn_filter = fn_filter,
                               add_cloud = add_cloud)

    # pre-condition
    .check_chr(bands, allow_na = FALSE, allow_empty = FALSE, len_min = 1,
               msg = "invalid bands")

    # always returns a list!
    res <- lapply(bands, function(band) {
        .config_get(key = c("sources", source, "collections",
                            collection, "bands", band, key),
                    default = default)
    })

    names(res) <- bands

    return(res)
}

#' @rdname config_functions
.config_bands_band_name <- function(source,
                                    collection, ...,
                                    bands = NULL) {
    # TO-DO: pre-condition
    # check source and collection

    res <- .config_bands_reap(source = source,
                              collection = collection,
                              key = "band_name",
                              bands = bands)

    # simplify to a unnamed character vector
    res <- unlist(res, recursive = FALSE, use.names = FALSE)

    # post-conditions
    .check_chr(res, allow_na = FALSE, allow_empty = FALSE,
               len_min = length(bands), len_max = length(bands),
               msg = "inconsistent 'band_name' values")

    return(res)
}

#' @rdname config_functions
.config_bands_resolutions <- function(source,
                                      collection, ...,
                                      bands = NULL,
                                      fn_filter = NULL,
                                      add_cloud = TRUE) {
    # TO-DO: pre-condition
    # check source and collection

    res <- .config_bands_reap(source = source,
                              collection = collection,
                              key = "resolutions",
                              bands = bands,
                              fn_filter = fn_filter,
                              add_cloud = add_cloud)

    # cannot simplify as each element can have length greater than one
    # post-condition
    .check_lst(res, fn_check = .check_num, min = 0,
               allow_zero = FALSE, len_min = 1,
               msg = "invalid 'resolutions' in config file")

    return(res)
}

#' @rdname config_functions
.config_cloud <- function() {

    return("CLOUD")
}

.config_cloud_bit_mask <- function(source,
                                   collection) {
    # TO-DO: pre-condition
    # check source and collection

    res <- .config_get(key = c("sources", source, "collections", collection,
                               "bands", "CLOUD", "bit_mask"))

    # post-condition
    .check_lgl(res, len_min = 1, len_max = 1,
               msg = "invalid 'bit_mask' value in config file")

    return(res)
}

#' @rdname config_functions
.config_cloud_values <- function(source,
                                 collection) {
    # TO-DO: pre-condition
    # check source and collection

    res <- .config_get(key = c("sources", source, "collections", collection,
                               "bands", "CLOUD", "values"))

    # post-condition
    .check_lst(res, min_len = 1, max_len = 1,
               msg = "invalid cloud 'values' in config file")

    return(res)
}

#' @rdname config_functions
.config_cloud_interp_values <- function(source,
                                        collection) {
    # TO-DO: pre-condition
    # check source and collection

    res <- .config_get(key = c("sources", source, "collections", collection,
                               "bands", "CLOUD", "interp_values"))

    # post-condition
    .check_num(res, msg = "invalid 'interp_values' in config file")

    return(res)
}

#' @rdname config_functions
.config_collections <- function(source) {

    res <- .config_names(c("sources", source, "collections"))

    return(res)
}

#' @rdname config_functions
.config_gtiff_default_options <- function() {

    res <- .config_get(key = c("GTiff_default_options"))

    # post-condition
    .check_chr(res, len_min = 1, len_max = 1,
               msg = "invalid 'GTiff_default_options' in config file")

    return(res)
}

#' @rdname config_functions
.config_local_file_extensions <- function() {

    res <- .config_get(key = c("sources", "LOCAL", "file_extensions"))

    # post-condition
    .check_chr(res, len_min = 1,
               msg = "invalid 'file_extensions' in config file")

    return(res)
}

#' @rdname config_functions
.config_memory_bloat <- function() {

    res <- .config_get(key = c("R_memory_bloat"))

    # post-condition
    .check_num(res, min = 1, len_min = 1, len_max = 1,
               msg = "invalid 'R_memory_bloat' in config file")

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

#' @rdname config_functions
.config_palettes <- function() {

    res <- .config_names(key = "palettes")

    # post-condition
    .check_chr(res, len_min = 1)

    return(res)
}

#' @rdname config_functions
.config_palette_colors <- function(labels, ...,
                                   palette = "default") {

    res <- .config_get(key = c("palettes", palette))[labels]
    names(res) <- labels

    if (any(is.na(res))) {

        random <- grDevices::colors()
        random <- random[!random %in% res]
        res[is.na(res)] <- sample(random, sum(is.na(res)))
    }

    # post-condition
    .check_chr(res, allow_empty = FALSE, is_named = TRUE,
               msg = "invalid ")

    return(res)
}
#' @rdname config_functions
.config_processing_bloat <- function() {

    res <- .config_get(key = c("R_processing_bloat"))

    # post-condition
    .check_num(res, min = 1, len_min = 1, len_max = 1,
               msg = "invalid 'R_processing_bloat' in config file")

    return(res)
}

#' @rdname config_functions
.config_rstac_limit <- function() {

    res <- .config_get(key = c("rstac_pagination_limit"))

    # post-condition
    .check_num(res, min = 1, len_min = 1, len_max = 1,
               msg = "invalid 'rstac_pagination_limit' in config file")

    return(res)
}

#' @rdname config_functions
.config_raster_pkg <- function() {

    res <- .config_get(key = c("R_raster_pkg"))

    .check_chr(res, allow_empty = FALSE,
               choices = c("terra", "raster"), len_min = 1, len_max = 1,
               msg = "invalid 'R_raster_pkg' in config file")

    return(res)
}

#' @rdname config_functions
.config_sources <- function() {

    res <- .config_names(c("sources"))

    # post-condition
    .check_chr(res, allow_empty = FALSE, len_min = 1,
               msg = "invalid 'sources' in config file")

    return(res)
}

#' @rdname config_functions
.config_source_url <- function(source) {
    # TO-DO: pre-condition
    # check source

    res <- .config_get(key = c("sources", source, "url"))

    # post-condition
    .check_chr(res, allow_empty = FALSE,
               len_min = 1, len_max = 1,
               msg = sprintf("invalid 'url' for source %s in config file",
                             source))

    return(res)
}

#' @rdname config_functions
.config_source_service <- function(source) {
    # TO-DO: pre-condition
    # check source

    res <- .config_get(key = c("sources", source, "service"))

    # post-condition
    .check_chr(res, allow_empty = FALSE,
               len_min = 1, len_max = 1,
               msg = sprintf("invalid 'service' for source %s in config file",
                             source))

    return(res)
}

#' @rdname config_functions
.config_source_s3class <- function(source) {
    # TO-DO: pre-condition
    # check source

    res <- .config_get(key = c("sources", source, "s3_class"))

    # post-condition
    .check_chr(res, allow_empty = FALSE, len_min = 1,
               msg = sprintf("invalid 's3_class' for source %s in config file",
                             source))

    return(res)
}
