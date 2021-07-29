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

.config_file <- function() {

    # load the default configuration file
    yml_file <- system.file("extdata", "config.yml", package = "sits")

    # check that the file name is valid
    .check_chr(yml_file, allow_na = FALSE, allow_empty = FALSE,
               min_len = 1, max_len = 1, msg = "invalid configuration file")

    # check if the file exists
    assertthat::assert_that(
        file.exists(yml_file),
        msg = sprintf(".config_file: file %s does not exist.", yml_file)
    )

    return(yml_file)
}

.config_user_file <- function() {

    # load the default configuration file
    yml_file <- Sys.getenv("SITS_USER_CONFIG_FILE")

    # check that the file name is valid
    .check_chr(yml_file, allow_na = FALSE, allow_empty = TRUE,
               min_len = 1, max_len = 1,
               msg = "invalid SITS_USER_CONFIG_FILE environment variable")

    # check if the file exists
    if (nchar(yml_file) > 0) {
        assertthat::assert_that(
            file.exists(yml_file),
            msg = sprintf(paste(
                ".config_user_file: file %s does not exist.",
                "Please, check your environment for the variable",
                "SITS_USER_CONFIG_FILE"),
                yml_file)
        )
    }

    return(yml_file)
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
                    "not found. Please, check the config file.")
    )

    if (simplify)
        return(unlist(res))

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

    assertthat::assert_that(
        !is.null(res),
        msg = paste(".config_names:", paste0(key, collapse = "$"),
                    "not found. Please, check the config file.")
    )

    .check_chr(res, allow_na = FALSE, allow_empty = FALSE)

    return(res)
}

#' @rdname config_functions
.config_aws_default_region <- function(source,
                                       collection) {

    res <- .config_get(key = c("sources", source, "collections", collection,
                               "AWS", "AWS_DEFAULT_REGION"),
                       default = NA)

    # post-condition
    .check_chr(res, allow_na = FALSE, allow_empty = TRUE,
               min_len = 1, max_len = 1, msg = "invalid AWS_DEFAULT_REGION")

    return(res)
}

#' @rdname config_functions
.config_aws_endpoint <- function(source,
                                 collection) {

    res <- .config_get(key = c("sources", source, "collections", collection,
                               "AWS", "AWS_S3_ENDPOINT"),
                       default = NA)

    # post-condition
    .check_chr(res, allow_na = FALSE, allow_empty = TRUE,
               min_len = 1, max_len = 1, msg = "invalid AWS_S3_ENDPOINT")

    return(res)
}

#' @rdname config_functions
.config_aws_request_payer <- function(source,
                                      collection) {

    res <- .config_get(key = c("sources", source, "collections", collection,
                               "AWS", "AWS_REQUEST_PAYER"),
                       default = NA)

    # post-condition
    .check_chr(res, allow_na = FALSE, allow_empty = TRUE,
               min_len = 1, max_len = 1, msg = "invalid AWS_REQUEST_PAYER")

    return(res)
}

#' @rdname config_functions
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

        assertthat::assert_that(
            any(select),
            msg = ".config_bands: no bands matched criteria."
        )

        return(res[select])
    }

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

    .check_chr(bands, allow_na = FALSE, allow_empty = FALSE, min_len = 1,
               allow_null = TRUE, "invalid informed bands")

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

#' @rdname config_functions
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

#' @rdname config_functions
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

    # post-condition
    .check_num(res, allow_na = FALSE, min = 1e-08, len_min = 1,
               msg = "invalid resolution")

    return(res)
}

#' @rdname config_functions
.config_cloud <- function() {

    return("CLOUD")
}

.config_cloud_bit_mask <- function(source,
                                   collection) {

    res <- .config_get(key = c("sources", source, "collections", collection,
                               "bands", "CLOUD", "bit_mask"))

    return(res)
}

#' @rdname config_functions
.config_cloud_values <- function(source,
                                 collection) {

    res <- .config_get(key = c("sources", source, "collections", collection,
                               "bands", "CLOUD", "values"))

    return(res)
}

#' @rdname config_functions
.config_cloud_interp_values <- function(source,
                                        collection) {

    res <- .config_get(key = c("sources", source, "collections", collection,
                               "bands", "CLOUD", "interp_values"))

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

    return(res)
}

#' @rdname config_functions
.config_local_file_extensions <- function() {

    res <- .config_get(key = c("sources", "LOCAL", "file_extensions"))

    return(res)
}

#' @rdname config_functions
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

#' @rdname config_functions
.config_palettes <- function() {

    .config_names(c("palettes"))
}

#' @rdname config_functions
.config_palette_colors <- function(labels, ...,
                                   palette = "default") {

    values <- .config_get(key = c("palettes", palette))[labels]
    names(values) <- labels

    if (any(is.na(values))) {

        random <- grDevices::colors()
        random <- random[!random %in% values]
        values[is.na(values)] <- sample(random, sum(is.na(values)))
    }

    return(values)
}
#' @rdname config_functions
.config_processing_bloat <- function() {

    res <- .config_get(key = c("R_processing_bloat"))

    # post-condition
    .check_num(res, allow_na = FALSE, min = 1, len_min = 1, len_max = 1,
               msg = "invalid 'R_processing_bloat' in config file")

    return(res)
}

#' @rdname config_functions
.config_rstac_limit <- function() {

    res <- .config_get(key = c("rstac_pagination_limit"))

    # post-condition
    .check_num(res, allow_na = FALSE, min = 1, len_min = 1, len_max = 1,
               msg = "invalid 'rstac_pagination_limit' in config file")

    return(res)
}

#' @rdname config_functions
.config_raster_pkg <- function() {

    res <- .config_get(key = c("R_raster_pkg"))

    .check_chr(res, allow_na = FALSE, allow_empty = FALSE,
               choices = c("terra", "raster"), min_len = 1, max_len = 1,
               msg = "invalid 'R_raster_pkg' in config file")

    return(res)
}

#' @rdname config_functions
.config_sources <- function() {

    res <- .config_names(c("sources"))

    .check_lst(res, allow_unnamed = FALSE, min_len = 1,
               msg = "invalid sources in config file")

    return(res)
}

#' @rdname config_functions
.config_source_url <- function(source) {

    res <- .config_get(key = c("sources", source, "url"))

    .check_chr(res, allow_na = FALSE, allow_empty = FALSE,
               min_len = 1, max_len = 1,
               msg = sprintf("invalid url for source %s in config file",
                             source))

    return(res)
}

#' @rdname config_functions
.config_source_service <- function(source) {

    res <- .config_get(key = c("sources", source, "service"))

    .check_chr(res, allow_na = FALSE, allow_empty = FALSE,
               min_len = 1, max_len = 1,
               msg = sprintf("invalid service for source %s in config file",
                             source))

    return(res)
}

#' @rdname config_functions
.config_source_s3class <- function(source) {

    res <- .config_get(key = c("sources", source, "s3_class"))

    .check_chr(res, allow_na = FALSE, allow_empty = FALSE, min_len = 1,
               msg = sprintf("invalid s3_class for source %s in config file",
                             source))

    return(res)
}
