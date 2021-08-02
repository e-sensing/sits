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
#' @param processing_bloat       A \code{numeric} value to estimate
#' growth size of R memory relative to block size.
#' @param rstac_pagination_limit A \code{numeric} value indicating number of
#' items returned by STAC service.
#' @param gdal_creation_options  A \code{character} vector specifying GDAL
#' creation option for GeoTiff image format.
#' @param sources                A named \code{list} object containing all
#' supported data cubes sources. Used in conjunction with
#' \code{sits_config_new_source()}.
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
sits_config <- function(processing_bloat = NULL,
                        rstac_pagination_limit = NULL,
                        raster_api_package = NULL,
                        gdal_creation_options = NULL,
                        sources = NULL) {

    # get and check the default configuration file path
    yml_file <- .config_file()

    # read the configuration parameters
    config <- yaml::yaml.load_file(input = yml_file,
                                   merge.precedence = "override")

    sits_env$config <- config

    # set options defined in sits config
    .config_set_options(
        processing_bloat = config[["processing_bloat"]],
        rstac_pagination_limit = config[["rstac_pagination_limit"]],
        raster_api_package = config[["raster_api_package"]],
        gdal_creation_options = config[["gdal_creation_options"]],
        sources = config[["sources"]]
    )


    message(paste0("Using configuration file: ", yml_file))

    # try to find a valid user configuration file
    user_yml_file <- .config_user_file()

    if (file.exists(user_yml_file)) {
        message(paste("Additional configurations found in", user_yml_file))
        config <- yaml::yaml.load_file(input = user_yml_file,
                                       merge.precedence = "override")

        # set options defined by user (via YAML file)
        .config_set_options(
            processing_bloat = config[["processing_bloat"]],
            rstac_pagination_limit = config[["rstac_pagination_limit"]],
            raster_api_package = config[["raster_api_package"]],
            gdal_creation_options = config[["gdal_creation_options"]],
            sources = config[["sources"]]
        )
    } else {
        message(paste("To provide additional configurations, create an",
                      "YAML file and inform its path to environment variable",
                      "'SITS_USER_CONFIG_FILE'."))
    }

    # set options defined by user (via parameters)
    .config_set_options(processing_bloat = processing_bloat,
                        rstac_pagination_limit = rstac_pagination_limit,
                        raster_api_package = raster_api_package,
                        gdal_creation_options = gdal_creation_options,
                        sources = sources)

    message(paste0("Using raster package: ", .config_raster_pkg()))

    return(invisible(NULL))
}

.config_set_options <- function(processing_bloat = NULL,
                                rstac_pagination_limit = NULL,
                                raster_api_package = NULL,
                                gdal_creation_options = NULL,
                                sources = NULL) {

    if (!is.null(processing_bloat)) {

        .check_num(processing_bloat, min = 1, len_min = 1, len_max = 1,
                   msg = "Invalid 'processing_bloat' parameter")

        sits_env$config[["processing_bloat"]] <- processing_bloat
    }
    if (!is.null(rstac_pagination_limit)) {

        .check_num(rstac_pagination_limit, min = 1, len_min = 1, len_max = 1,
                   msg = "Invalid 'rstac_pagination_limit' parameter")

        sits_env$config[["rstac_pagination_limit"]] <- rstac_pagination_limit
    }

    if (!is.null(raster_api_package)) {

        .check_chr(raster_api_package, choices = .raster_supported_packages(),
                   len_min = 1, len_max = 1,
                   msg = "Invalid 'raster_api_package' parameter")

        sits_env$config[["raster_api_package"]] <- raster_api_package
    }

    if (!is.null(gdal_creation_options)) {

        .check_chr(gdal_creation_options, allow_empty = FALSE,
                   regex = "^.+=.+$",
                   msg = "Invalid 'gdal_creation_options' parameter")

        sits_env$config[["gdal_creation_options"]] <- gdal_creation_options
    }

    if (!is.null(sources)) {

        .check_lst(sources, min_len = 1)

        names(sources) <- toupper(names(sources))

        sources <- lapply(sources, function(source) {

            # pre-condition
            .check_chr_choices(c("s3_class", "collections"),
                               choices = names(source),
                               msg = "invalid 'source' parameter")

            .check_lst(source, min_len = 2,
                       msg = "invalid 'source' parameter")

            names(source) <- tolower(names(source))

            # check source
            .check_error({
                source <- sits_config_new_source(
                    s3_class = source[["s3_class"]],
                    collections = source[["collections"]],
                    service = source[["service"]],
                    url = source[["url"]]
                )
            }, msg = "invalid 'source' parameter"
            )

            return(source)
        })

        sits_env$config[["sources"]] <- utils::modifyList(
            sits_env$config[["sources"]],
            sources
        )
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
    config <- yaml::as.yaml(sits_env$config)

    # read the configuration parameters
    message("sits configuration")
    cat(config, sep = "\n")

    # try to find a valid user configuration file
    user_yml_file <- .config_user_file()

    if (file.exists(user_yml_file)) {
        message("User configuration file - overrides default config")
        cat(readLines(user_yml_file), sep = "\n")
    }

    return(invisible(NULL))
}


sits_config_new_source <- function(s3_class,
                                   collections, ...,
                                   service = NULL,
                                   url = NULL) {

    # pre-condition
    .check_chr(s3_class, allow_empty = FALSE, len_min = 1,
               msg = "invalid 's3_class' parameter")

    if (!is.null(service))
        .check_chr(service, allow_empty = FALSE, len_min = 1, len_max = 1,
                   msg = "invalid 'service' parameter")

    if (!is.null(url))
        .check_chr(url, allow_empty = FALSE, len_min = 1, len_max = 1,
                   regex = '^(http|https)://[^ "]+$',
                   msg = "invalid 'url' parameter")

    .check_lst(collections, min_len = 1)

    names(collections) <- toupper(names(collections))

    collections <- lapply(collections, function(collection) {

        # pre-condition
        .check_chr_choices("bands", choices = names(collection),
                           msg = "invalid 'collections' parameter")

        .check_lst(collection, min_len = 1,
                   msg = "invalid 'collections' parameter")

        names(collection) <- tolower(names(collection))
        names(collection[["bands"]]) <- toupper(names(collection[["bands"]]))

        # check collection
        bands <- !names(collection[["bands"]]) %in% .config_cloud()
        .check_error(
            sits_config_new_collection(
                bands = collection[["bands"]][bands],
                cloud_band = collection[["bands"]][[.config_cloud()]],
                other_keys = collection[!names(collection) %in% "bands"]
            ),
            msg = "invalid 'collections' parameter"
        )
    })

    res <- as.list(environment())

    return(res)
}

sits_config_new_collection <- function(bands, ...,
                                       cloud_band = NULL,
                                       other_keys = NULL) {

    names(bands) <- toupper(names(bands))

    # pre-condition
    .check_lst(bands, min_len = 1,
               fn_check = function(band) {
                   do.call(sits_config_new_band, args = band)
                   return(TRUE)
               },
               msg = "invalid 'bands' parameter")

    if (!is.null(cloud_band))
        .check_error(do.call(sits_config_new_cloud_band, cloud_band),
                     msg = "invalid 'bands' parameter")

    if (length(other_keys) > 0)
        .check_lst(other_keys, msg = "invalid extra arguments in collection")

    cloud_band <- list(cloud_band)
    names(cloud_band) <- .config_cloud()

    return(c(list(bands = c(bands, cloud_band)), other_keys))
}

sits_config_new_band <- function(missing_value,
                                 minimum_value,
                                 maximum_value,
                                 scale_factor,
                                 offset_value,
                                 resampling,
                                 band_name, ...,
                                 resolutions = NULL) {

    # pre-condition
    .check_num(missing_value, len_min = 1, len_max = 1,
               msg = "invalid 'missing_value' parameter")

    .check_num(minimum_value, len_min = 1, len_max = 1,
               msg = "invalid 'minimum_value' parameter")

    .check_num(maximum_value, len_min = 1, len_max = 1,
               msg = "invalid 'maximum_value' parameter")

    .check_num(scale_factor, len_min = 1, len_max = 1,
               allow_zero = FALSE,
               msg = "invalid 'scale_factor' parameter")

    .check_num(offset_value, len_min = 1, len_max = 1,
               msg = "invalid 'offset_value' parameter")

    .check_chr(resampling, choices = .raster_resample_methods(),
               len_min = 1, len_max = 1,
               msg = "invalid 'resampling' parameter")

    if (!is.null(resolutions))
        .check_num(resolutions, len_min = 1, allow_zero = FALSE,
                   msg = "invalid 'resolutions' parameter")

    .check_chr(band_name, allow_empty = FALSE, len_min = 1, len_max = 1,
               msg = "invalid 'band_name' value")

    res <- as.list(environment())

    dots <- list(...)

    .check_lst(dots)

    return(res)
}

sits_config_new_cloud_band <- function(bit_mask,
                                       values,
                                       interp_values,
                                       resampling,
                                       resolutions,
                                       band_name) {

    # pre-condition
    .check_lgl(bit_mask, len_min = 1, len_max = 1,
               msg = "invalid 'bit_mask' parameter")

    .check_lst(values, fn_check = .check_chr,
               len_min = 1, len_max = 1,
               msg = "invalid cloud 'values' parameter")

    .check_num(interp_values, len_min = 1, is_integer = TRUE,
               msg = "invalid 'interp_values' parameter")

    .check_chr(resampling, choices = .raster_resample_methods(),
               len_min = 1, len_max = 1,
               msg = "invalid 'resampling' parameter")

    .check_chr(band_name, allow_empty = FALSE, len_min = 1, len_max = 1,
               msg = "invalid 'band_name' value")

    res <- as.list(environment())

    return(res)
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
        .check_warn(
            .check_file(yml_file,
                        msg = paste("invalid configuration file informed in",
                                    "SITS_USER_CONFIG_FILE"))
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
                          "not found. Please, check the config file")
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

    res <- .config_get(key = c("gdal_creation_options"))

    # post-condition
    .check_chr(res, len_min = 1, len_max = 1,
               msg = "invalid 'gdal_creation_options' in config file")

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
    .check_lst(res,
               min_len = length(labels),
               max_len = length(labels),
               fn_check = .check_chr,
               len_min = 1,
               len_max = 1,
               allow_empty = FALSE,
               msg = "invalid colors ")

    return(unlist(res, use.names = FALSE))
}
#' @rdname config_functions
.config_processing_bloat <- function() {

    res <- .config_get(key = c("processing_bloat"))

    # post-condition
    .check_num(res, min = 1, len_min = 1, len_max = 1,
               msg = "invalid 'processing_bloat' in config file")

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

    res <- .config_get(key = c("raster_api_package"))

    .check_chr(res, allow_empty = FALSE,
               choices = .raster_supported_packages(),
               len_min = 1, len_max = 1,
               msg = "invalid 'raster_api_package' in config file")

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
