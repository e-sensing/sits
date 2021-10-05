#' @title sits configuration
#' @name sits_configuration
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description These functions load and show sits configurations.
#'
#' \code{sits_config()} loads the default configuration file and
#' the user provided configuration file. The final configuration is
#' obtained by overriding the options by the values provided in
#' \code{processing_bloat}, \code{rstac_pagination_limit},
#' \code{raster_api_package}, and \code{gdal_creation_options} parameters.
#'
#' \code{sits_config_show()} prints the current sits
#' configuration options. To show specific configuration options for
#' a source, a collection, or a palette, users can inform the corresponding
#' keys to \code{source}, \code{collection}, and \code{palette} parameters.
#'
#' \code{sits_list_collections()} prints the collections available
#' in each cloud service supported by sits. Users can select to get information
#' only for a single service by using the \code{source} parameter.
#'
#' @param processing_bloat       A \code{numeric} value to estimate
#' growth size of R memory relative to block size.
#' @param rstac_pagination_limit A \code{numeric} value indicating number of
#' items returned by STAC service.
#' @param raster_api_package     A \code{character} value indicating a
#' supported raster handling package.
#' @param gdal_creation_options  A \code{character} vector specifying GDAL
#' creation option for GeoTiff image format.
#' @param reset                  A \code{logical} value indicating if current
#'                               configuration options must be cleaned before
#'                               load config files. Default \code{FALSE}.
#' @param source                 A \code{character} value indicating a source
#' key entry to be shown in detail.
#' @param collection             A \code{character} value used in conjunction
#' with \code{source} parameter to indicate a collection key entry to be shown
#' in detail.
#' @param palette                A \code{character} value indicating a palette
#' to be shown in detail.
#'
#' @details
#' Users can provide additional configuration files, by specifying the
#' location of their file in the environmental variable
#' \code{SITS_CONFIG_USER_FILE}.
#'
#' To see the key entries and contents of the current configuration values,
#' use \code{sits_config_show()}.
NULL

#' @rdname sits_configuration
#'
#' @return
#' \code{sits_config()} returns a \code{list} containing the final
#' configuration options.
#'
#' @export
sits_config <- function(processing_bloat = NULL,
                        rstac_pagination_limit = NULL,
                        raster_api_package = NULL,
                        gdal_creation_options = NULL,
                        reset = FALSE) {

    # get and check the default configuration file path
    yml_file <- .config_file()

    # read the configuration parameters
    config <- yaml::yaml.load_file(input = yml_file,
                                   merge.precedence = "override")

    # clear current configuration
    if (reset)
        sits_env$config <- list()

    # set options defined in sits config
    do.call(.config_set_options, args = config)


    message(paste0("Using configuration file: ", yml_file))

    # try to find a valid user configuration file
    user_yml_file <- .config_user_file()

    if (file.exists(user_yml_file)) {
        message(paste("Additional configurations found in", user_yml_file))
        config <- yaml::yaml.load_file(input = user_yml_file,
                                       merge.precedence = "override")
        config <- utils::modifyList(sits_env[["config"]],
                                    config,
                                    keep.null = FALSE)

        # set options defined by user (via YAML file)
        # modifying existing configuration
        .config_set_options(
            processing_bloat = config[["processing_bloat"]],
            rstac_pagination_limit = config[["rstac_pagination_limit"]],
            raster_api_package = config[["raster_api_package"]],
            gdal_creation_options = config[["gdal_creation_options"]],
            sources = config[["sources"]],
            palettes = config[["palettes"]]
        )
    } else {
        message(paste("To provide additional configurations, create an",
                      "YAML file and inform its path to environment variable",
                      "'SITS_CONFIG_USER_FILE'."))
    }

    # set options defined by user (via parameters)
    # modifying existing configuration
    .config_set_options(processing_bloat = processing_bloat,
                        rstac_pagination_limit = rstac_pagination_limit,
                        raster_api_package = raster_api_package,
                        gdal_creation_options = gdal_creation_options)

    message(paste0("Using raster package: ", .config_raster_pkg()))

    return(invisible(sits_env$config))
}

#' @rdname sits_configuration
#'
#' @return
#' \code{sits_config_show()} returns a \code{list} containing the respective
#' configuration printed in the console.
#'
#' @export
sits_config_show <- function(source = NULL,
                             collection = NULL,
                             palette = NULL) {

    config <- sits_env$config

    if (!is.null(source)) {

        .check_chr(source,
                   allow_empty = FALSE,
                   len_min = 1,
                   len_max = 1)

        .check_chr_within(source,
                          within = .sources(),
                          discriminator = "one_of")

        config <- config[[c("sources", source)]]

        if (!is.null(collection)) {
            .check_chr(collection,
                       allow_empty = FALSE,
                       len_min = 1,
                       len_max = 1)

            .check_chr_within(collection,
                              within = .source_collections(source = source),
                              discriminator = "one_of")

            config <- config[[c("collections", collection)]]
        } else
            config <- lapply(config, function(x) {
                if (is.atomic(x))
                    return(x)
                list(names(x))
            })

    } else if (!is.null(palette)) {

        .check_chr(palette, allow_empty = FALSE, len_min = 1, len_max = 1)
        .check_chr_within(palette,
                          within = .config_palettes(),
                          discriminator = "one_of")

        config <- config[[c("palettes", palette)]]
    } else
        config <- lapply(config, function(x) {
            if (is.atomic(x))
                return(x)
            list(names(x))
        })

    config_txt <- yaml::as.yaml(config, indent = 4,
                                handlers = list(
                                    character = function(x) {
                                        res <- paste0(x, collapse = ", ")
                                        class(res) <- "verbatim"
                                        res
                                    },
                                    integer = function(x) {
                                        res <- paste0(x, collapse = ", ")
                                        class(res) <- "verbatim"
                                        res
                                    },
                                    numeric = function(x) {
                                        res <- paste0(x, collapse = ", ")
                                        class(res) <- "verbatim"
                                        res
                                    }))
    cat(config_txt, sep = "\n")
    return(invisible(config))
}

#' @rdname sits_configuration
#'
#' @return
#' \code{sits_list_collections()} prints the collections available in
#' each cloud service supported by sits.
#'
#' @export
sits_list_collections <- function(source = NULL) {

    # get sources available
    sources <- .sources(internal = FALSE)

    # if the user has required a source
    # check that it is valid
    if (!purrr::is_null(source)) {
        # check if source exists
        .check_chr_within(
            x = source,
            within = sources,
            msg = "invalid source value"
        )
        sources <- source
    }

    purrr::map(sources, function(s){

        cat(paste0(s, ":\n"))
        collections <- .source_collections(source = s)
        purrr::map(collections, function(c){

            cat(paste0("- ", c))
            cat(paste0(" (", .source_collection_satellite(s, c),
                       "/", .source_collection_sensor(s, c), ")\n"))
            cat("- bands: ")
            cat(.source_bands(s, c))
            cat("\n")
            if (.source_collection_open_data(source = s, collection = c)) {
                cat("- opendata collection ")
                if (.source_collection_open_data_token(source = s,
                                                       collection = c))
                    cat("(requires access token)")

            }
            cat("\n")
            cat("\n")
        })
    })
    return(invisible(NULL))
}

.config_set_options <- function(processing_bloat = NULL,
                                rstac_pagination_limit = NULL,
                                raster_api_package = NULL,
                                gdal_creation_options = NULL,
                                local_s3_class = NULL,
                                local_file_extensions = NULL,
                                sources = NULL,
                                palettes = NULL, ...) {
    # set caller to show in errors
    .check_set_caller(".config_set_options")

    # initialize config
    if (!exists("config", envir = sits_env))
        sits_env$config <- list()

    # process processing_bloat
    if (!is.null(processing_bloat)) {

        .check_num(processing_bloat, min = 1, len_min = 1, len_max = 1,
                   msg = "Invalid 'processing_bloat' parameter")

        sits_env$config[["processing_bloat"]] <- processing_bloat
    }

    # process rstac_pagination_limit
    if (!is.null(rstac_pagination_limit)) {

        .check_num(rstac_pagination_limit, min = 1, len_min = 1, len_max = 1,

                   msg = "Invalid 'rstac_pagination_limit' parameter")

        sits_env$config[["rstac_pagination_limit"]] <- rstac_pagination_limit
    }

    # process raster_api_package
    if (!is.null(raster_api_package)) {

        .check_chr(raster_api_package, len_min = 1, len_max = 1,
                   msg = "invalid 'raster_api_package' parameter")

        .check_chr_within(raster_api_package,
                          within = .raster_supported_packages(),
                          discriminator = "one_of",
                          msg = "invalid 'raster_api_package' parameter")

        sits_env$config[["raster_api_package"]] <- raster_api_package
    }

    # process gdal_creation_options
    if (!is.null(gdal_creation_options)) {

        .check_chr(gdal_creation_options, allow_empty = FALSE,
                   regex = "^.+=.+$",
                   msg = "Invalid 'gdal_creation_options' parameter")

        sits_env$config[["gdal_creation_options"]] <- gdal_creation_options
    }

    # process local_s3_class
    if (!is.null(local_s3_class)) {

        .check_chr(local_s3_class, allow_empty = FALSE, len_min = 1,
                   msg = "Invalid 'local_s3_class' parameter")

        sits_env$config[["local_s3_class"]] <- local_s3_class
    }

    # process local_file_extensions
    if (!is.null(local_file_extensions)) {

        .check_chr(local_file_extensions, allow_empty = FALSE, len_min = 1,
                   msg = "Invalid 'local_file_extensions' parameter")

        sits_env$config[["local_file_extensions"]] <- local_file_extensions
    }

    # process sources
    if (!is.null(sources)) {

        .check_lst(sources, min_len = 1)

        names(sources) <- toupper(names(sources))

        sources <- lapply(sources, function(source) {

            # pre-condition
            .check_lst(source, min_len = 2,
                       msg = "invalid 'source' parameter")

            .check_chr_contains(names(source),
                                contains = c("s3_class", "collections"),
                                msg = "invalid 'source' parameter")

            names(source) <- tolower(names(source))

            # check source
            source <- .check_error({
                do.call(.config_new_source, args = source)
            }, msg = "invalid 'source' parameter")

            return(source)
        })

        # initialize sources
        if (is.null(sits_env$config[["sources"]]))
            sits_env$config[["sources"]] <- sources

        sits_env$config[["sources"]] <- utils::modifyList(
            sits_env$config[["sources"]],
            sources,
            keep.null = FALSE
        )

        # generate LOCAL source
        col_names <- unlist(
            lapply(.sources(internal = FALSE), function(source) {
                paste0(source, "/", .source_collections(source))
            })
        )
        local_collections <- lapply(col_names, function(x) {
            values <- strsplit(x, "/")[[1]]
            source <- values[[1]]
            col_name <- values[[2]]
            col <- .config_get(key = c("sources", source, "collections",
                                   col_name))

            c(col["bands"], col["satellite"], col["sensor"])
        })
        names(local_collections) <- col_names

        sits_env$config[["sources"]][["LOCAL"]] <- .config_new_source(
            s3_class = .config_local_s3_class(),
            collections = local_collections
        )
    }

    if (!is.null(palettes)) {

        # initialize palettes
        if (is.null(sits_env$config[["palettes"]]))
            sits_env$config[["palettes"]] <- palettes

        sits_env$config[["palettes"]] <- utils::modifyList(
            sits_env$config[["palettes"]],
            palettes,
            keep.null = FALSE
        )
    }

    # process extra parameters
    dots <- list(...)
    .check_lst(dots)

    if (length(dots) > 0) {

        sits_env$config <- utils::modifyList(
            sits_env$config,
            dots,
            keep.null = FALSE
        )
    }

    return(invisible(sits_env$config))
}


.config_new_source <- function(s3_class,
                               collections, ...,
                               service = NULL,
                               url = NULL) {
    # set caller to show in errors
    .check_set_caller(".config_new_source")

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
        .check_lst(collection, min_len = 1,
                   msg = "invalid 'collections' parameter")

        # collection members must be lower case
        names(collection) <- tolower(names(collection))

        collection <- .check_error({
            do.call(.config_new_collection, args = collection)
        }, msg = "invalid 'collections' parameter")
        return(collection)
    })

    # extra parameters
    dots <- list(...)
    .check_lst(dots, msg = "invalid extra arguments in collection")

    return(c(list(s3_class = s3_class,
                  service = service,
                  url = url,
                  collections = collections), dots))
}

.config_new_collection <- function(bands, ...,
                                   satellite = NULL,
                                   sensor = NULL) {
    # set caller to show in errors
    .check_set_caller(".config_new_collection")

    # check satellite
    .check_chr(satellite, allow_null = TRUE,
               msg = "invalid 'satellite' value")

    #  check sensor
    .check_chr(sensor, allow_null = TRUE,
               msg = "invalid 'sensor' value")

    # bands names is upper case
    names(bands) <- toupper(names(bands))

    # separate cloud and non-cloud bands
    non_cloud_bands <- bands[!names(bands) %in% .source_cloud()]
    cloud_band <- bands[names(bands) %in% .source_cloud()]

    non_cloud_bands <- lapply(non_cloud_bands, function(band) {

        # pre-condition
        .check_lst(bands, min_len = 1,
                   msg = "invalid 'bands' parameter")

        # bands' members are lower case
        names(band) <- tolower(names(band))

        band <- .check_error( {
            do.call(.config_new_band, args = band)
        }, msg = "invalid 'bands' parameter")

        return(band)
    })

    cloud_band <- lapply(cloud_band, function(cloud_band) {

        # pre-condition
        .check_lst(bands, min_len = 1,
                   msg = "invalid 'bands' parameter")

        # bands' members are lower case
        names(cloud_band) <- tolower(names(cloud_band))

        cloud_band <- .check_error({
            do.call(.config_new_cloud_band, args = cloud_band)
        }, msg = "invalid 'bands' parameter")

        return(cloud_band)
    })

    # extra parameters
    dots <- list(...)
    .check_lst(dots, msg = "invalid extra arguments in collection")

    res <- c(list(bands = c(non_cloud_bands, cloud_band)),
             "satellite" = satellite,
             "sensor" = sensor, dots)

    # post-condition
    .check_lst(res, min_len = 1,
               msg = "invalid 'collection' value")

    .check_lst(res$bands, min_len = 1,
               msg = "invalid collection 'bands' value")

    # return a new collection data
    return(res)
}

.config_new_band <- function(missing_value,
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

    .check_chr(resampling, len_min = 1, len_max = 1,
               msg = "invalid 'resampling' parameter")

    .check_chr_within(resampling,
                      within = .raster_resample_methods(sits_names = TRUE),
                      discriminator = "one_of",
                      msg = "invalid 'resampling' parameter")

    if (!is.null(resolutions))
        .check_num(resolutions, len_min = 1, allow_zero = FALSE,
                   msg = "invalid 'resolutions' parameter")

    .check_chr(band_name, allow_empty = FALSE, len_min = 1, len_max = 1,
               msg = "invalid 'band_name' value")

    # extra parameters
    dots <- list(...)
    .check_lst(dots, "invalid extra arguments in band")

    res <- c(list(missing_value = missing_value,
                  minimum_value = minimum_value,
                  maximum_value = maximum_value,
                  scale_factor = scale_factor,
                  offset_value = offset_value,
                  resampling = resampling,
                  band_name = band_name,
                  resolutions = resolutions), dots)

    # post-condition
    .check_lst(res, min_len = 8,
               msg = "invalid 'band' value")

    # return a band object
    return(res)
}

.config_new_cloud_band <- function(bit_mask,
                                   values,
                                   interp_values,
                                   resampling,
                                   resolutions,
                                   band_name, ...) {
    # set caller to show in errors
    .check_set_caller(".config_new_cloud_band")

    # pre-condition
    .check_lgl(bit_mask, len_min = 1, len_max = 1,
               msg = "invalid 'bit_mask' parameter")

    .check_lst(values, fn_check = .check_chr,
               len_min = 1, len_max = 1,
               msg = "invalid cloud 'values' parameter")

    .check_num(interp_values, len_min = 1, is_integer = TRUE,
               msg = "invalid 'interp_values' parameter")

    .check_chr(resampling, len_min = 1, len_max = 1,
               msg = "invalid 'resampling' parameter")

    .check_chr_within(resampling,
                      within = .raster_resample_methods(sits_names = TRUE),
                      discriminator = "one_of",
                      msg = "invalid 'resampling' parameter")

    .check_chr(band_name, allow_empty = FALSE, len_min = 1, len_max = 1,
               msg = "invalid 'band_name' value")

    # extra parameters
    dots <- list(...)
    .check_lst(dots, "invalid extra arguments in cloud band")

    res <- c(list(bit_mask = bit_mask,
                  values = values,
                  interp_values = interp_values,
                  resampling = resampling,
                  resolutions = resolutions,
                  band_name = band_name), dots)

    # post-condition
    .check_lst(res, min_len = 6,
               msg = "invalid 'band' value")

    # return a cloud band object
    return(res)
}

#' @title Get values from config file
#' @name config_functions
#'
#' @description Functions that get values from config file.
#'
#' @keywords internal
#'
#' @param collection Collection to be searched in the data source.
#' @param data       A sits data cube.
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
    yml_file <- Sys.getenv("SITS_CONFIG_USER_FILE")

    # check if the file exists
    if (nchar(yml_file) > 0) {
        .check_warn(
            .check_file(yml_file,
                        msg = paste("invalid configuration file informed in",
                                    "SITS_CONFIG_USER_FILE"))
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

    # set default
    if (is.null(res))
        res <- default

    # post-condition
    .check_null(res,
                msg = paste("key",
                            paste0("'", paste0(key, collapse = "$"), "'"),
                            "not found"))

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
    .check_chr(res, allow_empty = FALSE,
               msg = paste("invalid names for",
                           paste0("'", paste0(key, collapse = "$"), "'"),
                           "key")
    )

    return(res)
}

#' @rdname config_functions
.config_gtiff_default_options <- function() {

    res <- .config_get(key = c("gdal_creation_options"))

    # post-condition
    .check_chr(res, allow_empty = FALSE,
               msg = "invalid 'gdal_creation_options' in config file")

    return(res)
}

#' @rdname config_functions
.config_local_file_extensions <- function() {

    res <- .config_get(key = c("local_file_extensions"))

    # post-condition
    .check_chr(res, len_min = 1,
               msg = "invalid 'file_extensions' in config file")

    return(res)
}

#' @rdname config_functions
.config_local_s3_class <- function() {

    res <- .config_get(key = c("local_s3_class"))

    # post-condition
    .check_chr(res, len_min = 1,
               msg = "invalid 'local_s3_class' in config file")

    return(res)
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

    # pre-condition
    .config_palette_check(palette = palette)

    # get the names of the colors in the chosen pallete
    color_names <- .config_get(key = c("palettes", palette))

    .check_chr_within(
        x = labels,
        within = names(color_names),
        msg = "some labels are missing from the palette"
    )
    colors <- color_names[labels]

    # simplify
    colors <- unlist(colors)

    # post-condition
    .check_chr(colors,
               len_min = length(labels),
               len_max = length(labels),
               is_named = TRUE,
               msg = "invalid 'color' values")

    return(colors)
}

.config_palette_check <- function(palette) {

    # check if palette name exists
    .check_chr(palette, len_min = 1, len_max = 1,
               msg = "invalid 'palette' parameter")
    .check_chr_within(palette, within = .config_palettes(),
                      msg = "invalid 'palette' parameter")
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

    # post-condition
    .check_chr(res, len_min = 1, len_max = 1,
               msg = "invalid 'raster_api_package' in config file")

    .check_chr_within(res,
                      within = .raster_supported_packages(),
                      discriminator = "one_of",
                      msg = "invalid 'raster_api_package' in config file")

    return(res)
}
