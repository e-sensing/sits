#' @title Set configuration parameters
#' @name .conf_set_options
#' @noRd
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param processing_bloat       Estimated growth size of R memory relative
#'                               to block size.
#' @param rstac_pagination_limit Limit of number of items returned by STAC.
#' @param gdal_creation_options  GDAL creation options for GeoTiff.
#' @param gdalcubes_chunk_size   Chunk size to be used by gdalcubes
#' @param sources                Data sources
#' @param colors                 Color values
#' @param ...                    Other configuration params
.conf_set_options <- function(processing_bloat = NULL,
                              rstac_pagination_limit = NULL,
                              gdal_creation_options = NULL,
                              gdalcubes_chunk_size = NULL,
                              sources = NULL,
                              colors = NULL, ...) {
    # set caller to show in errors
    .check_set_caller(".conf_set_options")

    # initialize config
    if (!exists("config", envir = sits_env)) {
        sits_env$config <- list()
    }

    # process processing_bloat
    if (!is.null(processing_bloat)) {
        .check_num(processing_bloat,
            min = 1, len_min = 1, len_max = 1, max = 10,
            is_integer = TRUE,
            msg = "invalid 'processing_bloat' parameter"
        )
        sits_env$config[["processing_bloat"]] <- processing_bloat
    }

    # process rstac_pagination_limit
    if (!is.null(rstac_pagination_limit)) {
        .check_num(rstac_pagination_limit,
            min = 1, len_min = 1, len_max = 1, max = 500,
            is_integer = TRUE,
            msg = "invalid 'rstac_pagination_limit' parameter"
        )
        sits_env$config[["rstac_pagination_limit"]] <- rstac_pagination_limit
    }

    # process gdal_creation_options
    if (!is.null(gdal_creation_options)) {
        .check_chr(gdal_creation_options,
            allow_empty = FALSE,
            regex = "^.+=.+$",
            msg = "invalid 'gdal_creation_options' parameter"
        )
        sits_env$config[["gdal_creation_options"]] <- gdal_creation_options
    }
    # process gdalcubes_chunk_size
    if (!is.null(gdalcubes_chunk_size)) {
        .check_num(gdalcubes_chunk_size,
            min_len = 3,
            max_len = 3,
            is_named = FALSE,
            msg = "invalid gdalcubes chunk size"
        )
        sits_env$config[["gdalcubes_chunk_size"]] <- gdalcubes_chunk_size
    }
    # process sources
    if (!is.null(sources)) {
        .check_lst(sources, min_len = 1)

        # source names are uppercase
        names(sources) <- toupper(names(sources))

        sources <- lapply(sources, function(source) {
            # pre-condition
            .check_lst(source,
                min_len = 2,
                msg = "invalid 'source' parameter"
            )

            # check that source contains essential parameters
            .check_chr_contains(names(source),
                contains = c("s3_class", "collections"),
                msg = "invalid 'source' parameter"
            )
            names(source) <- tolower(names(source))

            # check source
            source <- .check_error(
                {
                    do.call(.conf_new_source, args = source)
                },
                msg = "invalid 'source' parameter"
            )
            return(source)
        })

        # initialize sources
        if (is.null(sits_env$config[["sources"]])) {
            sits_env$config[["sources"]] <- sources
        }

        sits_env$config[["sources"]] <- utils::modifyList(
            sits_env$config[["sources"]],
            sources,
            keep.null = FALSE
        )
    }
    # check and initialize palettes
    if (!is.null(colors)) {
        # initialize colors
        if (is.null(sits_env$config[["colors"]])) {
            sits_env$config[["colors"]] <- colors
        }
        # add colors
        sits_env$config[["colors"]] <- utils::modifyList(
            sits_env$config[["colors"]],
            colors,
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
#' @title Return the default configuration file
#' @name .conf_file
#' @keywords internal
#' @noRd
#' @return default configuration file
#'
.conf_file <- function() {
    # load the default configuration file
    yml_file <- system.file("extdata", "config.yml", package = "sits")

    # check that the file name is valid
    .check_file(yml_file, msg = "invalid configuration file")

    return(yml_file)
}

#' @title Return the internal configuration file (only for developers)
#' @name .conf_internals_file
#' @keywords internal
#' @noRd
#' @return default internal configuration file
.conf_internals_file <- function() {
    # load the default configuration file
    yml_file <- system.file("extdata", "config_internals.yml", package = "sits")

    # check that the file name is valid
    .check_file(yml_file, msg = "invalid configuration file")

    return(yml_file)
}
#' @title Return the default configuration file for colors
#' @name .conf_colors_file
#' @keywords internal
#' @noRd
#' @return default color configuration file
.conf_colors_file <- function() {
    # load the default configuration file
    yml_file <- system.file("extdata", "config_colors.yml", package = "sits")

    # check that the file name is valid
    .check_file(yml_file, msg = "invalid configuration file")

    return(yml_file)
}
#' @title Get color table
#' @name .conf_load_color_table
#' @description Loads the default color table
#' @keywords internal
#' @noRd
#' @return NULL, called for side effects
.conf_load_color_table <- function() {
    # load the color configuration file
    color_yml_file <- .conf_colors_file()
    config_colors <- yaml::yaml.load_file(
        input = color_yml_file,
        merge.precedence = "override"
    )
    class_schemes <- config_colors$class_schemes
    sits_env[["config"]] <- utils::modifyList(sits_env[["config"]],
                                              class_schemes,
                                              keep.null = FALSE
    )
    colors <- config_colors$colors
    color_table <- purrr::map2_dfr(colors, names(colors),
                                   function(cl, nm) {
        cc_tb <- tibble::tibble(
            name = nm,
            color = cl
        )
        return(cc_tb)
    })
    # set the color table
    .conf_set_color_table(color_table)
    return(invisible(color_table))
}
#' @title Set user color table
#' @name .conf_set_color_table
#' @description Loads a user color table
#' @keywords internal
#' @noRd
#' @return Called for side effects
.conf_set_color_table <- function(color_tb) {
    # pre condition - table contains name and hex code
    .check_chr_contains(
        x = colnames(color_tb),
        contains = .conf("sits_color_table_cols"),
        discriminator = "all_of",
        msg = "invalid colour table - missing either name or hex columns"
    )
    # pre condition - table contains no duplicates
    tbd <- dplyr::distinct(color_tb, .data[["name"]])
    .check_that(nrow(tbd) == nrow(color_tb),
        msg = "color table contains duplicate names"
    )
    sits_env$color_table <- color_tb
    return(invisible(color_tb))
}
#' @title Merge user colors with default colors
#' @name .conf_merge_colors
#' @description Combines user colors with default color table
#' @keywords internal
#' @noRd
#' @return NULL, called for side effects
.conf_merge_colors <- function(user_colors) {
    # get the current color table
    color_table <- .conf_colors()
    names_user_colors <- names(user_colors)
    col_user_colors <- unname(user_colors)
    for (i in seq_along(names_user_colors)) {
        name <- names_user_colors[[i]]
        col <- col_user_colors[[i]]
        id <- which(color_table$name == name)
        if (length(id) > 0) {
            color_table[id, "color"] <- col
        } else {
            color_table <- tibble::add_row(color_table,
                name = name,
                color = col
            )
        }
    }
    .conf_set_color_table(color_table)
    return(invisible(color_table))
}
#' @title Return the default color table
#' @name .conf_colors
#' @keywords internal
#' @noRd
#' @return default color table
#'
.conf_colors <- function() {
    return(sits_env$color_table)
}
#' @title Configure fonts to be used
#' @name .conf_set_fonts
#' @keywords internal
#' @noRd
#' @return NULL, called for side effects
#'
.conf_set_fonts <- function() {
    # verifies if sysfonts package is installed
    .check_require_packages("sysfonts")
    sysfonts::font_add_google("Open Sans", family = "opensans")
    sysfonts::font_add_google("Roboto", family = "roboto")
    sysfonts::font_add_google("Source Sans 3", family = "sourcesans")
    return(NULL)
}
#' @title Return the user configuration set in enviromental variable
#' @name .conf_user_env_var
#' @keywords internal
#' @noRd
#' @return YAML user configuration
.conf_user_env_var <- function() {
    # load the default user configuration file
    yml_file <- Sys.getenv("SITS_CONFIG_USER_FILE")
    yaml_user_config <- NULL
    # check if the file exists when env var is set
    if (nchar(yml_file) > 0) {
        .check_warn(
            .check_file(yml_file,
                msg = paste(
                    "invalid configuration file informed in",
                    "SITS_CONFIG_USER_FILE"
                )
            )
        )
        # if the YAML file exists, try to load it
        tryCatch({
            yaml_user_config <- yaml::yaml.load_file(
                    input = yml_file,
                    merge.precedence = "override"
            )},
            error = function(e) {
                warning(msg = paste(
                    "invalid configuration file informed in",
                    "SITS_CONFIG_USER_FILE"), call. = TRUE)
            }
        )
    }
    # returns the user configuration, otherwise null
    return(yaml_user_config)
}
#' @title Load the user configuration file
#' @name .conf_set_user_file
#' @param config_user_file  Configuration file provided by user
#' @keywords internal
#' @noRd
#' @return user configuration file
.conf_set_user_file <- function(config_user_file = NULL) {
    # try to find a valid user configuration file
    # check config user file is valid
    if (!purrr::is_null(config_user_file) && !is.na(config_user_file)) {
        user_config <- tryCatch(
            yaml::yaml.load_file(config_user_file, error.label = "",
                                 readLines.warn = FALSE),
            error = function(e) {
                stop("invalid user configuration file", call. = TRUE)
            }
        )
    } else {
        user_config <- .conf_user_env_var()
    }
    if (!purrr::is_null(user_config)) {
        if (!purrr::is_null(user_config$colors)) {
            user_colors <- user_config$colors
            .conf_merge_colors(user_colors)
            user_config$colors <- NULL
        }
        if (!purrr::is_null(user_config$class_schemes)) {
            class_schemes <- user_config$class_schemes
            sits_env[["config"]] <- utils::modifyList(
                sits_env[["config"]],
                class_schemes,
                keep.null = FALSE
            )
            user_config$class_schemes <- NULL
        }
        if (length(user_config) > 0) {
            user_config <- utils::modifyList(sits_env[["config"]],
                user_config,
                keep.null = FALSE
            )
            # set options defined by user (via YAML file)
            # modifying existing configuration
            .conf_set_options(
                processing_bloat = user_config[["processing_bloat"]],
                rstac_pagination_limit =
                    user_config[["rstac_pagination_limit"]],
                gdal_creation_options =
                    user_config[["gdal_creation_options"]],
                gdalcubes_chunk_size =
                    user_config[["gdalcubes_chunk_size"]],
                sources = user_config[["sources"]],
                colors = user_config[["colors"]]
            )
        }
    }
}
#' @title Check band availability
#' @name .conf_check_bands
#' @description Checks if the requested bands are available in the collection
#'
#' @keywords internal
#' @noRd
#' @param source        Data source
#' @param collection    Collection to be searched in the data source.
#' @param bands         Bands to be included.
#'
#' @return              Called for side effects.
.conf_check_bands <- function(source, collection, bands) {
    # set caller to show in errors
    .check_set_caller(".conf_check_bands")

    sits_bands <- .source_bands(
        source = source,
        collection = collection
    )
    source_bands <- .source_bands_band_name(
        source = source,
        collection = collection
    )
    .check_chr_within(
        x = bands,
        within = c(sits_bands, source_bands),
        msg = paste(
            "invalid bands.\nPlease verify",
            "the provided bands."
        )
    )
    return(invisible(bands))
}
#' @title Get names associated to a configuration key
#' @name .conf_names
#' @param key   key combination to access config information
#' @keywords internal
#' @noRd
#' @return   names associated to the chosen access key
.conf_names <- function(key) {
    res <- tryCatch(
        {
            names(sits_env$config[[key]])
        },
        error = function(e) {
            return(NULL)
        }
    )
    # post-condition
    .check_chr(res,
        allow_empty = FALSE,
        msg = paste(
            "invalid names for",
            paste0("'", paste0(key, collapse = "$"), "'"),
            "key"
        )
    )

    return(res)
}
#' @title Include a new source in the configuration
#' @name .conf_new_source
#' @description creates a new data source in the config file
#' @param s3_class   s3 class associated to the source
#' @param collections collections associated to the source
#' @param ...         other parameters associated to the new source
#' @param service     protocol associated to the source (e.g., "STAC")
#' @param url         url associated to the endpoint of the service
#' @keywords internal
#' @noRd
#' @return   list with the configuration associated to the new source
.conf_new_source <- function(s3_class,
                             collections, ...,
                             service = NULL,
                             url = NULL) {
    # set caller to show in errors
    .check_set_caller(".conf_new_source")

    # pre-condition
    .check_chr(s3_class,
        allow_empty = FALSE, len_min = 1,
        msg = "invalid 's3_class' parameter"
    )

    if (!is.null(service)) {
        .check_chr(service,
            allow_empty = FALSE, len_min = 1, len_max = 1,
            msg = "invalid 'service' parameter"
        )
    }

    if (!is.null(url)) {
        .check_chr(url,
            allow_empty = FALSE, len_min = 1, len_max = 1,
            regex = '^(http|https)://[^ "]+$',
            msg = "invalid 'url' parameter"
        )
    }

    .check_lst(collections, min_len = 1)

    names(collections) <- toupper(names(collections))

    collections <- lapply(collections, function(collection) {
        # pre-condition
        .check_lst(collection,
            min_len = 1,
            msg = "invalid 'collections' parameter"
        )

        # collection members must be lower case
        names(collection) <- tolower(names(collection))

        collection <- .check_error(
            {
                do.call(.conf_new_collection, args = collection)
            },
            msg = "invalid 'collections' parameter"
        )
        return(collection)
    })

    # extra parameters
    dots <- list(...)
    .check_lst(dots, msg = "invalid extra arguments in collection")

    return(c(list(
        s3_class = s3_class,
        service = service,
        url = url,
        collections = collections
    ), dots))
}
#' @title Include a new collection in the configuration
#' @name .conf_new_collection
#' @description creates a new collection associated to a source
#' @param bands       bands associated to the collection
#' @param ...         other relevant parameters
#' @param satellite   satellite associated to the collection
#' @param sensor      sensor associated to the collection
#' @keywords internal
#' @noRd
#' @return   list with the configuration associated to the new collection
.conf_new_collection <- function(bands, ...,
                                 satellite = NULL,
                                 sensor = NULL,
                                 metadata_search = NULL) {
    # set caller to show in errors
    .check_set_caller(".conf_new_collection")

    # check satellite
    .check_chr(satellite,
        allow_null = TRUE,
        msg = "invalid 'satellite' value"
    )

    #  check sensor
    .check_chr(sensor,
        allow_null = TRUE,
        msg = "invalid 'sensor' value"
    )

    # check metadata_search
    if (!missing(metadata_search)) {
        .check_chr_within(metadata_search,
            within = .conf("metadata_search_strategies"),
            msg = "invalid 'metadata_search' value"
        )
    }

    # bands names is upper case
    names(bands) <- toupper(names(bands))

    # separate cloud and non-cloud bands
    non_cloud_bands <- bands[!names(bands) %in% .source_cloud()]
    cloud_band <- bands[names(bands) %in% .source_cloud()]

    non_cloud_bands <- lapply(non_cloud_bands, function(band) {
        # pre-condition
        .check_lst(bands,
            min_len = 1,
            msg = "invalid 'bands' parameter"
        )

        # bands' members are lower case
        names(band) <- tolower(names(band))

        band <- .check_error(
            {
                do.call(.conf_new_band, args = band)
            },
            msg = "invalid 'bands' parameter"
        )

        return(band)
    })

    cloud_band <- lapply(cloud_band, function(cloud_band) {
        # pre-condition
        .check_lst(bands,
            min_len = 1,
            msg = "invalid 'bands' parameter"
        )

        # bands' members are lower case
        names(cloud_band) <- tolower(names(cloud_band))

        cloud_band <- .check_error(
            {
                do.call(.conf_new_cloud_band, args = cloud_band)
            },
            msg = "invalid 'bands' parameter"
        )

        return(cloud_band)
    })

    # extra parameters
    dots <- list(...)
    .check_lst(dots, msg = "invalid extra arguments in collection")

    res <- c(list(bands = c(non_cloud_bands, cloud_band)),
        "satellite" = satellite,
        "sensor" = sensor,
        "metadata_search" = metadata_search, dots
    )

    # post-condition
    .check_lst(res,
        min_len = 1,
        msg = "invalid 'collection' value"
    )

    .check_lst(res$bands,
        min_len = 1,
        msg = "invalid collection 'bands' value"
    )

    # return a new collection data
    return(res)
}
#' @title Include a new band in the configuration
#' @name .conf_new_band
#' @description creates a description associated to a new band
#' @param missing_value  missing value
#' @param minimum_value  minimum value
#' @param maximum_value  maximum_value
#' @param scale_factor   scale_factor associated with the data
#' @param offset_value   offset_value for the band
#' @param band_name      name of the band
#' @param resolution     spatial resolution (in meters)
#' @param ...            other relevant parameters
#' @keywords internal
#' @noRd
#' @return   list with the configuration associated to the new band
.conf_new_band <- function(missing_value,
                           minimum_value,
                           maximum_value,
                           scale_factor,
                           offset_value,
                           band_name,
                           resolution, ...) {
    # pre-condition
    .check_num(
        x = missing_value,
        len_min = 1,
        len_max = 1,
        msg = "invalid 'missing_value' parameter"
    )

    .check_num(
        x = minimum_value,
        len_min = 1,
        len_max = 1,
        msg = "invalid 'minimum_value' parameter"
    )

    .check_num(
        x = maximum_value,
        len_min = 1,
        len_max = 1,
        msg = "invalid 'maximum_value' parameter"
    )

    .check_num(
        x = scale_factor,
        len_min = 1,
        len_max = 1,
        exclusive_min = 0,
        msg = "invalid 'scale_factor' parameter"
    )

    .check_num(
        x = offset_value,
        len_min = 1,
        len_max = 1,
        msg = "invalid 'offset_value' parameter"
    )


    .check_num(
        x = resolution,
        exclusive_min = 0,
        len_min = 1,
        len_max = 1,
        allow_null = TRUE,
        msg = "invalid 'resolution' parameter"
    )

    .check_chr(
        x = band_name,
        allow_empty = FALSE,
        len_min = 1,
        len_max = 1,
        msg = "invalid 'band_name' value"
    )

    # extra parameters
    dots <- list(...)
    .check_lst(dots, "invalid extra arguments in band")

    res <- c(list(
        missing_value = missing_value,
        minimum_value = minimum_value,
        maximum_value = maximum_value,
        scale_factor = scale_factor,
        offset_value = offset_value,
        band_name = band_name,
        resolution = resolution
    ), dots)

    # post-condition
    .check_lst(res,
        min_len = 7,
        msg = "invalid 'band' value"
    )

    # return a band object
    return(res)
}
#' @title Include a new cloud band in the configuration
#' @name .conf_new_cloud_band
#' @description creates a description associated to a new cloud band
#' @param bit_mask       bit mask to describe clouds (if applicable)
#' @param value          values of the cloud band
#' @param interp_values  pixel values that need to be replaced by interpolation
#' @param resolution     spatial resolution (in meters)
#' @param band_name      name of the band
#' @param ...            other relevant parameters
#' @keywords internal
#' @noRd
#' @return   list with the configuration associated to the new band
.conf_new_cloud_band <- function(bit_mask,
                                 values,
                                 interp_values,
                                 resolution,
                                 band_name, ...) {
    # set caller to show in errors
    .check_set_caller(".conf_new_cloud_band")

    # pre-condition
    .check_lgl(bit_mask,
        len_min = 1, len_max = 1,
        msg = "invalid 'bit_mask' parameter"
    )

    .check_lst(values,
        fn_check = .check_chr,
        len_min = 1, len_max = 1,
        msg = "invalid cloud 'values' parameter"
    )

    .check_num(interp_values,
        len_min = 1, is_integer = TRUE,
        msg = "invalid 'interp_values' parameter"
    )

    .check_chr(band_name,
        allow_empty = FALSE, len_min = 1, len_max = 1,
        msg = "invalid 'band_name' value"
    )

    # extra parameters
    dots <- list(...)
    .check_lst(dots, "invalid extra arguments in cloud band")

    res <- c(list(
        bit_mask = bit_mask,
        values = values,
        interp_values = interp_values,
        resolution = resolution,
        band_name = band_name
    ), dots)

    # post-condition
    .check_lst(res,
        min_len = 5,
        msg = "invalid 'band' value"
    )

    # return a cloud band object
    return(res)
}
#' @title Retrieve the rstac pagination limit
#' @name .conf_rstac_limit
#' @keywords internal
#' @noRd
#' @return pagination limit to rstac output
.conf_rstac_limit <- function() {
    res <- .conf(key = c("rstac_pagination_limit"))

    # post-condition
    .check_num(res,
        min = 1, len_min = 1, len_max = 1,
        msg = "invalid 'rstac_pagination_limit' in config file"
    )

    return(res)
}
#' @title Retrieve the raster package to be used
#' @name .conf_raster_pkg
#' @keywords internal
#' @noRd
#' @return the raster package used to process raster data
#'
.conf_raster_pkg <- function() {
    res <- .conf(key = c("raster_api_package"))

    # post-condition
    .check_chr(res,
        len_min = 1, len_max = 1,
        msg = "invalid 'raster_api_package' in config file"
    )

    .check_chr_within(res,
        within = .raster_supported_packages(),
        discriminator = "one_of",
        msg = "invalid 'raster_api_package' in config file"
    )

    return(res)
}

#' @title Basic access config functions
#' @noRd
#'
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description
#' These are basic functions to access config options.
#'
#' @param ... Set of \code{character} values representing a key to access
#'   some hierarchical config entry.
#'
#' @examples
#' if (sits_run_examples()) {
#'     .conf_exists("run_tests") # TRUE
#'     .conf("run_tests")
#'     .conf_exists("not_existing_entry") # FALSE
#' }
NULL
#' @title Check if a key exists in config
#' @noRd
#' @param throw_error  Should an error be thrown if test fails?
#' @returns  A logical value or an error if key not found and
#'   `throw_error` is `TRUE`.
.conf_exists <- function(..., throw_error = FALSE) {
    key <- c(...)
    exists <- !is.null(.try(sits_env[["config"]][[key]], .default = NULL))
    if (!exists && throw_error) {
        stop("key '", paste(key, collapse = "->"), "' not found in config")
    }
    # Return test
    exists
}
#' @title Get a config value based on a key
#' @noRd
#' @returns A value in config or an error if key does not exists.
.conf <- function(...) {
    key <- c(...)
    # Check for key existence and throws an error if it not exists
    .conf_exists(key, throw_error = TRUE)
    sits_env[["config"]][[c(key)]]
}
#' @title Config functions eo_cube
#' @noRd
#'
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description
#' These are syntactic sugar functions to easily access config options for
#' bands of `eo_cube` cubes which is a S3 class representation
#' for an Earth Observation cube. It is the primary data used to obtain a
#' classification map.
#
#' The config entries of a `eo_cube` are located in
#' `sources -> <SOURCE> -> collections -> <COLLECTION>` key.
#' Values for source, collection, and band are uppercase.
#'
#' @examples
#' if (sits_run_examples()) {
#'     # tests if 'BDC -> MOD13Q1-6 -> NDVI' key exists in config
#'     .conf_eo_band_exists(
#'         source = "BDC",
#'         collection = "MOD13Q1-6",
#'         band = "NDVI"
#'     )
#'     # get configuration for band NDVI of 'BDC -> MOD13Q1-6' collection
#'     x <- .conf_eo_band(
#'         source = "BDC",
#'         collection = "MOD13Q1-6",
#'         band = "NDVI"
#'     )
#' }
NULL
#' @title Check if a band entry exists in config
#' @noRd
#' @param source  Data source.
#' @param collection  Collection in the data source.
#' @param band  Band name.
#' @returns  A logical value or an error if `source` or `collections`
#'   does not exists.
.conf_eo_band_exists <- function(source, collection, band) {
    # source, collection, and band are uppercase
    source <- toupper(source)
    collection <- toupper(collection)
    band <- .band_eo(band)
    # Check for source and collection and throws an error if it not exists
    .conf_exists(
        "sources", source, "collections", collection,
        throw_error = TRUE
    )
    # Test for band and return
    .conf_exists("sources", source, "collections", collection, "bands", band)
}
#' @title Get a config value for a band
#' @noRd
#' @param source  Data source.
#' @param collection  Collection in the data source.
#' @param band  Band name.
#' @details
#' If the band is not found, a default value will be returned from config.
#' If neither source nor collection entries are found in configuration file,
#' an error is thrown.
#' @returns  A value in config.
.conf_eo_band <- function(source, collection, band) {
    # Format band name
    band <- .band_eo(band)
    # Return a default value if band does not exists in config
    if (!.conf_eo_band_exists(source, collection, band)) {
        return(.conf("default_values", "eo_cube"))
    }
    # Get band config value and return it
    .conf("sources", source, "collections", collection, "bands", band)
}
#' @title Config functions for derived_cube
#' @noRd
#'
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description
#' These are syntactic sugar functions to easily access config options for
#' bands of `derived_cube` cubes. `derived_cube`s are a S3 class
#' representation of a cube generated by the classification workflow starting
#' from an Earth Observation data cube.
#'
#' There are several classes of `derived_cube`:
#' * probs_cube: multilayer probability cube produced by a
#'   classification with the probabilities attributed to each class by a
#'   model. The possible band names are 'probs', 'bayes', and
#'   'bilat', acronyms for 'probability', 'Bayesian smoothing', and
#'   'Bilateral smoothing'.
#' * class_cube: labeled cube (classified map) produced by choosing
#'   a label for each pixel. Its unique band name is 'class'.
#' * uncertainty_cube: a cube produced to measure the uncertainty of
#'   a classification for each pixel. The possible band names are
#'   'least', 'entropy', and 'margin', acronyms for the method used to
#'   produce the cube.
#'
#' Values for `derived_class` and band are lowercase. This was
#' done to avoid conflicts with `eo_cube` band naming (uppercase).
#' The config entries of a `derived_cube` are located in
#' `derived_cube -> <derived_class>` key.
#'
#' @examples
#' if (sits_run_examples()) {
#'     # get S3 class value that a derived_cube of class 'probs' must have
#'     .conf_derived_s3class("probs")
#' }
#'
NULL
#' @title Get the S3 class values of a `derived_cube`
#' @noRd
#' @param derived_class  A `derived_cube` class name.
#' @return A S3 class.
.conf_derived_s3class <- function(derived_class) {
    # derived_class is lowercase
    derived_class <- tolower(derived_class)
    .conf("derived_cube", derived_class, "s3_class")
}
#' @title Get a band configuration of a `derived_cube`
#' @noRd
#' @param derived_class  A `derived_cube` class name.
#' @param band  Band name of `derived_cube`.
#' @return  A band configuration.
.conf_derived_band <- function(derived_class, band) {
    # Format band
    band <- .band_derived(band)
    # Derived_class is lowercase
    derived_class <- tolower(derived_class)
    .conf("derived_cube", derived_class, "bands", band)
}
#' @title Band configuration accessors
#' @noRd
#'
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description
#' These functions are read-only accessors of band_conf objects. A
#' band_conf is an entry of band definition in config. It can be associated
#' to an eo_cube or derived_cube
#'
NULL
#' @title Get the data type from a band configuration
#' @param conf  A band definition value from config.
#' @noRd
#' @return  Data type associated to the configuration
.data_type <- function(conf) {
    .as_chr(conf[["data_type"]][[1]])
}
#' @title Get the missing value from a band configuration
#' @noRd
#' @param conf  A band definition value from config.
#' @return  Missing value associated to the band
.miss_value <- function(conf) {
    .as_dbl(conf[["missing_value"]][[1]])
}
#' @title Get the minimum value from a band configuration
#' @noRd
#' @param conf  A band definition value from config.
#' @return  Minimum value associated to the band
.min_value <- function(conf) {
    .as_dbl(conf[["minimum_value"]][[1]])
}
#' @title Get the maximum value from a band configuration
#' @noRd
#' @param conf  A band definition value from config.
#' @return  Maximum value associated to the band
.max_value <- function(conf) {
    .as_dbl(conf[["maximum_value"]][[1]])
}
#' @title Get the scale factor from a band configuration
#' @noRd
#' @param conf  A band definition value from config.
#' @return  Scale factor associated to the band
.scale <- function(conf) {
    .as_dbl(conf[["scale_factor"]][[1]])
}
#' @title Get the offset value from a band configuration
#' @noRd
#' @param conf  A band definition value from config.
#' @return  Offset value associated to the band
.offset <- function(conf) {
    .as_dbl(conf[["offset_value"]][[1]])
}
#' @title Get the cloud interpolation values from a band configuration
#' @noRd
#' @param conf  A band definition value from config.
#' @return  Cloud interpolation values associated to the band
.cloud_interp_values <- function(conf) {
    .as_int(conf[["interp_values"]])
}
#' @title Get the bit mask flag from a band configuration
#' @noRd
#' @param conf  A band definition value from config.
#' @return  Cloud bit mask values associated to the band.
.cloud_bit_mask <- function(conf) {
    .as_int(conf[["bit_mask"]][[1]])
}
#' @title Get the default parse info for local files  flag
#' @noRd
#' @param parse_info  Parse information set by user
#' @param results_cube Is this a results cube?
#' @return  Valid parse_info information
.conf_parse_info <- function(parse_info, results_cube) {
    # is parse info NULL? use the default
    if (purrr::is_null(parse_info)) {
        if (results_cube) {
            parse_info <- .conf("results_parse_info_def")
        } else {
            parse_info <- .conf("local_parse_info_def")
        }
    }

    # precondition - does the parse info have band and date?
    if (results_cube) {
        .check_chr_contains(
            parse_info,
            contains = .conf("results_parse_info_col"),
            msg = paste(
                "parse_info must include tile, start_date, end_date,",
                "and band."
            )
        )
    } else {
        .check_chr_contains(
            parse_info,
            contains = .conf("local_parse_info_col"),
            msg = "parse_info must include tile, date, and band."
        )
    }
    return(parse_info)
}
