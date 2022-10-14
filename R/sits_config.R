#' @title Configure parameters for sits package
#' @name sits_configuration
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description These functions load and show sits configurations.
#'
#' The `sits` package uses a configuration file
#' that contains information on parameters required by different functions.
#' This includes information about the image collections handled by `sits`.
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
#' @param run_tests              Should tests be run?
#' @param run_examples           Should examples be run?
#' @param processing_bloat       Estimated growth size of R memory relative
#'                               to block size.
#' @param rstac_pagination_limit Limit of number of items returned by STAC.
#' @param raster_api_package     Supported raster handling package.
#' @param gdal_creation_options  GDAL creation options for GeoTiff.
#' @param gdalcubes_chunk_size   Chunk size to be used by gdalcubes
#' @param leaflet_max_megabytes  Max image size of an image for leaflet (in MB)
#' @param leaflet_comp_factor    Compression factor for leaflet RGB display.
#' @param reset                  Should current configuration options be cleaned
#'                               before loading config files? Default is
#'                               \code{FALSE}.
#' @param source                 Data source to be shown in detail.
#' @param collection             Collection key entry to be shown in detail.
#' @param colors                 Show colors?
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
#' @examples
#' current_config <- sits_config()
#' @export
sits_config <- function(run_tests = NULL,
                        run_examples = NULL,
                        processing_bloat = NULL,
                        rstac_pagination_limit = NULL,
                        raster_api_package = NULL,
                        gdal_creation_options = NULL,
                        gdalcubes_chunk_size = NULL,
                        leaflet_max_megabytes = NULL,
                        leaflet_comp_factor = NULL,
                        reset = FALSE) {

    # clear current configuration
    if (reset && !is.null(sits_env$config)) {
        sits_env$config <- list()
    }

    # load the internal configuration file
    config_internals_file <- .conf_internals_file()

    # read the configuration parameters
    config_internals <- yaml::yaml.load_file(
        input = config_internals_file,
        merge.precedence = "override"
    )

    # set options defined in sits config
    do.call(.conf_set_options, args = config_internals)

    # get and check the default configuration file path
    yml_file <- .conf_file()

    # read the configuration parameters
    config <- yaml::yaml.load_file(
        input = yml_file,
        merge.precedence = "override"
    )

    # set options defined in sits config
    do.call(.conf_set_options, args = config)
    message(paste0("Using configuration file: ", yml_file))

    # load the color configuration file
    color_yml_file <- .conf_colors_file()
    message(paste("Color configurations found in", color_yml_file))
    config_colors <- yaml::yaml.load_file(
        input = color_yml_file,
        merge.precedence = "override"
    )
    .conf_set_options(colors = config_colors[["colors"]])

    # try to find a valid user configuration file
    user_yml_file <- .conf_user_file()

    if (file.exists(user_yml_file)) {
        message(paste("Additional configurations found in", user_yml_file))
        config <- yaml::yaml.load_file(
            input = user_yml_file,
            merge.precedence = "override"
        )
        config <- utils::modifyList(sits_env[["config"]],
            config,
            keep.null = FALSE
        )

        # set options defined by user (via YAML file)
        # modifying existing configuration
        .conf_set_options(
            run_tests = config[["run_tests"]],
            run_examples = config[["run_examples"]],
            processing_bloat = config[["processing_bloat"]],
            rstac_pagination_limit = config[["rstac_pagination_limit"]],
            raster_api_package = config[["raster_api_package"]],
            gdal_creation_options = config[["gdal_creation_options"]],
            gdalcubes_chunk_size = config[["gdalcubes_chunk_size"]],
            leaflet_max_megabytes = config[["leaflet_max_megabytes"]],
            leaflet_comp_factor = config[["leaflet_comp_factor"]],
            sources = config[["sources"]],
            colors = config[["colors"]]
        )
    } else {
        message(paste(
            "To provide additional configurations, create an",
            "YAML file and inform its path to environment variable",
            "'SITS_CONFIG_USER_FILE'."
        ))
    }

    # set options defined by user (via parameters)
    # modifying existing configuration
    .conf_set_options(
        run_tests = run_tests,
        run_examples = run_examples,
        processing_bloat = processing_bloat,
        rstac_pagination_limit = rstac_pagination_limit,
        raster_api_package = raster_api_package,
        gdal_creation_options = gdal_creation_options,
        gdalcubes_chunk_size = gdalcubes_chunk_size,
        leaflet_max_megabytes = leaflet_max_megabytes,
        leaflet_comp_factor = leaflet_comp_factor
    )

    message(paste0("Using raster package: ", .conf_raster_pkg()))

    return(invisible(sits_env$config))
}

#' @rdname sits_configuration
#'
#' @return A \code{list} containing the respective
#' configuration printed in the console.
#' @examples
#' sits_config_show()
#' @export
sits_config_show <- function(source = NULL,
                             collection = NULL,
                             colors = FALSE) {
    config <- sits_env$config

    if (!is.null(source)) {
        .check_chr(source,
            allow_empty = FALSE,
            len_min = 1,
            len_max = 1
        )

        .check_chr_within(source,
            within = .sources(),
            discriminator = "one_of"
        )

        config <- config[[c("sources", source)]]

        if (!is.null(collection)) {
            .check_chr(collection,
                allow_empty = FALSE,
                len_min = 1,
                len_max = 1
            )

            .check_chr_within(collection,
                within = .source_collections(source = source),
                discriminator = "one_of"
            )

            config <- config[[c("collections", collection)]]
        } else {
            config <- lapply(config, function(x) {
                if (is.atomic(x)) {
                    return(x)
                }
                list(names(x))
            })
        }
    } else if (colors) {
        config <- config[["colors"]]
    } else {
        config <- lapply(config, function(x) {
            if (is.atomic(x)) {
                return(x)
            }
            list(names(x))
        })
    }

    config_txt <- yaml::as.yaml(config,
        indent = 4,
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
            }
        )
    )
    cat(config_txt, sep = "\n")
    return(invisible(config))
}

#' @rdname sits_configuration
#'
#' @return Prints collections available in each cloud service supported by sits.
#'
#' @export
sits_list_collections <- function(source = NULL) {

    # get sources available
    sources <- .sources()

    # if the user has required a source
    # check that it is valid
    if (!purrr::is_null(source)) {
        # check if source exists
        .check_chr_within(
            x = source,
            within = sources,
            msg = "invalid 'source' value"
        )
        sources <- source
    }

    purrr::map(sources, function(s) {
        cat(paste0(s, ":\n"))
        collections <- .source_collections(source = s)
        purrr::map(collections, function(c) {
            cat(paste0("- ", c))
            cat(paste0(
                " (", .source_collection_satellite(s, c),
                "/", .source_collection_sensor(s, c), ")\n"
            ))
            cat("- bands: ")
            cat(.source_bands(s, c))
            cat("\n")
            if (.source_collection_open_data(source = s, collection = c)) {
                cat("- opendata collection ")
                if (.source_collection_open_data_token(
                    source = s,
                    collection = c
                )) {
                    cat("(requires access token)")
                }
            } else {
                cat("- not opendata collection")
            }
            cat("\n")
            cat("\n")
        })
    })
    return(invisible(NULL))
}

.conf_set_options <- function(run_tests = NULL,
                                run_examples = NULL,
                                processing_bloat = NULL,
                                rstac_pagination_limit = NULL,
                                raster_api_package = NULL,
                                gdal_creation_options = NULL,
                                gdalcubes_chunk_size = NULL,
                                leaflet_max_megabytes = NULL,
                                leaflet_comp_factor = NULL,
                                sources = NULL,
                                colors = NULL, ...) {
    # set caller to show in errors
    .check_set_caller(".conf_set_options")

    # initialize config
    if (!exists("config", envir = sits_env)) {
        sits_env$config <- list()
    }

    # run tests?
    if (!is.null(run_tests)) {
        .check_lgl(run_tests,
                   len_min = 1, len_max = 1,
                   msg = "invalid 'run_tests' parameter"
        )
        sits_env$config[["run_tests"]] <- run_tests
    }

    # run examples?
    if (!is.null(run_examples)) {
        .check_lgl(run_examples,
                   len_min = 1, len_max = 1,
                   msg = "invalid 'run_examples' parameter"
        )
        sits_env$config[["run_examples"]] <- run_examples
    }

    # process processing_bloat
    if (!is.null(processing_bloat)) {
        .check_num(processing_bloat,
            min = 1, len_min = 1, len_max = 1,
            msg = "invalid 'processing_bloat' parameter"
        )
        sits_env$config[["processing_bloat"]] <- processing_bloat
    }

    # process rstac_pagination_limit
    if (!is.null(rstac_pagination_limit)) {
        .check_num(rstac_pagination_limit,
            min = 1, len_min = 1, len_max = 1,
            msg = "invalid 'rstac_pagination_limit' parameter"
        )
        sits_env$config[["rstac_pagination_limit"]] <- rstac_pagination_limit
    }

    # process raster_api_package
    if (!is.null(raster_api_package)) {
        .check_chr(raster_api_package,
            len_min = 1, len_max = 1,
            msg = "invalid 'raster_api_package' parameter"
        )
        .check_chr_within(raster_api_package,
            within = .raster_supported_packages(),
            discriminator = "one_of",
            msg = "invalid 'raster_api_package' parameter"
        )
        sits_env$config[["raster_api_package"]] <- raster_api_package
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
    if (!is.null(leaflet_max_megabytes)) {
        .check_num(leaflet_max_megabytes,
            min = 16,
            max = 128,
            is_named = FALSE,
            msg = "invalid leaflet max megabytes"
        )
        sits_env$config[["leaflet_max_megabytes"]] <- leaflet_max_megabytes
    }
    if (!is.null(leaflet_comp_factor)) {
        .check_num(leaflet_comp_factor,
            min = 0.20,
            max = 1.00,
            is_named = FALSE,
            msg = "invalid leaflet_comp_factor"
        )
        sits_env$config[["leaflet_comp_factor"]] <- leaflet_comp_factor
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

#' @title Return the user configuration file
#' @name .conf_user_file
#' @keywords internal
#' @noRd
#' @return user configuration file
.conf_user_file <- function() {

    # load the default configuration file
    yml_file <- Sys.getenv("SITS_CONFIG_USER_FILE")
    # check if the file exists
    if (nchar(yml_file) > 0) {
        .check_warn(
            .check_file(yml_file,
                msg = paste(
                    "invalid configuration file informed in",
                    "SITS_CONFIG_USER_FILE"
                )
            )
        )
    }

    return(yml_file)
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

    # remove bands with equal names, like NDVI, EVI...
    source_bands <- source_bands[!source_bands %in% sits_bands]

    return(invisible(NULL))
}
#' @title Check metatype associated to the data
#' @name .conf_data_meta_type
#' @keywords internal
#' @noRd
#' @description associates a valid SITS class to the data
#'
#' @param  data    Time series or data cube.
#'
#' @return         The meta data type associated to a sits object.
.conf_data_meta_type <- function(data) {

    # set caller to show in errors
    .check_set_caller(".conf_data_meta_type")

    # if the data is one of the classes recognized by sits
    if (inherits(data, .conf("sits_s3_classes"))) {
        return(data)
    } else if (inherits(data, "tbl_df")) {
        # is this a data cube or a sits tibble?
        if (all(.conf("sits_cube_cols")
        %in% colnames(data))) {
            class(data) <- c("raster_cube", class(data))

            return(data)
        } else if (all(.conf("sits_tibble_cols") %in% colnames(data))) {
            class(data) <- c("sits", class(data))
            return(data)
        }
    }

    .check_that(FALSE,
        local_msg = "Data not recognized as a sits object",
        msg = "invalid 'data' parameter"
    )
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
#---- Config API ----

#' Basic access config functions
#'
#' These are basic functions to access config options.
#'
#' @param ... Set of \code{character} values representing a key to access
#'   some hierarchical config entry.
#' @param throw_error Should an error be thrown if test fails?
#'
#' @examples
#' if (sits_run_examples()) {
#' .conf_exists("run_tests") # TRUE
#' .conf("run_tests")
#' .conf_exists("not_existing_entry") # FALSE
#' }
#'
#' @returns Configuration value.
#'
#' @family config functions
#' @keywords internal
#' @name config_api
NULL

#' @describeIn config_api Tests if a key provided as \code{character} values
#'   in \code{...} parameter exists in the config. If \code{throws_error} is
#'   \code{TRUE} and the test failed, an error is raised.
.conf_exists <- function(..., throw_error = FALSE) {
    key <- c(...)
    exists <- !is.null(.try(sits_env[["config"]][[key]], .default = NULL))
    if (!exists && throw_error) {
        stop("key '", paste(key, collapse = "->"), "' not found in config")
    }
    # Return test
    exists
}

#' @describeIn config_api Get a config value located in a key provided as
#'   \code{character} values in \code{...} parameter. If a key does not
#'   exists, throws an error. Use \code{.conf_exists()} to test for a key
#'   existence.
.conf <- function(...) {
    key <- c(...)
    # Check for key existence and throws an error if it not exists
    .conf_exists(key, throw_error = TRUE)
    sits_env[["config"]][[c(key)]]
}

#' Config functions for \code{eo_cube}
#'
#' These are syntactic sugar functions to easily access config options for
#' bands of \code{eo_cube} cubes. \code{eo_cubes} are a S3 class representation
#' for an Earth Observation cube. It is the primary data used to obtain a
#' classification map.
#'
#' The config entries of a \code{eo_cube} are located in
#' \code{sources -> <SOURCE> -> collections -> <COLLECTION>} key.
#' Values for \code{source}, \code{collection}, and \code{band} are uppercase.
#'
#' @param source Source name.
#' @param collection Collection name.
#' @param band Band name.
#'
#' @examples
#' if (sits_run_examples()) {
#' # tests if 'BDC -> MOD13Q1-6 -> NDVI' key exists in config
#' .conf_eo_band_exists(
#'   source = "BDC",
#'   collection = "MOD13Q1-6",
#'   band = "NDVI"
#' )
#' # get configuration for band NDVI of 'BDC -> MOD13Q1-6' collection
#' x <- .conf_eo_band(
#'   source = "BDC",
#'   collection = "MOD13Q1-6",
#'   band = "NDVI"
#' )
#' }
#'
#' @returns Configuration value.
#'
#' @seealso Band accessors: \link{band_accessors}
#' @family config functions
#' @keywords internal
#' @name eo_cube_config
NULL

#' @describeIn eo_cube_config Tests if a \code{band} entry exists in config
#'   for some \code{source} and \code{collection}. If neither \code{source}
#'   nor \code{collection} entry are found in config, an error is thrown.
#'   Use \code{.conf_exists()} to test for \code{source} and \code{collection}
#'   existence.
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

#' @describeIn eo_cube_config Get a config value of for a \code{band} located
#'   in a \code{source} and \code{collection}. If the \code{band} is not
#'   found, a default value will be returned from config. If neither
#'   \code{source} nor \code{collection} entry are found in config, an
#'   error is thrown. Use \code{.conf_exists()} to test for \code{source} and
#'   \code{collection} existence.
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

#' Config functions for \code{derived_cube}
#'
#' These are syntactic sugar functions to easily access config options for
#' bands of \code{derived_cube} cubes. \code{derived_cubes} are a S3 class
#' representation of a cube generated by the classification workflow starting
#' from an Earth Observation data cube.
#'
#' There are several classes of \code{derived_cube}:
#' \itemize{
#' \item \code{probs_cube}: multilayer probability cube produced by a
#'   classification with the probabilities attributed to each class by a
#'   model. The possible band names are \code{'probs'}, \code{'bayes'}, and
#'   \code{'bilat'}, acronyms for 'probability', 'Bayesian smoothing', and
#'   'Bilateral smoothing'.
#' \item \code{class_cube}: labeled cube (classified map) produced by choosing
#'   a label for each pixel. Its unique band name is \code{'class'}.
#' \item \code{uncertainty_cube}: a cube produced to measure the uncertainty of
#'   a classification for each pixel. The possible band names are
#'   \code{'least'}, \code{'entropy'}, and \code{'margin'}, acronyms for
#'   the method used to produce the cube.
#'   \code{'bilat'}, acronyms for 'probability', 'Bayesian smoothing', and
#'   'Bilateral smoothing'.
#' }
#'
#' Values for \code{derived_class} and \code{band} are lowercase. This was
#' done to avoid conflicts with \code{eo_cube} band naming (uppercase).
#' The config entries of a \code{derived_cube} are located in
#' \code{derived_cube -> <derived_class>} key.
#'
#' @param derived_class Class name of the \code{derived_cube}.
#' @param band Band name.
#'
#' @examples
#' if (sits_run_examples()) {
#' # get S3 class value that a derived_cube of class 'probs' must have
#' .conf_derived_s3class("probs")
#' }
#'
#' @returns Configuration value.
#'
#' @seealso Band accessors: \link{band_accessors}
#' @family config functions
#' @keywords internal
#' @name derived_cube_config
NULL

#' @describeIn derived_cube_config Get the S3 class values to instantiate a
#'   new \code{derived_cube}.
.conf_derived_s3class <- function(derived_class) {
    # derived_class is lowercase
    derived_class <- tolower(derived_class)
    .conf("derived_cube", derived_class, "s3_class")
}

#' @describeIn derived_cube_config Get the S3 class values to instantiate a
#'   new \code{derived_cube}.
.conf_derived_band <- function(derived_class, band) {
    # Format band
    band <- .band_derived(band)
    # Derived_class is lowercase
    derived_class <- tolower(derived_class)
    .conf("derived_cube", derived_class, "bands", band)
}

