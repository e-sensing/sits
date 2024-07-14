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
    if (!exists("config", envir = sits_env))
        sits_env[["config"]] <- list()
    # process processing_bloat
    if (!is.null(processing_bloat)) {
        .check_int_parameter(processing_bloat,
            min = 1, len_min = 1, len_max = 1, max = 10
        )
        sits_env[["config"]][["processing_bloat"]] <- processing_bloat
    }
    # process rstac_pagination_limit
    if (!is.null(rstac_pagination_limit)) {
        .check_int_parameter(rstac_pagination_limit,
            min = 1, len_min = 1, len_max = 1, max = 500
        )
        sits_env[["config"]][["rstac_pagination_limit"]] <- rstac_pagination_limit
    }
    # process gdal_creation_options
    if (!is.null(gdal_creation_options)) {
        .check_chr(gdal_creation_options,
            allow_empty = FALSE,
            regex = "^.+=.+$",
            msg = .conf("messages", ".conf_set_options_gdal_creation")
        )
        sits_env$config[["gdal_creation_options"]] <- gdal_creation_options
    }
    # process gdalcubes_chunk_size
    if (!is.null(gdalcubes_chunk_size)) {
        .check_num_parameter(gdalcubes_chunk_size,
            len_min = 3,
            len_max = 3,
            is_named = FALSE
        )
        sits_env[["config"]][["gdalcubes_chunk_size"]] <- gdalcubes_chunk_size
    }
    # process sources
    if (!is.null(sources)) {
        .check_lst_parameter(sources, len_min = 1)
        # source names are uppercase
        names(sources) <- toupper(names(sources))
        # check each source
        sources <- lapply(sources, function(source) {
            # pre-condition
            .check_lst_parameter(source, len_min = 2)

            # check that source contains essential parameters
            .check_chr_contains(names(source),
                contains = c("s3_class", "collections")
            )
            names(source) <- tolower(names(source))
            # check source
            source <- .check_error(
                {
                    do.call(.conf_new_source, args = source)
                },
                msg = .conf("messages", ".conf_set_options_source")
            )
            return(source)
        })

        # initialize sources
        if (is.null(sits_env[["config"]][["sources"]])) {
            sits_env[["config"]][["sources"]] <- sources
        }

        sits_env[["config"]][["sources"]] <- utils::modifyList(
            sits_env[["config"]][["sources"]],
            sources,
            keep.null = FALSE
        )
    }
    # check and initialize palettes
    if (.has(colors)) {
        # initialize colors
        if (is.null(sits_env[["config"]][["colors"]])) {
            sits_env[["config"]][["colors"]] <- colors
        }
        # add colors
        sits_env[["config"]][["colors"]] <- utils::modifyList(
            sits_env[["config"]][["colors"]],
            colors,
            keep.null = FALSE
        )
    }
    # process extra parameters
    dots <- list(...)
    .check_lst(dots)

    if (length(dots) > 0) {
        sits_env[["config"]] <- utils::modifyList(
            sits_env[["config"]],
            dots,
            keep.null = FALSE
        )
    }
    return(invisible(sits_env[["config"]]))
}
#' @title Return the default configuration file
#' @name .conf_file
#' @keywords internal
#' @noRd
#' @return default configuration file
#'
.conf_file <- function() {
    .check_set_caller(".conf_file")
    # load the default configuration file
    yml_file <- system.file("extdata", "config.yml", package = "sits")

    # check that the file name is valid
    .check_file(yml_file)

    return(yml_file)
}
#' @title Return the internal configuration file (only for developers)
#' @name .conf_internals_file
#' @keywords internal
#' @noRd
#' @return default internal configuration file
.conf_internals_file <- function() {
    .check_set_caller(".conf_internals_file")
    # load the default configuration file
    yml_file <- system.file("extdata", "config_internals.yml", package = "sits")
    # check that the file name is valid
    .check_that(file.exists(yml_file))
    return(yml_file)
}
#' @title Return the user-relevant configuration file
#' @name .config_file
#' @keywords internal
#' @noRd
#' @return default user-relevant configuration file
.config_file <- function() {
    .check_set_caller(".config_file")
    # load the default configuration file
    yml_file <- system.file("extdata", "config.yml", package = "sits")
    # check that the file name is valid
    .check_that(file.exists(yml_file))
    return(yml_file)
}
#' @title Return the message configuration files (only for developers)
#' @name .conf_sources_files
#' @keywords internal
#' @noRd
#' @return internal sources configuration
.conf_sources_files <- function() {
    .check_set_caller(".conf_sources_files")
    # list the source files configurations
    package_files <- system.file("extdata", "sources", package = "sits")
    yml_files <- list.files(
        path = package_files,
        pattern = "config_source_*",
        full.names = TRUE
    )
    # check that the file name is valid
    purrr::map(yml_files, .check_file)
    return(yml_files)
}
#' @name .conf_load_sources
#' @description Loads sources configurations
#' @keywords internal
#' @noRd
#' @return NULL, called for side effects
.conf_load_sources <- function() {
    # get file paths
    source_yml_files <- .conf_sources_files()
    # load files
    source_configs <- purrr::map(source_yml_files, function(file) {
        yaml::yaml.load_file(
            input = file,
            merge.precedence = "override"
        )
    })
    # prepare sources object
    source_obj <- purrr::map(source_configs, "sources")
    source_obj <- purrr::flatten(source_obj)
    # prepare extras objects (e.g., token, url config)
    extras_obj <- purrr::map(source_configs, function(source_config) {
        source_config[["sources"]] <- NULL
        source_config
    })
    extras_obj <- purrr::flatten(extras_obj)
    # merge objects
    config_obj <- utils::modifyList(extras_obj, list(
        sources = source_obj
    ))
    # set configurations
    do.call(.conf_set_options, args = config_obj)
    # done
    return(invisible(NULL))
}
#' @title Return the message configuration file (only for developers)
#' @name .conf_messages_file
#' @keywords internal
#' @noRd
#' @return default internal configuration file
.conf_messages_file <- function() {
    .check_set_caller(".conf_messages_file")
    # load the default configuration file
    yml_file <- system.file("extdata", "config_messages.yml", package = "sits")
    # check that the file name is valid
    .check_file(yml_file)
    return(yml_file)
}
#' @name .conf_load_messages
#' @description Loads the error messages and warnings
#' @keywords internal
#' @noRd
#' @return NULL, called for side effects
.conf_load_messages <- function() {
    # load the color configuration file
    msgs_yml_file <- .conf_messages_file()
    config_msgs <- yaml::yaml.load_file(
        input = msgs_yml_file,
        merge.precedence = "override"
    )
    # set the messages
    sits_env[["config"]][["messages"]] <- config_msgs
    return(invisible(NULL))
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
#' @title Loads default color table and legends
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
    # set the legends
    sits_env[["legends"]] <- config_colors$legends
    # build the color table
    colors <- config_colors[["colors"]]
    color_table <- purrr::map2_dfr(colors, names(colors),
                                   function(cl, nm) {
        cc_tb <- tibble::tibble(
            name = nm,
            color = cl
        )
        return(cc_tb)
    })

    # set the color table
    sits_env[["color_table"]] <- color_table
    return(invisible(color_table))
}
#' @title Add user color table
#' @name .conf_add_color_table
#' @description Loads a user color table
#' and merges with default color table
#' @param color_tb user color table
#' @keywords internal
#' @noRd
#' @return new color table (invisible)
.conf_add_color_table <- function(color_tb) {
    .check_set_caller(".conf_add_color_table")
    # pre condition - table contains name and hex code
    .check_chr_contains(
        x = colnames(color_tb),
        contains = .conf("color_table_cols"),
        discriminator = "all_of"
    )
    # replace all duplicates
    new_colors <- dplyr::pull(color_tb, .data[["name"]])
    # remove duplicate colors
    old_color_tb <- dplyr::filter(sits_env[["color_table"]],
                                  !(.data[["name"]] %in% new_colors))
    sits_env[["color_table"]] <- dplyr::bind_rows(old_color_tb, color_tb)
    return(invisible(sits_env[["color_table"]]))
}
#' @title Merge user colors with default colors
#' @name .conf_merge_colors
#' @description Combines user colors with default color table
#' @param user_colors  list of user colors
#' @keywords internal
#' @noRd
#' @return new color table
.conf_merge_colors <- function(user_colors) {
    # get the current color table
    color_table <- .conf_colors()
    names_user_colors <- names(user_colors)
    col_user_colors <- unname(user_colors)
    for (i in seq_along(names_user_colors)) {
        name <- names_user_colors[[i]]
        col <- col_user_colors[[i]]
        id <- which(color_table[["name"]] == name)
        if (length(id) > 0) {
            color_table[id, "color"] <- col
        } else {
            color_table <- tibble::add_row(color_table,
                name = name,
                color = col
            )
        }
    }
    sits_env[["color_table"]] <- color_table
    return(color_table)
}
#' @title Merge user legends with default legends
#' @name .conf_merge_legends
#' @description Combines user legends with default
#' @param user_legends  List of user legends
#' @keywords internal
#' @noRd
#' @return new color table
.conf_merge_legends <- function(user_legends){
    .check_set_caller(".conf_merge_legends")
    # check legends are valid names
    .check_chr_parameter(names(user_legends), len_max = 100,
                         msg = .conf("messages", ".conf_merge_legends_user"))
    # check legend names do not already exist
    .check_that(!(any(names(user_legends) %in% names(sits_env[["legends"]]))))
    # check colors names are valid
    ok <- purrr::map_lgl(user_legends, function(leg){
        .check_chr_parameter(leg, len_max = 100,
                        msg = .conf("messages", ".conf_merge_legends_colors"))
        return(TRUE)
    })
    sits_env[["legends"]] <- c(sits_env[["legends"]], user_legends)
    return(invisible(sits_env[["legends"]]))

}
#' @title Return the default color table
#' @name .conf_colors
#' @keywords internal
#' @noRd
#' @return default color table
#'
.conf_colors <- function() {
    return(sits_env[["color_table"]])
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
    .check_require_packages("showtext")
    showtext::showtext_auto()
    sysfonts::font_add_google("IBM Plex Sans", family = "plex_sans")
    sysfonts::font_add_google("Roboto", family = "roboto")
    sysfonts::font_add_google("Lato", family = "lato")

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
                msg = .conf("messages", ".conf_user_env_var")
            )
        )
        # if the YAML file exists, try to load it
        tryCatch({
            yaml_user_config <- yaml::yaml.load_file(
                    input = yml_file,
                    merge.precedence = "override"
            )},
            error = function(e) {
                warning(.conf("messages", ".conf_user_env_var"), call. = TRUE)
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
    .check_set_caller(".conf_set_user_file")
    # try to find a valid user configuration file
    # check config user file is valid
    if (.has(config_user_file) && !is.na(config_user_file)) {
        user_config <- tryCatch(
            yaml::yaml.load_file(config_user_file, error.label = "",
                                 readLines.warn = FALSE),
            error = function(e) {
                stop(.conf("messages", ".conf_set_user_file"), call. = TRUE)
            }
        )
    } else {
        user_config <- .conf_user_env_var()
    }
    if (.has(user_config)) {
        if (.has(user_config[["colors"]])) {
            user_colors <- user_config[["colors"]]
            .conf_merge_colors(user_colors)
            user_config[["colors"]] <- NULL
        }
        if (.has(user_config[["legends"]])) {
            user_legends <- user_config[["legends"]]
            .conf_merge_legends(user_legends)
            user_config[["legends"]] <- NULL
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
                colors = user_config[["colors"]],
                view   = user_config[["view"]],
                plot   = user_config[["plot"]]
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
        within = c(sits_bands, source_bands)
    )
    return(invisible(bands))
}
#' @title List configuration parameters
#' @name .conf_list_params
#' @description List the contents of a source
#'
#' @keywords internal
#' @noRd
#' @param params        parameter list
#'
#' @return              Called for side effects.
.conf_list_params <- function(params) {
    params <- lapply(params, function(x) {
        if (is.atomic(x)) {
            return(x)
        }
        list(names(x))
    })
    params_txt <- yaml::as.yaml(
        params,
        indent = 4,
        handlers = list(
            character = function(x) {
                res <- toString(x)
                class(res) <- "verbatim"
                res
            },
            integer = function(x) {
                res <- toString(x)
                class(res) <- "verbatim"
                res
            },
            numeric = function(x) {
                res <- toString(x)
                class(res) <- "verbatim"
                res
            }
        )
    )
    cat(params_txt, sep = "\n")
}

#' @title List contents of a source
#' @name .conf_list_source
#' @description List the contents of a source
#'
#' @keywords internal
#' @noRd
#' @param source        Data source
#'
#' @return              Called for side effects.
.conf_list_source <- function(source){
    cat(paste0(source, ":\n"))
    collections <- .source_collections(source)
    purrr::map(collections, function(col) {
        cat(paste0("- ", col))
        cat(paste0(
            " (", .source_collection_satellite(source, col),
            "/", .source_collection_sensor(source, col), ")\n",
            "- grid system: ", .source_collection_grid_system(source, col), "\n"
        ))
        cat("- bands: ")
        cat(.source_bands(source, col))
        cat("\n")
        if (.source_collection_open_data(source, col)) {
            cat("- opendata collection ")
            if (.source_collection_open_data(
                source = source,
                collection = col,
                token = TRUE
            )) {
                cat("(requires access token)")
            }
        } else {
            cat("- not opendata collection")
        }
        cat("\n")
        cat("\n")
    })
}


#' @title Get names associated to a configuration key
#' @name .conf_names
#' @param key   key combination to access config information
#' @keywords internal
#' @noRd
#' @return   names associated to the chosen access key
.conf_names <- function(...) {
    key <- c(...)
    res <- tryCatch(
        {
            names(sits_env[["config"]][[key]])
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
    .check_chr_parameter(s3_class,
        allow_empty = FALSE, len_min = 1,
        msg = .conf("messages", ".conf_new_source_s3class")
    )

    if (!is.null(service)) {
        .check_chr_parameter(service,
            allow_empty = FALSE, len_min = 1, len_max = 1,
            msg = .conf("messages", ".conf_new_source_service")
        )
    }
    if (!is.null(url)) {
        .check_chr_parameter(url,
            allow_empty = FALSE, len_min = 1, len_max = 1,
            regex = '^(http|https)://[^ "]+$',
            msg = .conf("messages", ".conf_new_source_url")
        )
    }
    .check_lst(collections, len_min = 1)
    names(collections) <- toupper(names(collections))

    collections <- lapply(collections, function(collection) {
        # pre-condition
        .check_lst_parameter(collection,
            len_min = 1,
            msg = .conf("messages", ".conf_new_source_collections")
        )
        # collection members must be lower case
        names(collection) <- tolower(names(collection))
        collection <- .check_error(
            {
                do.call(.conf_new_collection, args = collection)
            },
            msg = .conf("messages", ".conf_new_source_collections")
        )
        return(collection)
    })

    # extra parameters
    dots <- list(...)
    .check_lst_parameter(dots, len_min = 0,
                msg = .conf("messages", ".conf_new_source_collections_args"))

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
    .check_chr_parameter(satellite,
        allow_null = TRUE,
        msg = .conf("messages", ".conf_new_collection_satellite")
    )
    #  check sensor
    .check_chr(sensor,
        allow_null = TRUE,
        msg = .conf("messages", ".conf_new_collection_sensor")
    )
    # check metadata_search
    if (!missing(metadata_search)) {
        .check_chr_within(metadata_search,
            within = .conf("metadata_search_strategies"),
            msg = .conf("messages", ".conf_new_collection_metadata")
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
            len_min = 1,
            msg = .conf("messages", ".conf_new_collection_bands")
        )
        # bands' members are lower case
        names(band) <- tolower(names(band))
        band <- .check_error(
            {
                do.call(.conf_new_band, args = band)
            },
            msg = .conf("messages", ".conf_new_collection_bands")
        )
        return(band)
    })

    cloud_band <- lapply(cloud_band, function(cloud_band) {
        # pre-condition
        .check_lst(bands,
            len_min = 1,
            msg = .conf("messages", ".conf_new_collection_bands")
        )
        # bands' members are lower case
        names(cloud_band) <- tolower(names(cloud_band))
        cloud_band <- .check_error(
            {
                do.call(.conf_new_cloud_band, args = cloud_band)
            },
            msg = .conf("messages", ".conf_new_collection_bands")
        )
        return(cloud_band)
    })

    # extra parameters
    dots <- list(...)
    .check_lst(dots,
               msg = .conf("messages", ".conf_new_collection_metadata_args")
    )

    res <- c(list(bands = c(non_cloud_bands, cloud_band)),
        "satellite" = satellite,
        "sensor" = sensor,
        "metadata_search" = metadata_search, dots
    )
    # post-condition
    .check_lst(res,
        len_min = 1,
        msg = .conf("messages", ".conf_new_collection")
    )
    .check_lst(res$bands,
        len_min = 1,
        msg = .conf("messages", ".conf_new_collection_bands")
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
    .check_set_caller(".conf_new_band")
    # pre-condition
    .check_num_parameter(
        missing_value,
        len_min = 1,
        len_max = 1
    )
    .check_num_parameter(
        minimum_value,
        len_min = 1,
        len_max = 1
    )
    .check_num_parameter(
        x = maximum_value,
        len_min = 1,
        len_max = 1
    )
    .check_num_parameter(
        scale_factor,
        len_min = 1,
        len_max = 1,
        exclusive_min = 0
    )
    .check_num_parameter(
        offset_value,
        len_min = 1,
        len_max = 1
    )
    .check_num_parameter(
        resolution,
        exclusive_min = 0,
        len_min = 1,
        len_max = 1,
        allow_null = TRUE
    )
    .check_chr(
        band_name,
        allow_empty = FALSE,
        len_min = 1,
        len_max = 1
    )
    # extra parameters
    dots <- list(...)
    .check_lst(dots, msg = .conf("messages", ".check_new_band_dots"))

    new_band_params <- c(list(
        missing_value = missing_value,
        minimum_value = minimum_value,
        maximum_value = maximum_value,
        scale_factor = scale_factor,
        offset_value = offset_value,
        band_name = band_name,
        resolution = resolution
    ), dots)

    # post-condition
    .check_lst_parameter(new_band_params,
        len_min = 7
    )
    # return a band object
    return(new_band_params)
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
    .check_lgl_parameter(bit_mask)
    .check_lst_parameter(values, fn_check = .check_chr)
    .check_int_parameter(interp_values, len_min = 1)
    .check_chr_parameter(band_name, len_min = 1, len_max = 1)

    # extra parameters
    dots <- list(...)
    .check_lst(dots, msg = .conf("messages", ".check_new_cloud_band_dots"))

    cloud_band_params <- c(list(
        bit_mask = bit_mask,
        values = values,
        interp_values = interp_values,
        resolution = resolution,
        band_name = band_name
    ), dots)

    # post-condition
    .check_lst_parameter(cloud_band_params, len_min = 5)

    # return a cloud band object
    return(cloud_band_params)
}
#' @title Retrieve the rstac pagination limit
#' @name .conf_rstac_limit
#' @keywords internal
#' @noRd
#' @return pagination limit to rstac output
.conf_rstac_limit <- function() {
    res <- .conf("rstac_pagination_limit")
    return(res)
}
#' @title Retrieve the raster package to be used
#' @name .conf_raster_pkg
#' @keywords internal
#' @noRd
#' @return the raster package used to process raster data
#'
.conf_raster_pkg <- function() {
    res <- .conf("raster_api_package")
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
#' @name .conf_exists
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
#'     # tests if 'BDC -> MOD13Q1-6.1 -> NDVI' key exists in config
#'     .conf_eo_band_exists(
#'         source = "BDC",
#'         collection = "MOD13Q1-6.1",
#'         band = "NDVI"
#'     )
#'     # get configuration for band NDVI of 'BDC -> MOD13Q1-6' collection
#'     x <- .conf_eo_band(
#'         source = "BDC",
#'         collection = "MOD13Q1-6.1",
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
#' @param band  Band name
#' @details
#' If the band is not found, a default value will be returned from config.
#' If neither source nor collection entries are found in configuration file,
#' an error is thrown.
#' @returns  A value in config.
.conf_eo_band <- function(source, collection, band) {
    # Format band name
    band <- .band_eo(band)
    # does the band exists in cube config?
    if (!.conf_eo_band_exists(source, collection, band)) {
        return(NULL)
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
#' @title Get the S3 class values of a `vector_cube`
#' @noRd
#' @param vector_class  A `vector_cube` class name.
#' @return A S3 class.
.conf_vector_s3class <- function(vector_class) {
    # derived_class is lowercase
    vector_class <- tolower(vector_class)
    .conf("vector_cube", vector_class, "s3_class")
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
.conf_parse_info <- function(parse_info, results_cube = FALSE) {
    # is parse info NULL? use the default
    if (!.has(parse_info)) {
        if (results_cube) {
            parse_info <- .conf("results_parse_info_def")
        } else {
            parse_info <- .conf("local_parse_info_def")
        }
        return(parse_info)
    }

    # precondition - does the parse info have band and date?
    if (results_cube) {
        .check_chr_contains(
            parse_info,
            contains = .conf("results_parse_info_col"),
            msg = .conf("messages", ".conf_parse_info_results")
        )
    } else {
        .check_chr_contains(
            parse_info,
            contains = .conf("local_parse_info_col"),
            msg = .conf("messages", ".conf_parse_info")
        )
    }
    return(parse_info)
}
