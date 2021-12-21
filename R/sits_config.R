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
#' @param processing_bloat       A \code{numeric} value to estimate
#' growth size of R memory relative to block size.
#' @param rstac_pagination_limit A \code{numeric} value indicating number of
#' items returned by STAC service.
#' @param raster_api_package     A \code{character} value indicating a
#' supported raster handling package.
#' @param gdal_creation_options  A \code{character} vector specifying GDAL
#' creation option for GeoTiff image format.
#' @param gdalcubes_chunk_size   A \code{numeric} vector specifying the chunk size
#'                               to be used by gdalcubes
#' @param leaflet_max_Mbytes     A \code{numeric} value indicating the maximum
#'                               size of an image to be shown by leaflet (in MB)
#' @param leaflet_comp_factor    A \code{numeric} value indicating the compression
#'                               factor for leaflet RGB display
#' @param reset                  A \code{logical} value indicating if current
#'                               configuration options must be cleaned before
#'                               load config files. Default \code{FALSE}.
#' @param source                 A \code{character} value indicating a source
#' key entry to be shown in detail.
#' @param collection             A \code{character} value used in conjunction
#' with \code{source} parameter to indicate a collection key entry to be shown
#' in detail.
#' @param colors                A \code{logical} value indicating if colors
#' will to be shown in detail.
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
                        gdalcubes_chunk_size = NULL,
                        leaflet_max_Mbytes = NULL,
                        leaflet_comp_factor = NULL,
                        reset = FALSE) {

    # clear current configuration
    if (reset && !is.null(sits_env$config))
        sits_env$config <- list()

    # load the internal configuration file
    config_internals_file <- .config_internals_file()

    # read the configuration parameters
    config_internals <- yaml::yaml.load_file(input = config_internals_file,
                                             merge.precedence = "override")

    # set options defined in sits config
    do.call(.config_set_options, args = config_internals)


    # get and check the default configuration file path
    yml_file <- .config_file()

    # read the configuration parameters
    config <- yaml::yaml.load_file(input = yml_file,
                                   merge.precedence = "override")

    # set options defined in sits config
    do.call(.config_set_options, args = config)
    message(paste0("Using configuration file: ", yml_file))

    # load the color configuration file
    color_yml_file <- .config_colors_file()
    message(paste("Color configurations found in", color_yml_file))
    config_colors <- yaml::yaml.load_file(input = color_yml_file,
                                   merge.precedence = "override")
    .config_set_options(colors = config_colors[["colors"]])

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
            gdalcubes_chunk_size = config[["gdalcubes_chunk_size"]],
            leaflet_max_Mbytes = config[["leaflet_max_Mbytes"]],
            leaflet_comp_factor = config[["leaflet_comp_factor"]],
            sources = config[["sources"]],
            colors = config[["colors"]]
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
                        gdal_creation_options = gdal_creation_options,
                        gdalcubes_chunk_size = gdalcubes_chunk_size,
                        leaflet_max_Mbytes = leaflet_max_Mbytes,
                        leaflet_comp_factor = leaflet_comp_factor
                        )

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
                             colors = FALSE) {

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

    } else if (colors) {
        config <- config[["colors"]]
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
            else
                cat("- not opendata collection")
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
                                gdalcubes_chunk_size = NULL,
                                leaflet_max_Mbytes = NULL,
                                leaflet_comp_factor = NULL,
                                sources = NULL,
                                colors = NULL, ...) {
    # set caller to show in errors
    .check_set_caller(".config_set_options")

    # initialize config
    if (!exists("config", envir = sits_env))
        sits_env$config <- list()

    # process processing_bloat
    if (!is.null(processing_bloat)) {
        .check_num(processing_bloat,
                   min = 1, len_min = 1, len_max = 1,
                   msg = "invalid 'processing_bloat' parameter")
        sits_env$config[["processing_bloat"]] <- processing_bloat
    }

    # process rstac_pagination_limit
    if (!is.null(rstac_pagination_limit)) {
        .check_num(rstac_pagination_limit,
                   min = 1, len_min = 1, len_max = 1,
                   msg = "invalid 'rstac_pagination_limit' parameter")
        sits_env$config[["rstac_pagination_limit"]] <- rstac_pagination_limit
    }

    # process raster_api_package
    if (!is.null(raster_api_package)) {
        .check_chr(raster_api_package,
                   len_min = 1, len_max = 1,
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
                   msg = "invalid 'gdal_creation_options' parameter")
        sits_env$config[["gdal_creation_options"]] <- gdal_creation_options
    }
    # process gdalcubes_chunk_size
    if (!is.null(gdalcubes_chunk_size)) {
        .check_num(gdalcubes_chunk_size,
                   min_len = 3,
                   max_len = 3,
                   is_named = FALSE,
                   msg = "invalid gdalcubes chunk size")
        sits_env$config[["gdalcubes_chunk_size"]] <- gdalcubes_chunk_size
    }
    if (!is.null(leaflet_max_Mbytes)) {
        .check_num(leaflet_max_Mbytes,
                   min = 16,
                   max = 128,
                   is_named = FALSE,
                   msg = "invalid leaflet max Mbytes")
        sits_env$config[["leaflet_max_Mbytes"]] <- leaflet_max_Mbytes
    }
    if (!is.null(leaflet_comp_factor)) {
        .check_num(leaflet_comp_factor,
                   min = 0.45,
                   max = 0.75,
                   is_named = FALSE,
                   msg = "invalid leaflet_comp_factor")
        sits_env$config[["leaflet_comp_factor"]] <- leaflet_comp_factor
    }
    # process sources
    if (!is.null(sources)) {

        .check_lst(sources, min_len = 1)

        # source names are uppercase
        names(sources) <- toupper(names(sources))

        sources <- lapply(sources, function(source) {

            # pre-condition
            .check_lst(source, min_len = 2,
                       msg = "invalid 'source' parameter")

            # check that source contains essential parameters
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
    }
    # check and initialize palettes
    if (!is.null(colors)) {
        # initialize colors
        if (is.null(sits_env$config[["colors"]]))
            sits_env$config[["colors"]] <- colors
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
#' @name config_file
#' @keywords internal
#' @return default configuration file
#'
#'
.config_file <- function() {

    # load the default configuration file
    yml_file <- system.file("extdata", "config.yml", package = "sits")

    # check that the file name is valid
    .check_file(yml_file, msg = "invalid configuration file")

    return(yml_file)
}

#' @title Return the internal configuration file (only for developers)
#' @name config_internals_file
#' @keywords internal
#' @return default configuration file
.config_internals_file <- function() {

    # load the default configuration file
    yml_file <- system.file("extdata", "config_internals.yml", package = "sits")

    # check that the file name is valid
    .check_file(yml_file, msg = "invalid configuration file")

    return(yml_file)
}
#' @title Return the default configuration file for colors
#' @name config_colors_file
#' @keywords internal
#' @return default configuration file
.config_colors_file <- function() {

    # load the default configuration file
    yml_file <- system.file("extdata", "config_colors.yml", package = "sits")

    # check that the file name is valid
    .check_file(yml_file, msg = "invalid configuration file")

    return(yml_file)
}

#' @title Return the user configuration file
#' @name .config_user_file
#' @keywords internal
#' @return user configuration file
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

#' @title Given a key, get config values
#' @name .config_get
#' @keywords internal
#' @return config values associated to a key
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
#' @title Check band availability
#' @name .config_check_bands
#' @description Checks if the requested bands are available in the collection
#'
#' @keywords internal
#' @param source        Data source
#' @param collection    Collection to be searched in the data source.
#' @param bands         Bands to be included.
#'
#' @return An invisible null
.config_check_bands <- function(source, collection, bands) {

    # set caller to show in errors
    .check_set_caller(".config_check_bands")

    sits_bands <- .source_bands(source = source,
                                collection = collection)
    source_bands <- .source_bands_band_name(source = source,
                                            collection = collection)

    .check_chr_within(x = bands,
                      within = c(sits_bands, source_bands),
                      msg = paste("invalid bands.\nPlease verify",
                                  "the provided bands."))

    # remove bands with equal names, like NDVI, EVI...
    source_bands <- source_bands[!source_bands %in% sits_bands]

    return(invisible(NULL))
}
#' @title Check GEOTIFF creation options
#' @name .config_gtiff_default_options
#' @keywords internal
#' @return  the creation options associated to the configuration
.config_gtiff_default_options <- function() {

    res <- .config_get(key = c("gdal_creation_options"))

    # post-condition
    .check_chr(res, allow_empty = FALSE,
               msg = "invalid 'gdal_creation_options' in config file")

    return(res)
}
#' @title Check metatype associated to the data
#' @name .config_data_meta_type
#' @keywords internal
#' @description associates a valid SITS class to the data
#'
#' @param  data    time series or cube
#'
#' @return an error if the meta data type is wrong
.config_data_meta_type <- function(data) {

    # set caller to show in errors
    .check_set_caller(".config_data_meta_type")

    # if the data is one of the classes recognized by sits
    if (inherits(data, .config_get("sits_s3_classes"))) {
        return(data)

    } else if (inherits(data, "tbl_df")) {
        # is this a data cube or a sits tibble?
        if (all(.config_get("sits_cube_cols")
                %in% colnames(data))) {

            class(data) <- c("raster_cube", class(data))

            return(data)
        } else if (all(.config_get("sits_tibble_cols") %in% colnames(data))) {

            class(data) <- c("sits", class(data))
            return(data)
        }
    }

    .check_that(FALSE,
                local_msg = "Data not recognized as a sits object",
                msg = "invalid 'data' parameter")
}
#' @title Get local file extensions
#' @name .config_local_file_extension
#' @keywords internal
#' @return local file extensions known to sits
.config_local_file_extensions <- function() {

    res <- .config_get(key = c("local_file_extensions"))

    # post-condition
    .check_chr(res, len_min = 1,
               msg = "invalid 'file_extensions' in config file")

    return(res)
}
#' @title Get local S3 class
#' @name .config_local_s3_class
#' @keywords internal
#' @return classes associated to local cubes
.config_local_s3_class <- function() {

    res <- .config_get(key = c("local_s3_class"))

    # post-condition
    .check_chr(res, len_min = 1,
               msg = "invalid 'local_s3_class' in config file")

    return(res)
}
#' @title Get names associated to a configuration key
#' @name .config_names
#' @param key   key combination to access config information
#' @keywords internal
#' @return   names associated to the chosen access key
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
#' @title Include a new source in the configuration
#' @name .config_new_source
#' @description creates a new data source in the config file
#' @param s3_class   s3 class associated to the source
#' @param collections collections associated to the source
#' @param ...         other parameters associated to the new source
#' @param service     protocol associated to the source (e.g., "STAC")
#' @param url         url associated to the endpoint of the service
#' @keywords internal
#' @return   list with the configuration associated to the new source
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
#' @title Include a new collection in the configuration
#' @name .config_new_collection
#' @description creates a new collection associated to a source
#' @param bands       bands associated to the collection
#' @param ...         other relevant parameters
#' @param satellite   satellite associated to the collection
#' @param sensor      sensor associated to the collection
#' @keywords internal
#' @return   list with the configuration associated to the new collection
.config_new_collection <- function(bands, ...,
                                   satellite = NULL,
                                   sensor = NULL,
                                   metadata_search = NULL) {
    # set caller to show in errors
    .check_set_caller(".config_new_collection")

    # check satellite
    .check_chr(satellite, allow_null = TRUE,
               msg = "invalid 'satellite' value")

    #  check sensor
    .check_chr(sensor, allow_null = TRUE,
               msg = "invalid 'sensor' value")

    # check metadata_search
    if (!missing(metadata_search))
        .check_chr_within(metadata_search,
                          within = .config_metadata_search_strategies(),
                          msg = "invalid 'metadata_search' value")

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
             "sensor" = sensor,
             "metadata_search" = metadata_search, dots)

    # post-condition
    .check_lst(res, min_len = 1,
               msg = "invalid 'collection' value")

    .check_lst(res$bands, min_len = 1,
               msg = "invalid collection 'bands' value")

    # return a new collection data
    return(res)
}
#' @title Include a new band in the configuration
#' @name .config_new_band
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
#' @return   list with the configuration associated to the new band
.config_new_band <- function(missing_value,
                             minimum_value,
                             maximum_value,
                             scale_factor,
                             offset_value,
                             band_name,
                             resolution,...) {

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

    if (!is.null(resolution))
        .check_num(resolution, len_min = 1, allow_zero = FALSE,
                   msg = "invalid 'resolution' parameter")

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
                  band_name = band_name,
                  resolution = resolution), dots)

    # post-condition
    .check_lst(res, min_len = 7,
               msg = "invalid 'band' value")

    # return a band object
    return(res)
}
#' @title Include a new cloud band in the configuration
#' @name .config_new_cloud_band
#' @description creates a description associated to a new cloud band
#' @param bit_mask       bit mask to describe clouds (if applicable)
#' @param value          values of the cloud band
#' @param interp_values  pixel values that need to be replaced by interpolation
#' @param resolution     spatial resolution (in meters)
#' @param band_name      name of the band
#' @param ...            other relevant parameters
#' @keywords internal
#' @return   list with the configuration associated to the new band
.config_new_cloud_band <- function(bit_mask,
                                   values,
                                   interp_values,
                                   resolution,
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

    .check_chr(band_name, allow_empty = FALSE, len_min = 1, len_max = 1,
               msg = "invalid 'band_name' value")

    # extra parameters
    dots <- list(...)
    .check_lst(dots, "invalid extra arguments in cloud band")

    res <- c(list(bit_mask = bit_mask,
                  values = values,
                  interp_values = interp_values,
                  resolution = resolution,
                  band_name = band_name), dots)

    # post-condition
    .check_lst(res, min_len = 5,
               msg = "invalid 'band' value")

    # return a cloud band object
    return(res)
}
#' @title Get colors associated to the labels
#' @name .config_colors
#' @param  labels  labels associated to the training classes
#' @param  palette  palette from `grDevices::hcl.pals()`
#'                  replaces default colors
#'                  when labels are not included in the config palette
#' @param  rev      revert the order of colors?
#' @keywords internal
#' @return colors required to display the labels
.config_colors <- function(labels,
                           palette = "Harmonic",
                           rev = TRUE) {

    # ensure labels are unique
    labels <- unique(labels)
    # get the names of the colors in the chosen palette
    colors_palette <- unlist(.config_get(key = "colors"))
    # if labels are included in the config palette, use them
    if (all(labels %in% names(colors_palette))) {
        colors <- colors_palette[labels]
    }
    else {
        labels_found <- labels[labels %in% names(colors_palette)]
        if (length(labels_found) > round(length(labels)/2)) {
            warning("Some labels are not available in the chosen palette",
                    call. = FALSE)
            missing_labels <- unique(labels[!(labels %in% labels_found)])
            warning(paste0("Consider adjusting labels: ", missing_labels),
                    call. = FALSE)
        }
        else
            warning("Most labels are not available in the chosen palette",
                    call. = FALSE)

        warning(paste0("Using hcl_color palette ", palette),
                call. = FALSE)

        # get the number of labels
        n_labels <- length(unique(labels))
        # generate a set of hcl colors
        colors <- grDevices::hcl.colors(n = n_labels,
                                        palette = palette,
                                        alpha = 1,
                                        rev = rev)
        names(colors) <- labels
    }
    # post-condition
    .check_chr(colors,
               len_min = length(labels),
               len_max = length(labels),
               is_named = TRUE,
               msg = "invalid color values")

    return(colors)
}
#' @title Retrieve the processing bloat
#' @name .config_processing_bloat
#' @keywords internal
#' @return estimated processing bloat
.config_processing_bloat <- function() {

    res <- .config_get(key = c("processing_bloat"))

    # post-condition
    .check_num(res, min = 1, len_min = 1, len_max = 1,
               msg = "invalid 'processing_bloat' in config file")

    return(res)
}
#' @title Retrieve the parallel requests number
#' @name .config_gdalcubes_open_connections
#' @keywords internal
#' @return get parallel requests
.config_gdalcubes_open_connections <- function() {

    n_conn <- .config_get(key = c("gdalcubes_open_connections"))

    # post-condition
    .check_num(n_conn, min = 1, len_min = 1, len_max = 1,
               msg = "invalid 'gdalcubes_open_connections' in config file")

    return(n_conn)
}
#' @title Retrieve the minimum requests to do in parallel
#' @name .config_gdalcubes_min_files_for_parallel
#' @keywords internal
#' @return get minimum times the parallel requests will be done
.config_gdalcubes_min_files_for_parallel <- function() {

    min_files <- .config_get(key = c("gdalcubes_min_files_for_parallel"))

    # post-condition
    .check_num(min_files, min = 1, len_min = 1, len_max = 1,
               msg = paste("invalid 'gdalcubes_min_files_for_parallel' in",
                           "config file"))

    return(min_files)
}
#' @title Retrieve the gdalcubes chunk size
#' @name .config_gdalcubes_chunk_size
#' @keywords internal
#' @return a numeric vector with chunk size
.config_gdalcubes_chunk_size <- function() {

    chunk_size <- .config_get(key = c("gdalcubes_chunk_size"))

    # post-condition
    .check_num(chunk_size, len_min = 3, len_max = 3,
               msg = "invalid 'gdalcubes_chunk_size' in config file")

    return(chunk_size)
}
#' @title Retrieve the maximum number of threads in gdalcubes
#' @name .config_gdalcubes_max_threads
#' @keywords internal
#' @return a numeric with the number of threads
.config_gdalcubes_max_threads <- function() {

    n_threads <- .config_get(key = c("gdalcubes_max_threads"))

    # post-condition
    .check_num(n_threads, min = 1, len_min = 1, len_max = 1,
               msg = "invalid 'gdalcubes_max_threads' in config file")

    return(n_threads)
}
#' @title Retrieve the valid types of metadata search
#' @name .config_metadata_search_strategies
#' @keywords internal
#' @return Character values
.config_metadata_search_strategies <- function() {

    res <- .config_get(key = c("metadata_search_strategies"))

    # post-condition
    .check_chr(res, len_min = 1,
               msg = "invalid 'metadata_search_strategies' in config file")

    return(res)
}
#' @title Retrieve the processing bloat
#' @name .config_processing_bloat
#' @keywords internal
#' @return estimated processing bloat
.config_processing_bloat <- function() {

    res <- .config_get(key = c("processing_bloat"))

    # post-condition
    .check_num(res, min = 1, len_min = 1, len_max = 1,
               msg = "invalid 'processing_bloat' in config file")

    return(res)
}
#' @title Retrieve the rstac pagination limit
#' @name .config_rstac_limit
#' @keywords internal
#' @return pagination limit to rstac output
.config_rstac_limit <- function() {

    res <- .config_get(key = c("rstac_pagination_limit"))

    # post-condition
    .check_num(res, min = 1, len_min = 1, len_max = 1,
               msg = "invalid 'rstac_pagination_limit' in config file")

    return(res)
}

#' @title Retrieve the raster package to be used
#' @name .config_raster_pkg
#' @keywords internal
#' @return the raster package used to process raster data
#'
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
