#' @title Configure parameters for sits package
#' @name sits_config
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
#' \code{gdal_creation_options} and \code{gdalcubes_chunk_size}
#'
#' @param processing_bloat       Estimated growth size of R memory relative
#'                               to block size.
#' @param rstac_pagination_limit Limit of number of items returned by STAC.
#' @param gdal_creation_options  GDAL creation options for GeoTiff.
#' @param gdalcubes_chunk_size   Chunk size to be used by gdalcubes
#' @param reset                  Should current configuration options be
#'                               cleaned before loading config files?
#'                               Default is \code{FALSE}.
#'
#' @details
#' Users can provide additional configuration files, by specifying the
#' location of their file in the environmental variable
#' \code{SITS_CONFIG_USER_FILE}.
#'
#' To see the key entries and contents of the current configuration values,
#' use \code{sits_config_show()}.
#'
#' @return A list containing the current configuration options.
#'
#' @examples
#' current_config <- sits_config()
#' @export
sits_config <- function(processing_bloat = NULL,
                        rstac_pagination_limit = NULL,
                        gdal_creation_options = NULL,
                        gdalcubes_chunk_size = NULL,
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

    # set the default color table
    .conf_load_color_table()

    # set the use options
    .conf_set_user_file()

    # set options defined by user (via parameters)
    # modifying existing configuration
    .conf_set_options(
        processing_bloat = processing_bloat,
        rstac_pagination_limit = rstac_pagination_limit,
        gdal_creation_options = gdal_creation_options,
        gdalcubes_chunk_size = gdalcubes_chunk_size
    )

    return(invisible(sits_env$config))
}

#' @title Show current sits configuration
#' @name sits_config_show
#' @param source                 Data source to be shown in detail.
#' @param collection             Collection key entry to be shown in detail.
#' @param colors                 Show colors?
#'
#' @description
#' Prints the current sits configuration options.
#' To show specific configuration options for
#' a source, a collection, or a palette, users can inform the corresponding
#' keys to \code{source}, \code{collection}, and \code{colors} parameters.
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
        colors_sort <- sort(names(config), index.return = TRUE)
        config <- config[colors_sort$ix]
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

#' @title List the cloud collections supported by sits
#' @name sits_list_collections
#' @param source    Data source to be shown in detail.
#' @description
#' Prints the collections available
#' in each cloud service supported by sits.
#' Users can select to get information
#' only for a single service by using the \code{source} parameter.
#'
#' @return Prints collections available in
#'         each cloud service supported by sits.
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
                "/", .source_collection_sensor(s, c), ")\n",
                "- grid system: ", .source_collection_grid_system(s,c), "\n"
            ))
            cat("- bands: ")
            cat(.source_bands(s, c))
            cat("\n")
            if (.source_collection_open_data(source = s, collection = c)) {
                cat("- opendata collection ")
                if (.source_collection_open_data(
                    source = s,
                    collection = c,
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
    })
    return(invisible(NULL))
}

