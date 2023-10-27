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
#' obtained by overriding the options by the values provided by the user.
#'
#' @param config_user_file    YAML user configuration file
#'                            (character vector of a file with "yml" extension)
#'
#' @details
#' Users can provide additional configuration files, by specifying the
#' location of their file in the environmental variable
#' \code{SITS_CONFIG_USER_FILE} or as parameter to this function.
#'
#' To see the key entries and contents of the current configuration values,
#' use \code{sits_config_show()}.
#'
#' @return Called for side effects
#'
#' @examples
#' yaml_user_file <- system.file("extdata/config_user_example.yml",
#'                   package = "sits")
#' sits_config(config_user_file = yaml_user_file)
#' @export
sits_config <- function(config_user_file = NULL) {
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
    # set the user options
    .conf_set_user_file(config_user_file)
    # set the fonts
    .conf_set_fonts()
    # return configuration
    return(invisible(sits_env$config))
}
#' @title Show current sits configuration
#' @name sits_config_show
#' @param source                 Data source (character vector).
#' @param collection             Collection (character vector).
#'
#' @description
#' Prints the current sits configuration options.
#' To show specific configuration options for
#' a source, a collection, or a palette, users can inform the corresponding
#' keys to \code{source} and \code{collection}.
#'
#' @return No return value, called for side effects.
#' @examples
#' sits_config_show(source = "BDC")
#' sits_config_show(source = "BDC", collection = "CBERS-WFI-16D")
#' @export
sits_config_show <- function(source = NULL,
                             collection = NULL) {
    config <- sits_env$config

    if (!is.null(source)) {
        # check source value
        .check_chr(source,
            allow_empty = FALSE,
            len_min = 1,
            len_max = 1
        )
        # check source is available
        source <- toupper(source)
        .check_chr_within(source,
            within = .sources(),
            discriminator = "one_of"
        )
        # get the configuration values associated to the source
        config <- config[[c("sources", source)]]
        # check collection value
        if (!is.null(collection)) {
            .check_chr(collection,
                allow_empty = FALSE,
                len_min = 1,
                len_max = 1
            )
            # check collection is available
            collection <- toupper(collection)
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
#' @examples
#' if (sits_run_examples()) {
#'     # show the names of the colors supported by SITS
#'     sits_list_collections()
#' }
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
                "- grid system: ", .source_collection_grid_system(s, c), "\n"
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
