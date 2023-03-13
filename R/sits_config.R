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

    # set the default color table
    .conf_load_color_table()

    # try to find a valid user configuration file
    user_yml_file <- .conf_user_file()

    if (file.exists(user_yml_file)) {
        config <- yaml::yaml.load_file(
            input = user_yml_file,
            merge.precedence = "override"
        )
        if (!purrr::is_null(config$colors)) {
            user_colors <- config$colors
            .conf_merge_colors(user_colors)
            config$colors <- NULL
        }
        if (length(config) > 0 ) {
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
        }

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
#' @title Get color table
#' @name .conf_load_color_table
#' @description Loads the default color table
#' @keywords internal
#' @noRd
#' @return NULL, called for side effects
.conf_load_color_table <- function(){
    # load the color configuration file
    color_yml_file <- .conf_colors_file()
    config_colors <- yaml::yaml.load_file(
        input = color_yml_file,
        merge.precedence = "override"
    )
    config_colors <- config_colors$colors
    base_names <- names(config_colors)
    color_table <- purrr::map2_dfr(config_colors, base_names, function(cl, bn){
        cc_tb <- tibble::tibble(name = names(cl),
                                color = unlist(cl),
                                group = bn)
        return(cc_tb)
    })
    # set the color table
    .conf_set_color_table(color_table)
    return(invisible(NULL))
}
#' @title Set user color table
#' @name .conf_set_color_table
#' @description Loads a user color table
#' @keywords internal
#' @noRd
#' @return NULL, called for side effects
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
                msg = "color table contains duplicate names")

    # pre condition - valid hex codes?
    # .is_color <- function(x)
    # {
    #     res <- try(col2rgb(x),silent = TRUE)
    #     return(!"try-error" %in% class(res))
    # }
    # # check values one by one - to help user find wrong value
    # col_vls <- unname(color_tb$color)
    # print(col_vls)
    # purrr::map(col_vls, function(col) {
    #     .check_that(.is_color(col),
    #                 msg = paste0("invalid color code ", col, " in color table")
    #     )
    # })
    sits_env$color_table <- color_tb
    return(invisible(NULL))
}
.conf_merge_colors <- function(user_colors) {
    # get the current color table
    color_table <- .conf_colors()
    names_user_colors <- names(user_colors)
    col_user_colors <- unname(user_colors)
    for (i in seq_along(names_user_colors)) {
        name <- names_user_colors[[i]]
        col <- col_user_colors[[i]]
        id <- which(color_table$name == name)
        if (length(id) > 0)
            color_table[id, "color"] <- col
        else
            color_table <- tibble::add_row(color_table,
                                           name = name,
                                           color = col,
                                           group = "User")
    }
    .conf_set_color_table(color_table)
    return(invisible(NULL))
}
.conf_colors <- function(){
    return(sits_env$color_table)
}
