#' @title Configure parameters for sits package
#' @name sits_config
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
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
#' use \code{link[sits]{sits_config_show()}}.
#'
#' @return Called for side effects
#'
#' @examples
#' yaml_user_file <- system.file("extdata/config_user_example.yml",
#'                   package = "sits")
#' sits_config(config_user_file = yaml_user_file)
#' @export
sits_config <- function(config_user_file = NULL) {
    # load the error messages file
    .conf_load_messages()
    # load the internal configuration file
    config_internals_file <- .conf_internals_file()
    # read the configuration parameters
    config_internals <- yaml::yaml.load_file(
        input = config_internals_file,
        merge.precedence = "override"
    )
    # set options defined in config_internals
    do.call(.conf_set_options, args = config_internals)

    # load the user-relevant configuration parameters
    config_file <- .config_file()
    config_user <- yaml::yaml.load_file(
        input = config_file,
        merge.precedence = "override"
    )
    # set options defined in config_internals
    do.call(.conf_set_options, args = config_user)

    # load sources configuration
    .conf_load_sources()
    # set the default color table
    .conf_load_color_table()
    # set the user options
    .conf_set_user_file(config_user_file)
    # set global leaflet
    .conf_load_leaflet()
    # return configuration
    return(invisible(sits_env[["config"]]))
}
#' @title Show current sits configuration
#' @name sits_config_show
#'
#' @description
#' Prints the current sits configuration options.
#' To show specific configuration options for
#' a source, a collection, or a palette, users can inform the corresponding
#' keys to \code{source} and \code{collection}.
#'
#' @return No return value, called for side effects.
#' @examples
#' sits_config_show()
#' @export
sits_config_show <- function() {
    config <- sits_env[["config"]]

    cat("Data sources and user configurable parameters in sits\n\n")
    cat("Data sources available in sits\n")
    cat(toString(.sources()))
    cat("\n\n")
    cat("Use sits_list_collections(<source>) to get details for each source\n\n")

    cat("User configurable parameters for plotting\n")
    config_plot <- sits_env[["config"]][["plot"]]
    .conf_list_params(config_plot)

    cat("User configurable parameters for visualisation\n")
    config_view <- sits_env[["config"]][["view"]]
    .conf_list_params(config_view)

    cat("Use sits_config_user_file() to create a user configuration file")
    return(invisible(NULL))
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
    .check_set_caller("sits_list_collections")
    # get sources available
    sources <- .sources()

    # if the user has required a source
    # check that it is valid
    if (.has(source)) {
        # check if source exists
        .check_chr_within(
            x = source,
            within = sources
        )
        sources <- source
    }
    purrr::map(sources, function(s) {
        .conf_list_source(s)
    })
    return(invisible(NULL))
}
#' @title List the cloud collections supported by sits
#' @name sits_config_user_file
#' @param  file_path file to store the user configuration file
#' @param  overwrite replace current configuration file?
#' @description
#' Creates a user configuration file.
#'
#' @return Called for side effects
#' @examples
#' user_file <- paste0(tempdir(), "/my_config_file.yml")
#' sits_config_user_file(user_file)
#' @export
sits_config_user_file <- function(file_path, overwrite = FALSE){
    # get default user configuration file
    user_conf_def <- system.file("extdata", "config_user_example.yml",
                                 package = "sits")
    update <- FALSE
    new_file <- FALSE
    # try to find if SITS_CONFIG_USER_FILE exists
    env <- Sys.getenv("SITS_CONFIG_USER_FILE")
    # file already exists
    if (file.exists(env)) {
        # does current env point to chosen file path?
        if (env == file_path) {
            # should I overwrite existing file?
            if (overwrite)
                update <- TRUE
            else
                update <- FALSE
        # if file path is not current the env variable, update it
        } else {
            update <- TRUE
        }
    } else {
        new_file <- TRUE
    }
    # update
    if (update || new_file){
        file.copy(
            from = user_conf_def,
            to = file_path,
            overwrite = TRUE
        )
        Sys.setenv(SITS_CONFIG_USER_FILE = file_path)
    }

    if (update)
        warning(.conf("messages", "sits_config_user_file_updated"))
    else if (new_file)
        warning(.conf("messages", "sits_config_user_file_new_file"))
    else
        warning(.conf("messages", "sits_config_user_file_no_update"))

    return(invisible(NULL))
}
