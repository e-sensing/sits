#' @title Reads a configuration file and loads it in the main environment
#' @name sits_config
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Reads a user-specified configuration file, located in a "config.yml" file
#' in the working directory. If this file is not found, reads a default package configuration file.
#' By default, the SITS configuration file "config.yml" is located at the directory "extdata" of the
#' package. The configuration file is an YAML file that should provide at least the following parameters:
#'
#' default:
#'    ts_servers     :
#'        - "WTSS"
#'        - "SATVEG"
#'    WTSS_server    : "http://www.dpi.inpe.br/tws/wtss"
#'    SATVEG_server  : "https://www.satveg.cnptia.embrapa.br"
#'    SATVEG_account : "/satvegws/ws/perfil/ZW46IXzr4pRzJlX/"
#'
#' To see the contents of the configuration file, please use the function \code{link[sits]{sits_show_config()}}.
#'
#' @return config_sits  A list with the configuration parameters used by SITS
#' @examples
#' config_sits <- sits_config()
#' @export
#'
sits_config <- function() {

    # try to find a valid configuration file
    WD <- getwd()
    if (file.exists(paste0(WD, "/config.yml")))
        yml_file <- paste0(WD, "/config.yml")
    else
        yml_file <- system.file("extdata", "config.yml", package = "sits")

    # check that the file is valid
    ensurer::ensure_that(yml_file, !purrr::is_null(.),
                         err_desc = "sits_config : Please provide a valid configuration file")

    # read the configuration parameters
    config_sits <<- config::get(file = yml_file)

    return(invisible(config_sits))
}

#' @title Shows the contents of the SITS configuration file
#' @name sits_show_config
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Displays the contents of SITS configuration file. For more details
#' on how to set the configuration file, please use the function \code{link[sits]{sits_config()}}.
#'
#' @return config_sits  A list with the configuration parameters used by SITS
#' @examples
#' config_sits <- sits_config()
#' @export
#'
sits_show_config <- function() {

    # try to find a valid configuration file
        WD <- getwd()
        if (file.exists(paste0(WD, "/config.yml")))
            yml_file <- paste0(WD, "/config.yml")
        else
            yml_file <- system.file("extdata", "config.yml", package = "sits")

        # check that the file is valid
        ensurer::ensure_that(yml_file, !purrr::is_null(.),
                             err_desc = "sits_config : Please provide a valid configuration file")
    # read the configuration parameters
    cat(readLines(yml_file), sep = "\n")

    return(invisible())
}

