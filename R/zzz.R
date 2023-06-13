# On load
.onAttach <- function(lib, pkg) {
    packageStartupMessage("SITS - satellite image time series analysis.")
    packageStartupMessage(
        sprintf(
            "Loaded sits v%s.
        See ?sits for help, citation(\"sits\") for use in publication.
        Documentation avaliable in %s.",
            utils::packageDescription("sits")$Version,
            "https://e-sensing.github.io/sitsbook/"
        )
    )
}

.onLoad <- function(lib, pkg) {
    Sys.setenv(R_CONFIG_FILE = "config.yml")
    sits_config()
}

# Creates a package environment to store global variables
sits_env <- new.env()
# Uses "log" as the default model formula
sits_env$model_formula <- "log"

# Include the following global variables in the sits package
utils::globalVariables(c(
    ".x", ":=", # dplyr
    "self", "ctx", "super", "private", # torch
    "uniform", "choice", "randint",
    "normal", "lognormal", "loguniform" # sits_tuning_random
))

#' @importFrom lubridate %within% %m+%
#' @importFrom Rcpp sourceCpp
#' @importFrom dplyr .data
#' @importFrom utils read.csv
#' @importFrom utils download.file
#' @useDynLib sits, .registration = TRUE
NULL
