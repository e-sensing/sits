# On load
.onAttach <- function(lib, pkg) {
    packageStartupMessage("SITS - satellite image time series analysis.")
    packageStartupMessage(
        sprintf(
            "Loaded sits v%s.
        See ?sits for help, citation(\"sits\") for use in publication.
        Documentation avaliable in %s.",
            utils::packageDescription("sits")[["Version"]],
            "https://e-sensing.github.io/sitsbook/"
        )
    )
    packageStartupMessage(
        sprintf(
            "Important: Please read \"Release Notes for SITS 1.5.3\" in
                https://github.com/e-sensing/sits."
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
sits_env[["model_formula"]] <- "log"
# Include the following global variables in the sits package
utils::globalVariables(c(
    ".x", ".y", ":=", # dplyr
    "self", "ctx", "super", "private", # torch
    "uniform", "choice", "randint", # sits_tuning
    "normal", "lognormal", "loguniform", # sits_tuning
    "geometry", # sf operations
    "value", "label", "Eval", # ggplot
    "product:type", "grid:code", # CDSE stac
    "sar:frequency_band", "sar:instrument_mode", "sat:orbit_state" # S1 stac
))
#' @importFrom lubridate %within% %m+%
#' @importFrom Rcpp sourceCpp
#' @importFrom dplyr .data
#' @importFrom utils read.csv
#' @importFrom utils download.file
#' @useDynLib sits, .registration = TRUE
NULL
