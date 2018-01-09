# On load
.onAttach = function(lib, pkg){
    packageStartupMessage("Satellite Image Time Series package.")
    packageStartupMessage(
        sprintf("Loaded sits v%s. See ?sits for help, citation(\"sits\") for use in publication.
                See demo(package = 'sits') for examples.", utils::packageDescription("sits")$Version) )
}
.onLoad = function(lib, pkg) {
    Sys.setenv(R_CONFIG_ACTIVE = "default")
    Sys.setenv(R_CONFIG_FILE = "config.yml")
    sits_config()
    sits_log()
}
sits.env <- new.env()
sits.env$config <- NULL
# Include the following global variables in the SITS package
utils::globalVariables(c(".", "%>%", "Index", "from", "value", "variable", "cond",
                         "med", "qt25", "qt75", "cluster_id",
                         "longitude", "latitude", "label", "coverage", "service",
                         "year", "start_date", "end_date", "time_series",
                         "name", "scale_factor", "missing_value",
                         "original_label", "n_members", "n_members.n", "count",
                         "segr", "frac", "total", "n",
                         "distance", "predicted", "new_values",
                         "Alig.N", "matches", "reference", "cluster", ".data", "logger"))
#' @import dtwSat
#' @import dtwclust
#' @importFrom dtw symmetric1 symmetric2
#' @importFrom lubridate %within% %m+%
#' @useDynLib sits, .registration = TRUE
NULL
