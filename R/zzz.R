# On load
.onAttach = function(lib, pkg){
    packageStartupMessage("sits - satellite image time series analysis.")
    packageStartupMessage(
        sprintf("Loaded sits v%s. See ?sits for help, citation(\"sits\") for use in publication.
                See demo(package = \"sits\") for examples.", utils::packageDescription("sits")$Version) )
    sits_log()
}

.onLoad = function(lib, pkg) {
    Sys.setenv(R_CONFIG_ACTIVE = "default")
    Sys.setenv(R_CONFIG_FILE = "config.yml")
    sits_config()
}

# Creates a package environment to store global variables
sits.env <- new.env()
sits.env$model_formula <- "log"

# Include the following global variables in the sits package
utils::globalVariables(c(".", "%>%", ":=", ".SD", ".SDcols", ".N", "V1", "Index", "from", "value", "variable", "cond",
                         "med", "qt25", "qt75", "cluster_id", "x", "y",
                         "longitude", "latitude", "label", "cube", "coverage", "service",
                         "year", "start_date", "end_date", "time_series",
                         "name", "scale_factor", "missing_value",
                         "original_label", "n_members", "n_members.n", "count",
                         "segr", "frac", "total", "n", "rows.lst", "whit",
                         "blue", "red", "green", "nir", "mir", "swir", "ndvi", "evi", "model_formula",
                         "distance", "predicted", "new_values", "sum_area", "area_vec", "area",
                         "Alig.N", "matches", "original_row", "reference", "cluster", ".data"))
#' @importFrom dtw symmetric1 symmetric2
#' @importFrom lubridate %within% %m+%
#' @useDynLib sits, .registration = TRUE
NULL
