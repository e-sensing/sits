# On load
.onAttach = function(lib, pkg){
    packageStartupMessage("Satellite Image Time Series package.")
    packageStartupMessage(
        sprintf("Loaded sits v%s. See ?sits for help, citation(\"sits\") for use in publication.\n",
                utils::packageDescription("sits")$Version) )
    packageStartupMessage("Registering TWDTW distance from `dtwSat` into `proxy::pr_DB`.\n")
    dtwSat::twdtwProxyRegister()
}

# Include the following global variables in the SITS package
utils::globalVariables(c(".", "%>%", "Index", "from", "value", "variable", "cond",
                         "med", "qt25", "qt75", "cluster_id",
                         "longitude", "latitude", "label", "coverage",
                         "year", "start_date", "end_date", "time_series",
                         "name", "scale_factor", "missing_value",
                         "original_label", "n_members", "n_members.n", "count",
                         "segr", "frac", "total", "n",
                         "distance", "predicted", "new_values",
                         "Alig.N", "matches", "reference", "cluster", ".data"))
#' @import dtwSat
#' @import dtwclust
#' @importFrom dtw symmetric1 symmetric2
#' @importFrom lubridate %within% %m+%
#' @useDynLib sits, .registration = TRUE
NULL
