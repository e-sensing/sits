# On load
.onAttach <- function(lib, pkg){
    packageStartupMessage("sits - satellite image time series analysis.")
    packageStartupMessage(
        sprintf("Loaded sits v%s.
        See ?sits for help, citation(\"sits\") for use in publication.
        See demo(package = \"sits\") for examples.",
                utils::packageDescription("sits")$Version) )
}

.onLoad <- function(lib, pkg) {
    Sys.setenv(R_CONFIG_ACTIVE = "default")
    Sys.setenv(R_CONFIG_FILE = "config.yml")
    sits_config()
    sits_log()
}

# Creates a package environment to store global variables
sits.env <- new.env()
sits.env$model_formula <- "log"

# Include the following global variables in the sits package
utils::globalVariables(c(".", "%>%", ":=", ".SD", ".SDcols", ".N", "V0", "V1", "Index",
                         "from", "value", "variable", "cond",
                         "med", "qt25", "qt75", "cluster_id", "x", "y", "median",
                         "longitude", "latitude", "label", "cube", "coverage",
                         "service","year", "start_date", "end_date", "time_series",
                         "name", "scale_factor", "missing_value",
                         "original_label", "n_members", "n_members.n", "count",
                         "segr", "frac", "total", "n", "rows.lst", "whit",
                         "blue", "red", "green", "nir", "mir", "swir",
                         "ndvi", "evi", "model_formula",
                         "distance", "predicted", "new_values", "sum_area",
                         "area_vec", "area",
                         "id_sample", "neuron_label", "percentage_s",
                         "id_neuron", "neuron_class",
                         "label_neighbor_neuron", "p_Neighbor", "desc",
                         "percentage_n", "iteration",
                         "samples_label", "label_subgroup", "cluster_name",
                         "index_time", "dup_neuron",
                         "probability", "total_probability", "radius", "som_label",
                         "Alig.N", "matches", "original_row", "reference",
                         "cluster", ".data"))
#' @importFrom dtw symmetric1 symmetric2
#' @importFrom lubridate %within% %m+%
#' @useDynLib sits, .registration = TRUE
NULL
