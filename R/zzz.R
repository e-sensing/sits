# On load
.onAttach <- function(lib, pkg) {
    packageStartupMessage("SITS - satellite image time series analysis.")
    packageStartupMessage(
        sprintf(
            "Loaded sits v%s.
        See ?sits for help, citation(\"sits\") for use in publication.
        See demo(package = \"sits\") for examples.",
            utils::packageDescription("sits")$Version
        )
    )
    sits_config_info()
}

.onLoad <- function(lib, pkg) {
    Sys.setenv(R_CONFIG_ACTIVE = "default")
    Sys.setenv(R_CONFIG_FILE = "config.yml")
    # Sys.setenv(R_FUTURE_RNG_ONMISUSE = "ignore")
    Sys.setenv(TF_CPP_MIN_LOG_LEVEL = "2")
    sits_config()
}

# Creates a package environment to store global variables
sits_env <- new.env()
# Uses "log" as the default model formula
sits_env$model_formula <- "log"

# Include the following global variables in the sits package
utils::globalVariables(c(
    ".", "%>%", ":=", ".SD", ".SDcols",
    ".N", "V0", "V1", "Index",
    "from", "value", "variable", "cond",
    "med", "qt25", "qt75", "cluster_id",
    "x", "y", "median", "X", "Y",
    "geometry",
    "longitude", "latitude", "label", "cube", "coverage",
    "type", "year", "start_date", "end_date", "time_series",
    "name", "scale_factor", "missing_value",
    "band", "instance", "maximum_value", "minimum_value", "size",
    "..b", "..band", "bands", "b_box", "tile",
    "original_label", "n_members", "n_members.n", "count",
    "segr", "frac", "total", "n", "rows.lst", "whit",
    "blue", "red", "green", "nir", "mir", "swir", "ndvi", "evi",
    "NDVI", "EVI", "MIR", "NIR", "RED", "model_formula",
    "X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10",
    "month", "day", "res", "path",
    "distance", "predicted", "new_values", "sum_area",
    "area_vec", "area",
    "id_sample", "neuron_label", "percentage_s",
    "id_neuron", "neuron_class", "label_samples",
    "label_neighbor_neuron", "p_Neighbor", "desc",
    "percentage_n", "iteration",
    "samples_label", "label_subgroup", "cluster_name",
    "index_time", "dup_neuron",
    "probability", "total_probability", "radius", "som_label",
    "Alig.N", "matches", "original_row", "reference",
    "cluster", "mixture_percentage", "prior_prob", "post_prob", ".data"
))

#' @importFrom lubridate %within% %m+%
#' @useDynLib sits, .registration = TRUE
NULL
