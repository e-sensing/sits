#' @title Initialization commands for SITS package
#' @name sits_init
#' @description provides the information for the dependencies of the sits package
#'

#' @title .onAttach
#' @param lib libname argument
#' @param  pkg pkgname argument

.onAttach = function(lib, pkg){
     packageStartupMessage(
          sprintf("Loaded sits v%s. See ?sits for help, citation(\"sits\") for use in publication.\n",
                  utils::packageDescription("sits")$Version) )
}
# Include the following global variables in the SITS package
#
utils::globalVariables(c(".", "%>%", "Index", "value", "variable", "cond",
                         "rmean", "std", "stdplus", "stdminus", "cluster_id", "cross_join",
                         "longitude", "latitude", "label", "coverage",
                         "year", "start_date", "end_date", "time_series",
                         "name", "scale_factor", "missing_value",
                         "original_label", "n_members", "n_members.n", "count",
                         "segr", "frac", "total", "n",
                         "distance", "twdtw_distances", "predicted", "new_values", "Alig.N"))

#  define the dependencies of the SITS package
#' @import lattice
#' @import dplyr
#' @import dtwclust
#' @import dtwSat
#' @import magrittr
#' @import wtss
#' @useDynLib sits, .registration = TRUE
#'
