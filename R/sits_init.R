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
                         "original_label", "n_members", "count",
                         "segr", "frac", "total", "n"))

#  define the dependencies of the SITS package
#' @importFrom ensurer ensure_that ensures_that ensure check check_that
#' @importFrom entropy entropy
#' @importFrom ggplot2 ggplot aes geom_line labs scale_color_brewer scale_colour_hue
#' @importFrom grDevices rgb
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom kohonen supersom somgrid
#' @importFrom lubridate as_date dyears days period yday year ymd
#' @importFrom methods new as
#' @importFrom mgcv gam predict.gam s
#' @importFrom purrr map map2 map_df is_null
#' @importFrom purrrlyr by_row
#' @importFrom parallel mcMap
#' @importFrom ptw whit2
#' @importFrom readr cols col_integer col_double col_date col_character read_csv write_lines
#' @importFrom raster shapefile
#' @importFrom reshape2 melt
#' @importFrom signal sgolayfilt
#' @importFrom sp bbox SpatialPoints CRS proj4string
#' @importFrom stats sd setNames
#' @importFrom stringr str_extract str_detect
#' @importFrom tibble tibble as_tibble add_column add_row lst
#' @importFrom tidyr nest unnest drop_na
#' @importFrom tools file_ext file_path_sans_ext
#' @importFrom utils head tail
#' @importFrom wtss WTSS timeSeries listCoverages describeCoverage
#' @importFrom zoo zoo fortify.zoo
#' @import dendextend
#' @import dplyr
#' @import dtwclust
#' @import dtwSat
#' @import magrittr
#' @import Rcpp
#' @useDynLib sits, .registration = TRUE
#' 
NULL
