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
#' @importFrom snow sendCall recvOneData clusterExport
#' @importFrom raster shapefile brick beginCluster endCluster getCluster returnCluster blockSize trim canProcessInMemory rasterTmpFile getValues pbCreate pbStep pbClose writeStart writeValues setValues writeStop
#' @importFrom kohonen supersom somgrid
#' @importFrom ensurer ensure_that ensure check check_that
#' @importFrom entropy entropy
#' @importFrom dplyr bind_rows contains distinct do filter inner_join left_join matches mutate num_range one_of rename rename_ rowwise select starts_with transmute if_else
#' @importFrom readr cols col_integer col_double col_date col_character read_csv write_lines
#' @importFrom tibble tibble as_tibble add_column add_row lst
#' @importFrom tidyr nest unnest drop_na
#' @importFrom purrr map map2 map_df is_null
#' @importFrom purrrlyr by_row
#' @importFrom ggplot2 ggplot aes geom_line labs scale_color_brewer scale_colour_hue
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom lubridate as_date dyears days period yday year ymd
#' @importFrom methods new as
#' @importFrom mgcv gam predict.gam s
#' @importFrom parallel mcMap
#' @importFrom ptw whit2
#' @importFrom reshape2 melt
#' @importFrom rfUtilities accuracy
#' @importFrom signal sgolayfilt
#' @importFrom sp bbox SpatialPoints CRS proj4string
#' @importFrom stats sd setNames predict as.formula 
#' @importFrom stringr str_extract str_detect
#' @importFrom tools file_ext file_path_sans_ext
#' @importFrom utils head tail
#' @importFrom wtss WTSS timeSeries listCoverages describeCoverage
#' @importFrom zoo zoo fortify.zoo
#' @importFrom nnet multinom class.ind 
#' @importFrom randomForest randomForest
#' @importFrom gbm gbm
#' @importFrom nnet multinom
#' @importFrom glmnet cv.glmnet 
#' @importFrom MASS lda
#' @importFrom e1071 svm 
#' @importFrom caret createDataPartition 
#' @import magrittr
#' @import dtwclust
#' @import dtwSat
#' @import Rcpp
#' @useDynLib sits, .registration = TRUE
#' 
NULL
