library (sits)
library (kohonen)

# load patterns from savanna and pasture (expert = Rodrigo)
cerrado.tb <- readRDS(system.file("extdata/samples/cerrado.rds", package="sits"))

# time_series <- cerrado.tb$time_series
#
# time.df <- data.frame()
#
# for (i in 1:length(time_series)) {
#      col_ndvi <- time_series[[i]]$ndvi
#      row_ndvi <- t(col_ndvi)
#      time.df <- dplyr::bind_rows (time.df, as.data.frame(row_ndvi))
# }

BCD <- "BandsCasesDates"
BDC <- "BandsDatesCases"
CDB <- "CasesDatesBands"

# returns a list of matrices
sits_values <- function(data.tb, bands = NULL, format = "CasesDatesBands"){
     ensurer::ensure_that(format, . == "CasesDatesBands" || . == "BandsCasesDates" || . == "BandsDatesCases",
                          err_desc = "sits_values: valid format parameter are 'CasesDatesBands', 'BandsCasesDates', or 'BandsDatesCases'")

     if (purrr::is_null(bands))
          bands <- sits_bands(data.tb)

     # equivalent to former sits_values_rows()
     # used in sits_cluster input data
     # list elements: bands, matrix's rows: cases, matrix's cols: dates
     if (format == "CasesDatesBands") {
          values.lst <- data.tb$time_series %>%
               purrr::map(function (ts) data.matrix(dplyr::select(ts, dplyr::one_of(bands))))
     # equivalent to former sits_values_cols()
     # list elements: bands, matrix's rows: dates, matrix's cols: cases
     } else if (format == "BandsDatesCases") {
          values.lst <- data.tb$time_series %>%
               data.frame() %>%
               tibble::as_tibble() %>%
               dplyr::select (dplyr::starts_with (band))
     # another kind of sits_values_rows()
     # used in sits_kohonen input
     # list elements: bands, matrix's rows: cases, matrix's cols: dates
     } else {
          values.lst <- bands %>% purrr::map(function (band) {
               cerrado.tb$time_series %>%
                    purrr::map(function (ts) dplyr::select(ts, dplyr::one_of(band))) %>%
                         data.frame() %>%
                         tibble::as_tibble() %>%
                         as.matrix() %>% t()
          })
     }
     return (values.lst)
}


sits_kohonen <- function(data.tb, grid, rlen = 100, alpha = c(0.05, 0.01),
                         radius = quantile(nhbrdist, 0.67),
                         whatmap = NULL, user.weights = 1, maxNA.fraction = 0L,
                         keep.data = TRUE, dist.fcts = NULL,
                         mode = "online", cores = -1, init,
                         normalizeDataLayers = TRUE){

     kohonen <- supersom(data, grid=grid, rlen = rlen, alpha = alpha,
                         radius = radius, whatmap = whatmap, user.weights = user.weights,
                         maxNA.fraction = maxNA.fraction, keep.data = keep.data, dist.fcts = dist.fcts,
                         mode = mode, cores = cores, init = init, normalizeDataLayers = normalizeDataLayers)

}


sits_values(cerrado.tb, format = BCD)
