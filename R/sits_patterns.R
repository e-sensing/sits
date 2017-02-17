#' Find time series patterns to use using TWDTW (using the dtwSat package)
#'
#' \code{sits_patterns} returns a sits table with a list of patterns based on samples
#'
#' A sits table has the metadata and data for each time series
#' <longitude, latitude, start_date, end_date, label, coverage, time_series>
#'
#' The patterns are calculated based on a statistical model that tries to find a suitable
#' for a set of samples
#'
#' This method is based on the "createPatterns" method of the dtwSat package
#'
#' Reference: Maus V, Camara G, Cartaxo R, Sanchez A, Ramos FM, de Queiroz GR (2016).
#' “A Time-Weighted Dynamic Time Warping Method for Land-Use and Land-Cover Mapping.” IEEE
#'  Journal of Selected Topics in Applied Earth Observations and Remote Sensing, 9(8):3729-3739,
#'  August 2016. ISSN 1939-1404. doi:10.1109/JSTARS.2016.2517118.
#'
#' @param  samples.tb    a table in SITS format with time series to be classified using TWTDW
#' @param  method        the method to be used for classification
#' @return patterns.tb   a SITS table with the patterns
#' @export
#'
#'
sits_patterns <- function (samples.tb, method = "gam",){

     start_date <- samples.tb[1,]$start_date
     end_date   <- samples.tb[1,]$end_date
     freq <- 8
     formula <- y ~ s(x)
     # Get formula variables
     vars <-  all.vars(formula)

     bands <- sits_bands(samples.tb)

     bands %>%
          map (function (b) {
               ts <- samples.tb %>%
                    sits_select (b) %>%
                    sits_align (start_date) %>%
                    .$time_series

               ts2 <- ts %>%
                    reshape2::melt (id.vars = "Index") %>%
                    dplyr::select (Index, value) %>%
                    dplyr::transmute (x = Index, y = value) %>%
                    dplyr::transmute (x = as.numeric(x), y)

               fit <-  gam(data = ts2, formula = formula)

               pred_time = seq(from = as.Date(start_date), to = as.Date(end_date), by = freq)

               time <- data.frame(as.numeric(pred_time))
               names(time) = vars[2]

               pat <- predict.gam(fit, newdata = time)

          })

}
