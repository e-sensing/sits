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
#' @param  bands         string - the bands to be used for classification
#' @param  method        the method to be used for classification
#' @return patterns.tb   a SITS table with the patterns
#' @export
#'
#'
sits_patterns <- function (samples.tb, bands, method = "gam"){

     # dtwSat -Create temporal patterns
     # patt = createPatterns(x=ts, from="2005-09-01", to="2006-09-01", freq=8, formula = y~s(x))
     #
     # Get formula variables
     # vars = all.vars(formula)

     # Shift dates to match the same period

     #dates = as.Date(df[[vars[2]]])
     #pred_time = seq(from, to, freq)

     # fun = function(y, ...){
     #      df = data.frame(y, as.numeric(dates))
     #      names(df) = vars
     #      fit = gam(data = df, formula = formula, ...)
     #      time = data.frame(as.numeric(pred_time))
     #      names(time) = vars[2]
     #      predict.gam(fit, newdata = time)
     # }
     #
     # if(is.null(attr)) attr = names(df)[-which(names(df) %in% vars[2])]
     #
     # res = sapply(as.list(df[attr]), FUN=fun, ...)
     # zoo(data.frame(res), as.Date(pred_time))
}
