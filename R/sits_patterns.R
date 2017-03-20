#' Find time series patterns for classification
#' \code{sits_patterns} returns a sits table with a list of patterns
#'
#' A sits table has the metadata and data for each time series
#' <longitude, latitude, start_date, end_date, label, coverage, time_series>
#'
#' This function allows the user to select different alternatives to define a set of
#' patterns, given his samples. The alternatives are:
#' "gam" - uses a generalised additive model to approximate a smooth spline for each pattern
#' "dendogram" - uses a herarchical clustering method to group the patterns
#' "centroids" - uses a positional clustering method to group the patterns
#'
#' @param  samples.tb    a table in SITS format with a set of labelled time series
#' @param  method        the method to be used for classification
#' @return patterns.tb   a SITS table with the patterns
#' @export
sits_patterns <- function (samples.tb, method = "gam", ...) {
     # check the input exists
     ensurer::ensure_that(samples.tb, !purrr::is_null(.), err_desc = "sits_patterns: input data not provided")

     switch(method,
            "gam"            =  { patterns.tb <- sits_patterns_gam (samples.tb, ...) },
            "dendogram"      =  { patterns.tb <- sits_cluster (samples.tb, method = "dendogram", ...)},
            "centroids"      =  { patterns.tb <- sits_cluster (samples.tb, method = "centroids", ...)},
            message (paste ("sits_patterns: valid methods are gam, dendogram, centroids", "\n", sep = ""))
            )

     # return the patterns found in the analysis
     return (patterns.tb)
}

#' Find time series patterns for classification
#'
#' \code{sits_patterns_gam} returns a sits table with a list of patterns based on samples using a gam model
#'
#' A sits table has the metadata and data for each time series
#' <longitude, latitude, start_date, end_date, label, coverage, time_series>
#'
#' The patterns are calculated based on a statistical model that tries to find a suitable
#' for a set of samples. The idea is to use a formula of type y ~ s(x), where x is a temporal
#' reference and y if the value of the signal. For each time, there will be as many predictions
#' as there are sample values. A generalised additive model ("gam") is used to predict a suitable
#' approximation that fits the assumptions of the statistical model.
#'
#' By default, the gam methods  produces an approximation based on a smooth function.
#' This method is based on the "createPatterns" method of the dtwSat package, which is also
#' described in the reference paper:
#'
#' Maus V, Camara G, Cartaxo R, Sanchez A, Ramos FM, de Queiroz GR (2016).
#' A Time-Weighted Dynamic Time Warping Method for Land-Use and Land-Cover Mapping.
#' IEEE Journal of Selected Topics in Applied Earth Observations and Remote Sensing, 9(8):3729-3739,
#' August 2016. ISSN 1939-1404. doi:10.1109/JSTARS.2016.2517118.
#'
#' @param  samples.tb    a table in SITS format with time series to be classified using TWTDW
#' @param  method        the method to be used for classification
#' @param  freq          int - the interval in days for the estimates to be generated
#' @param  from          starting date of the estimate (month-day)
#' @param  to            end data of the estimated (month-day)
#' @param  formula       the formula to be applied in the estimate
#' @return patterns.tb   a SITS table with the patterns
#' @export
#'
#'
sits_patterns_gam <- function (samples.tb, freq = 8, from = NULL, to = NULL, formula = y ~ s(x)){
     # create a tibble to store the results
     patterns.tb <- sits_table()

     # what are the variables in the formula?
     vars <-  all.vars(formula)

     # what are the bands of the data set?
     bands <- sits_bands(samples.tb)

     # how many different labels are there?
     labels <- dplyr::distinct (samples.tb, label)

     # for each label in the sample data, find the appropriate pattern
     for (i in 1:nrow(labels)) {
          # get the label name as a character
          lb <-  as.character (labels[i,1])

          # filter only those rows with the same label
          label.tb <- dplyr::filter (samples.tb, label == lb)

          # if dates are not given, get them from the sample data set
          if (purrr::is_null (from))
               from <- head(label.tb[1,]$time_series[[1]],1)$Index
          else
               from <- from
          if (purrr::is_null (to))
               to   <- tail(label.tb[1,]$time_series[[1]],1)$Index
          else
               to <- to

          # determine the sequence of prediction times
          pred_time = seq(from = lubridate::as_date(from),
                          to   = lubridate::as_date(to),
                          by   = freq)

          # create a data frame to store the time instances
          time <- data.frame(as.numeric(pred_time))

          # name the time as the second variable of the formula (usually, this is x)
          names(time) = vars[2]

          # create a tibble to store the time series associated to the pattern
          res.tb <- tibble::tibble (Index = lubridate::as_date(pred_time))

          # calculate the fit for each band
          for (i in 1:length(bands)) {
               band <-  bands[i]

               # retrieve the time series for each band
               ts <- label.tb %>%
                    sits_select (band) %>%
                    sits_align (from) %>%
                    .$time_series

               # melt the time series for each band into a long table
               # with all values together
               ts2 <- ts %>%
                    reshape2::melt   (id.vars = "Index") %>%
                    dplyr::select    (Index, value)      %>%
                    dplyr::transmute (x = as.numeric(Index), y = value)

               #calculate the best fit for the data set
               fit <-  mgcv::gam(data = ts2, formula = formula)

               # Takes a fitted gam object and produces predictions
               # for the desired dates in the sequence of prediction times
               pred_values <- mgcv::predict.gam(fit, newdata = time)

               #include the predicted values for the band in the results table
               res.tb <- tibble::add_column(res.tb, b = pred_values)

               # rename the columns to match the band names
               # this is workaround because of a bug in standard dplyr::rename
               res.tb <- dplyr::rename_(res.tb, .dots = stats::setNames (names(res.tb), c("Index", bands[1:i])))
          } # for each band

          # put the pattern in a list to store in a sits table
          ts <- tibble::lst()
          ts[[1]] <- res.tb

          # add the pattern to the results table
          patterns.tb <- tibble::add_row (patterns.tb,
                                 longitude    = 0.0,
                                 latitude     = 0.0,
                                 start_date   = as.Date(from),
                                 end_date     = as.Date(to),
                                 label        = lb,
                                 coverage     = label.tb[1,]$coverage,
                                 time_series  = ts)
     } # for each label

     return (patterns.tb)
}

#' Find time series patterns for classification
#'
#' \code{sits_patterns_cluster} returns a sits table with a list of patterns
#' based on a clustering method (either "dendogram" or "centroids")
#'
#' A sits table has the metadata and data for each time series
#' <longitude, latitude, start_date, end_date, label, coverage, time_series>
#'
#' This function uses an algorithm that tries to create a hierarchy
#' of groups in which, as the level in the hierarchy increases, clusters are created by merging
#' the clusters from the next lower level, such that an ordered sequence of groupings is obtained
#' (Hastie et al. 2009). The similarity method used is the "dtw" distance
#'
#' This function uses the dendogram clustering method available in the "dtwclust" pattern
#'
#'
#' @param  samples.tb    a table in SITS format with a set of labelled time series
#' @param  method        the method to be used for classification
#' @return patterns.tb   a SITS table with the patterns
#' @export
sits_patterns_cluster <- function (samples.tb, method = "dendogram", nclusters = 2, perc = 0.10, show = FALSE){
     # create an output
     patterns.tb <- tibble::tibble()

     # how many different labels are there?
     labels <- dplyr::distinct (samples.tb, label)

     for (i in 1:nrow(labels)) {
          # get the label name as a character
          lb <-  as.character (labels[i,1])

          # filter only those rows with the same label
          label.tb <- dplyr::filter (samples.tb, label == lb)
          # apply the clustering method
          if (method == "dendogram")
               clu.tb <- sits_dendogram (label.tb, n_clusters = nclusters, perc = perc, show = show)
          else
               clu.tb <- sits_centroids (label.tb, n_clusters = nclusters, perc = perc, show = show)
          # get the result
          dplyr::bind_rows(patterns.tb, clu.tb)
     }
     return (patterns.tb)
}
