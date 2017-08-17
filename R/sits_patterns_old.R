#' @title Create time series patterns for classification
#' @name sits_patterns
#' @author Victor Maus, \email{vwmaus1@@gmail.com}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description This function allows the user to select different alternatives to define a set of
#' patterns, given his samples. The alternatives are:
#' "gam" - uses a generalised additive model to approximate a smooth spline for each pattern
#' "dendogram" - uses a herarchical clustering method to group the patterns
#' "centroids" - uses a positional clustering method to group the patterns
#'
#' @param data.tb         a table in SITS format with a set of labelled time series.
#' @param method          the method to be used for classification.
#' @param bands           the bands used to obtain the pattern.
#' @param from            starting date of the estimate in month-day.
#' @param to              end data of the estimated in month-day.
#' @param freq            int - the interval in days for the estimates to be generated.
#' @param formula         the formula to be applied in the estimate (for "gam" method).
#' @param n_clusters      the maximum number of clusters to be identified (for clustering methods).
#' @param grouping_method the agglomeration method to be used. Any `hclust` method (see `hclust`) (ignored in `gam` and `kohonen` methods). Default is 'ward.D2'.
#' @param min_clu_perc    the minimum percentagem of valid cluster members, with reference to the total number of samples (for clustering methods).
#' @param apply_gam       apply gam method after a clustering algorithm (ignored if method is `gam`).
#' @param koh_xgrid       x dimension of the SOM grid (used only in `kohonen` or `kohonen-dendogram` methods). Defaul is 5.
#' @param koh_ygrid       y dimension of the SOM grid (used only in `kohonen` or `kohonen-dendogram` methods). Defaul is 5.
#' @param koh_rlen        the number of times the complete data set will be presented to the SOM grid.
#' (used only in `kohonen` or `kohonen-dendogram` methods). Default is 100.
#' @param koh_alpha       learning rate, a vector of two numbers indicating the amount of change.
#' Default is to decline linearly from 0.05 to 0.01 over rlen updates.
#' @param unsupervised    if TRUE, proceeds an unsupervised cluster followed by a relabel taking original label majority (
#' this option has not any effect if method == "gam")
#' @param show            show the results of the clustering algorithm? (for clustering methods).
#' @param ...             additional arguments to be passed to the method function.
#' @return patterns.tb    a SITS table with the patterns.
#' @export
sits_patterns <- function (data.tb = NULL, method = "gam", bands = NULL, from = NULL, to = NULL, freq = 8,
                           formula = y ~ s(x), n_clusters = 2, grouping_method = "ward.D2", min_clu_perc = 0.10, apply_gam = FALSE,
                           koh_xgrid = 5, koh_ygrid = 5, koh_rlen = 100, koh_alpha = c(0.05, 0.01), unsupervised = FALSE, show = FALSE, ...) {
     # check the input exists
     ensurer::ensure_that(data.tb, !purrr::is_null(.),
                          err_desc = "sits_patterns: input data not provided")

     # check valid methods
     ensurer::ensure_that(method, (. == "gam" || . == "dendogram" || . == "centroids" || . == "kohonen" || . == "kohonen-dendogram"),
                          err_desc = "sits_patterns: valid methods are 'gam', 'dendogram', 'centroids', 'kohonen', or 'kohonen-dendogram'.")

     # check valid min_clu_perc
     ensurer::ensure_that(min_clu_perc, . >= 0.0 && . <= 1.0,
                          err_desc = "sits_patterns: invalid min_clust_perc value. Value must be between 0 and 1.")

     if (purrr::is_null (bands)) bands <- sits_bands(data.tb)

     # # prune the samples to remove all samples greater than 365 days
     # samples.tb <- sits_prune(samples.tb)

     # align all samples to the same time series intervals
     sample_dates <- sits_dates (data.tb[1,])
     data.tb      <- sits_align (data.tb, sample_dates)

     # if "from" and "to" are not given, extract them from the data samples
     if (purrr::is_null (from) || purrr::is_null (to)) {
          from <- lubridate::as_date(utils::head(sample_dates, n = 1))
          to   <- lubridate::as_date(utils::tail(sample_dates, n = 1))
     }

     switch(method,
            "gam"            =  { patterns.tb <- sits_patterns_gam (data.tb = data.tb, bands = bands, from = from, to = to, freq = freq, formula = formula, ...) },
            "dendogram"      =  {
                 patterns.tb <- sits_cluster (data.tb, bands = bands, method = "dendogram",
                                              n_clusters = n_clusters, grouping_method = grouping_method,
                                              return_members = apply_gam, unsupervised = unsupervised, show = show, ... = ...)
            },
            "centroids"      =  {
                 patterns.tb <- sits_cluster (data.tb, bands = bands, method = "centroids",
                                              n_clusters = n_clusters, grouping_method = grouping_method,
                                              return_members = apply_gam, unsupervised = unsupervised, show = show, ... = ...)
                 },
            "kohonen"      =  {
                 patterns.tb <- sits_cluster (data.tb, bands = bands, method = "kohonen",
                                              koh_xgrid = koh_xgrid, koh_ygrid = koh_ygrid, koh_rlen = koh_rlen, koh_alpha = koh_alpha,
                                              return_members = apply_gam, unsupervised = unsupervised, show = show, ... = ...)
            },
            "kohonen-dendogram"      =  {
                 patterns.tb <- sits_cluster (data.tb, bands = bands, method = "kohonen-dendogram",
                                              n_clusters = n_clusters, grouping_method = grouping_method,
                                              koh_xgrid = koh_xgrid, koh_ygrid = koh_ygrid, koh_rlen = koh_rlen, koh_alpha = koh_alpha,
                                              return_members = apply_gam, unsupervised = unsupervised, show = show, ... = ...)
            })

     # get only the significant clusters
     if (method != "gam"){
          if (!unsupervised)
               patterns.tb <- sits_significant_labels(patterns.tb, min_label_frac = min_clu_perc)
     }

     if (apply_gam) {
          # get cluster information before calling GAM...
          pat_labels.tb <- sits_labels(patterns.tb)
          # extract only significant clusters (cut line given by min_clu_perc parameter)
          patterns.tb <- sits_patterns_gam (patterns.tb, bands = bands, from = from, to = to, freq = freq, formula = formula, ... = ...)
          # append cluster informations to the result
          patterns.tb <- dplyr::inner_join(pat_labels.tb, patterns.tb, by = "label") %>%
               dplyr::select(longitude, latitude, start_date, end_date, label, coverage, time_series, original_label, n_members = count)
     }

     # return the patterns found in the analysis
     return (patterns.tb)
}
#' @title Create temporal patterns using a generalised additive model (gam)
#' @name sits_patterns_gam
#' @author Victor Maus, \email{vwmaus1@@gmail.com}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function takes a set of time series samples as input
#' estimates a set of patterns. The patterns are calculated based in a GAM model.
#' The idea is to use a formula of type y ~ s(x), where x is a temporal
#' reference and y if the value of the signal. For each time, there will be as many predictions
#' as there are sample values. The GAM model predicts a suitable
#' approximation that fits the assumptions of the statistical model.
#' By default, the gam methods  produces an approximation based on a smooth function.
#'
#' This method is based on the "createPatterns" method of the dtwSat package, which is also
#' described in the reference paper.
#'
#' @references Maus V, Camara G, Cartaxo R, Sanchez A, Ramos FM, de Queiroz GR (2016).
#' A Time-Weighted Dynamic Time Warping Method for Land-Use and Land-Cover Mapping.
#' IEEE Journal of Selected Topics in Applied Earth Observations and Remote Sensing, 9(8):3729-3739,
#' August 2016. ISSN 1939-1404. doi:10.1109/JSTARS.2016.2517118.
#'
#' @param  data.tb       a table in SITS format with time series to be classified using TWTDW
#' @param  bands         the bands used to obtain the pattern
#' @param  from          starting date of the estimate (month-day)
#' @param  to            end data of the estimated (month-day)
#' @param  freq          int - the interval in days for the estimates to be generated
#' @param  formula       the formula to be applied in the estimate
#' @param  ...           any additional parameters
#' @return patterns.tb   a SITS table with the patterns
#'
sits_patterns_gam <- function (data.tb = NULL, bands = NULL, from = NULL, to = NULL, freq = 8, formula = y ~ s(x), ...){

    # handle the case of null bands
    if (purrr::is_null (bands)) bands <- sits_bands(data.tb)

    # create a tibble to store the results
    patterns.tb <- sits_table()

    # what are the variables in the formula?
    vars <-  all.vars(formula)

    # align all samples to the same time series intervals
    sample_dates <- sits_dates (data.tb[1,])
    data.tb      <- sits_align (data.tb, sample_dates)

    # if "from" and "to" are not given, extract them from the data samples
    if (purrr::is_null (from) || purrr::is_null (to)) {
        from <- lubridate::as_date(utils::head(sample_dates, n = 1))
        to   <- lubridate::as_date(utils::tail(sample_dates, n = 1))
    }

    # determine the sequence of prediction times
    pred_time = seq(from = lubridate::as_date(from),
                     to   = lubridate::as_date(to),
                     by   = freq)

     # how many different labels are there?
     labels <- dplyr::distinct (data.tb, label)$label

     #
     message("Applying GAM...")

     # add a progress bar
     i <- 0
     progress_bar <- utils::txtProgressBar(min = 0, max = length(labels) * length(bands), style = 3)

     # traverse labels
     labels %>%
          purrr::map(function (lb){

               # filter only those rows with the same label
               label.tb <- dplyr::filter (data.tb, label == lb)

               # create a data frame to store the time instances
               time <- data.frame(as.numeric(pred_time))

               # name the time as the second variable of the formula (usually, this is x)
               names(time) = vars[2]

               # create a tibble to store the time series associated to the pattern
               res.tb <- tibble::tibble (Index = lubridate::as_date(pred_time))

               # calculate the fit for each band
               bands %>%
                    purrr::map(function (band) {

                         # retrieve the time series for each band
                         ts <- label.tb %>%
                              sits_select (band) %>%
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

                         # rename the column to match the band names
                         names(res.tb)[names(res.tb) == "b"] <- band
                         # return the value out of the function scope
                         res.tb <<- res.tb

                         # update progress bar
                         i <<- i + 1
                         utils::setTxtProgressBar(progress_bar, i)
                    }) # for each band

               # put the pattern in a list to store in a sits table
               ts <- tibble::lst()
               ts[[1]] <- res.tb

               # add the pattern to the results table
               patterns.tb <<- tibble::add_row (patterns.tb,
                                                longitude      = 0.0,
                                                latitude       = 0.0,
                                                start_date     = as.Date(from),
                                                end_date       = as.Date(to),
                                                label          = lb,
                                                coverage       = label.tb[1,]$coverage,
                                                time_series    = ts)
          })

     close(progress_bar)
     return (patterns.tb)
}
