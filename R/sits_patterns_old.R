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

