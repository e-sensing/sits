#' @title Cluster a set of time series using hierarchical clustering
#' @name sits_dendrogram
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Cluster time series in hierarchical mode. Hierarchical clustering, as its name suggests,
#' is an algorithm that tries to create a hierarchy of groups in which, as the level in the hierarchy increases, clusters are created by merging
#' the clusters from the next lower level, such that an ordered sequence of groupings is obtained.
#' The similarity measure used to group time series in a cluster is the dtw metric.
#' The procedure is deterministic, so it will always give the same
#' result for a chosen set of similarity measures (taken from the DTWCLUST package docs).
#'
#' @references `dtwclust` package (https://CRAN.R-project.org/package=dtwclust)
#'
#' @param data.tb         a tibble the list of time series to be clustered
#' @param bands           a vector the bands to be clusterized.
#' @param n_clusters      the number of clusters to be identified. Default is 2.
#' @param dist_method     A supported distance from proxy's dist, e.g. \code{TWDTW}.
#' @param grouping_method the agglomeration method to be used. Any `hclust` method (see `hclust`) Default is 'ward.D2'..
#' @param  ...            any additional parameters to be passed to dtwclust::tsclust() function
#' @return clusters       a clusters obj from dtwclust with the full dendrogram tree for data analysis
sits_dendrogram <- function (data.tb = NULL, bands = NULL,
                            dist_method = "dtw_basic",
                            grouping_method = "ward.D2", ...){

    # if no bands informed, get all bands available in SITS table
    if (purrr::is_null(bands))
        bands <- sits_bands(data.tb)

    # get the values of the time series
    values.tb <- sits_values (tb, bands, format = "cases_dates_bands")

    # call dtwclust and get the resulting clusters
    clusters  <- dtwclust::tsclust (values.tb,
                                    type     = "hierarchical",
                                    k        = NROW (data.tb),
                                    distance = dist_method,
                                    control  = dtwclust::hierarchical_control(method = grouping_method), ...)


    # return the clusters
    return (clusters)
}
