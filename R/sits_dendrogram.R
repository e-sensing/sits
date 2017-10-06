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
#' @param dist_method     A supported distance from proxy's dist, e.g. \code{TWDTW}.
#' @param grouping_method the agglomeration method to be used. Any `hclust` method (see `hclust`) Default is 'ward.D2'..
#' @param  ...            any additional parameters to be passed to dtwclust::tsclust() function
#' @return clusters       a clusters obj from dtwclust with the full dendrogram tree for data analysis
#' @export
sits_dendrogram <- function (data.tb, bands = NULL,
                            dist_method = "dtw_basic",
                            grouping_method = "ward.D2", ...){

    # does the input data exist?
    .sits_test_tibble(data.tb)

    # if no bands informed, get all bands available in SITS tibble
    if (purrr::is_null(bands))
        bands <- sits_bands(data.tb)

    # get the values of the time series
    values.tb <- sits_values (data.tb, bands, format = "cases_dates_bands")

    # call dtwclust and get the resulting clusters
    clusters  <- dtwclust::tsclust (values.tb,
                                    type     = "hierarchical",
                                    k        = NROW (data.tb),
                                    distance = dist_method,
                                    control  = dtwclust::hierarchical_control(method = grouping_method), ...)

    # return the clusters
    return (clusters)
}

#' @title Create time series patterns for classification
#' @name sits_patt_dendgam
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Michelle Picoli, \email{mipicoli@@gmail.com}
#'
#'
#' @description This function ...
#'
#' @param data.tb                 a SITS tibble time series with an alignment column
#' @param first_cluster_cleaner   first_cluster_cleaner
#' @param cluster_a_labels        cluster_a_labels
#' @param second_cluster_cleaner  second_cluster_cleaner
#' @param k_cluster_a             k_cluster_a
#' @param k_cluster_b             k_cluster_b
#' @return result                 result
#' @export
#'
sits_patt_dendgam <- function(data.tb = NULL, first_cluster_cleaner = 0.027,
                              cluster_a_labels = c("Forest", "Cerrado", "Pasture"),
                              second_cluster_cleaner = 0.035, k_cluster_a = 8, k_cluster_b = 6){

    result_fun <- function(tb){

        # does the input data exist?
        .sits_test_tibble (tb)

        # first cluster cutree and cleaner
        data_clus.obj <- sits_dendrogram(tb, window.size = 7, step.pattern = symmetric1)
        cluster.tb <- sits_cluster(tb, data_clus.obj, k = 2)
        cluster.tb <- sits_cluster_cleaner(cluster.tb, min_clu_perc = first_cluster_cleaner)

        # cluster partitioning
        frequency.tb <- sits_cluster_frequency(cluster.tb, relative = TRUE)
        if (any(row.names(frequency.tb)[which.max(frequency.tb[1:(NROW(frequency.tb) - 1), 1])] %in% cluster_a_labels)){
            data_a.tb <- dplyr::filter(cluster.tb, cluster == 1)
            data_b.tb <- dplyr::filter(cluster.tb, cluster == 2)
        } else {
            data_a.tb <- dplyr::filter(cluster.tb, cluster == 2)
            data_b.tb <- dplyr::filter(cluster.tb, cluster == 1)
        }

        # second cluster cutree and cleaner
        # cluster_a
        data_clus_a.obj <- sits_dendrogram(data_a.tb, window.size = 7, step.pattern = symmetric1)
        cluster_a.tb <- sits_cluster(data_a.tb, data_clus_a.obj, k = k_cluster_a)
        cluster_a.tb <- sits_cluster_cleaner(cluster_a.tb, min_clu_perc = second_cluster_cleaner)
        # cluster_b
        data_clus_b.obj <- sits_dendrogram(data_b.tb, window.size = 7, step.pattern = symmetric1)
        cluster_b.tb <- sits_cluster(data_b.tb, data_clus_b.obj, k = k_cluster_b)
        cluster_b.tb <- sits_cluster_cleaner(cluster_b.tb, min_clu_perc = second_cluster_cleaner)

        # bind cluster_a and cluster_b
        combined.tb <- dplyr::bind_rows(cluster_a.tb, cluster_b.tb)

        # generate pattern with gam
        result.tb <- sits_gam(combined.tb)
        return(result.tb)
    }

    result.tb <- .sits_factory_function (data.tb, result_fun)
    return(result.tb)
}
