# -----------------------------------------------------------
#' Cluster a set of time series
#'
#' uses the dtwclust package to cluster time series. s
#' See "dtwclust" documentation for more details
#'
#' @param data.tb      a tibble the list of time series to be clustered
#' @param n_clusters   the number of clusters to be identified
#' @return             none
sits_cluster <- function (data.tb, type = "partitional", n_clusters = 6){
     if (type != "partitional" && type != "hierarchical") {
          message (paste ("sits_cluster: valid cluster methods are partitional or hierarchical", "\n", sep = ""))
     }

     # plot all points joined in time
     if (type == "partitional") {
          sits_cluster_partitional(data.tb, n_clusters)
     }else if (type == "hierarchical") {
          sits_cluster_hierarchical(data.tb, n_clusters)
     }
}

# -----------------------------------------------------------
#' Cluster a set of time series using hierarchical clustering
#'
#' \code{sits_cluster_hierarchical} cluster time series in hierarchical mode
#'
#' Hierarchical clustering, as its name suggests, is an algorithm that tries to create a hierarchy
#' of groups in which, as the level in the hierarchy increases, clusters are created by merging
#' the clusters from the next lower level, such that an ordered sequence of groupings is obtained
#' (Hastie et al. 2009). In order to decide how the merging is performed, a similarity measure
#' between groups should be specied, in addition to the one that is used to calculate pairwise
#' similarities. However, a specific number of clusters does not need to be specified for the
#' hierarchy to be created, and the procedure is deterministic, so it will always give the same
#' result for a chosen set of similarity measures (taken from the DTWCLUST package docs).
#'
#' Ref: Comparing Time-Series Clustering Algorithms in R Using the dtwclust Package
#' (dtwclust vignette)
#'
#' @param data.tb      a tibble the list of time series to be clustered
#' @param n_clusters   the number of clusters to be identified
#' @return             none
#' @keywords STIS
#' @family   STIS auxiliary functions
#' @examples sits_plot  (ts, "Set1")
#'
#'
sits_cluster_hierarchical <- function (data.tb, n_clusters = 6) {

     cluster_dendogram <- function (data.tb, band, n_clusters){
          values.tb <- sits_tibble_values_only (data.tb, band)
          clusters  <- dtwclust (values.tb,
                                 type     = "hierarchical",
                                 k        = n_clusters,
                                 distance = "dtw_basic",
                                 seed     = 899)

          # By default, the dendrogram is plotted in hierarchical clustering
          plot(clusters)
          # The series and the obtained prototypes can be plotted too
          plot (clusters, type = "sc")

          # Focusing on the first cluster
          plot(clusters, type = "series", clus = 1L)
          plot(clusters, type = "centroids", clus = 1L)

          #create a list of each cluster
          cl <- lapply (unique (clusters@cluster), function (clu) data.tb[clusters@cluster == clu,] )
     }

     bands  <- sits_band_names (data.tb[1,]$time_series)

     for (band in bands) cluster_dendogram (data.tb, band, n_clusters)
}

# -----------------------------------------------------------
#' Cluster a set of time series using partitional clustering
#'
#' \code{sits_cluster_partitional} plot one time series for a single place and interval
#'
#' Partitional clustering assigns the data
#' to one and only one cluster out of k total clusters. The total number of
#' desired clusters must be specified beforehand.
#' Partitional clustering algorithms commonly work in the following way.
#' First, k centroids are randomly initialized, usually by choosing k objects
#' from the dataset at random. these are assigned to individual clusters.
#' The distance between all objects in the data and all centroids
#' is calculated, and each object is assigned to the cluster of its closest centroid.
#' A prototyping function is applied to each cluster to update the corresponding centroid.
#' Then, distances and centroids are updated iteratively until a certain number of iterations have elapsed,
#' or no object changes clusters anymore.
#'
#' @param data.tb      a tibble the list of time series to be clustered
#' @param n_clusters   the number of clusters to be identified
#' @return          none
#' @keywords STIS
#' @family   STIS auxiliary functions
#' @examples sits_plot  (ts, "Set1")
#'
#'
sits_cluster_partitional <- function (data.tb, n_clusters = 6) {

     cluster_partitional <- function (data.tb, band, n_clusters) {
          values.tb <- sits_tibble_values_only (data.tb, band)
          clusters  <- dtwclust (values.tb,
                                 type     = "partitional",
                                 k        = n_clusters,
                                 distance = "dtw_basic",
                                 centroid = "pam",
                                 seed     = 899)

          # By default, the dendrogram is plotted in hierarchical clustering
          plot(clusters)
          # The series and the obtained prototypes can be plotted too
          plot (clusters, type = "sc")

          # Focusing on the first cluster
          plot(clusters, type = "series", clus = 1L)
          plot(clusters, type = "centroids", clus = 1L)

          #create a list of each cluster
          cl <- lapply (unique (clusters@cluster), function (clu) data.tb[clusters@cluster == clu,] )
     }

     bands  <- sits_band_names (data.tb[1,]$time_series)

     for (band in bands) cluster_partitional (data.tb, band, n_clusters)
}
