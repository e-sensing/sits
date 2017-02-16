# -----------------------------------------------------------
#' Cluster a set of time series using partitional clustering (uses dtwclust package)
#'
#' \code{sits_centroids} find the k centroids of a set of time series clusters
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
#' or no object changes clusters anymore (extracted from dtwclust package)
#'
#' @param data.tb      a tibble the list of time series to be clustered
#' @param n_clusters   the number of clusters to be identified
#' @return          none
#' @keywords STIS
#' @family   STIS main functions
#' @examples sits_plot  (data.tb, n_clusters = 6)
#' @export
#'
sits_centroids <- function (data.tb, n_clusters = 6) {

     cluster_partitional <- function (band, data.tb, n_clusters) {
          values.tb <- sits_values_rows (data.tb, band)
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
     data.tb %>%
          sits_bands() %>%
          purrr::map (function (b) cluster_partitional (b, data.tb, n_clusters))
}
