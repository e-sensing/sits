# -----------------------------------------------------------
#' Cluster a set of time series using hierarchical clustering
#'
#' \code{sits_dendogram} cluster time series in hierarchical mode
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
#' @family   STIS main functions
#' @export
#' @examples sits_dendogram  (data.tb, n_clusters = 6)
#'
#'
sits_dendogram <- function (data.tb, n_clusters = 6) {

     cluster_dendogram <- function (data.tb, band, n_clusters){
          # get the values of the various time series for this band
          values.tb <- sits_values (data = data.tb, band = band)
          clusters  <- dtwclust (values.tb,
                                 type     = "hierarchical",
                                 k        = n_clusters,
                                 distance = "dtw_basic",
                                 seed     = 899)

          # By default, the dendrogram is plotted in hierarchical clustering
          # plot(clusters)
          # The series and the obtained prototypes can be plotted too
          plot (clusters, type = "sc")

          # Focusing on the first cluster
          # plot(clusters, type = "series", clus = 1L)
          plot(clusters, type = "centroids")

          #create a list of each cluster
          #cl <- lapply (unique (clusters@cluster), function (clu) data.tb[clusters@cluster == clu,] )
          return (clusters)
     }

     cluster.lst <- data.tb %>%
          sits_bands() %>%
          purrr::map (function (b) cluster_dendogram (data.tb, b, n_clusters))

     return (cluster.lst)
}
