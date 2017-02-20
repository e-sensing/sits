# -----------------------------------------------------------
#' Cluster a set of time series
#'
#' uses the dtwclust package to cluster time series. s
#' See "dtwclust" documentation for more details
#'
#' @param data.tb      a tibble the list of time series to be clustered
#' @param n_clusters   the number of clusters to be identified
#' @return             none
#' @export
sits_cluster <- function (data.tb, type = "dendogram", n_clusters = 4){
     # select cluster option
     switch (type,
             "dendogram" = { sits_dendogram (data.tb, n_clusters) },
             "centroids" = { sits_centroids (data.tb, n_clusters) },
             message (paste ("sits_cluster: valid cluster methods are dendogram or centroids",
                             "\n", sep = ""))
     )
}

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
sits_dendogram <- function (data.tb, n_clusters = 4) {

     cluster_dendogram <- function (data.tb, band, n_clusters){
          # get the values of the various time series for this band
          values.tb <- sits_values_rows (data = data.tb, band = band)
          clusters  <- dtwclust (values.tb,
                                 type     = "hierarchical",
                                 k        = n_clusters,
                                 distance = "dtw_basic",
                                 seed     = 899)

          # Plot the series and the obtained prototypes
          plot (clusters, type = "sc")

          # Plot the centroids
          plot(clusters, type = "centroids")

          # print information about the clusters
          .sits_cluster_info (clusters)

          return (clusters)
     }

     cluster.lst <- data.tb %>%
          sits_bands() %>%
          purrr::map (function (b) cluster_dendogram (data.tb, b, n_clusters))

     cluster.tb <- .sits_fromClusters (data.tb, cluster.lst)
     return (cluster.tb)
}

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
sits_centroids <- function (data.tb, n_clusters = 4) {

     cluster_partitional <- function (band, data.tb, n_clusters) {
          values.tb <- sits_values_rows (data.tb, band)
          clusters  <- dtwclust (values.tb,
                                 type     = "partitional",
                                 k        = n_clusters,
                                 distance = "dtw_basic",
                                 centroid = "pam",
                                 seed     = 899)

          # Plot the series and the obtained prototypes
          plot (clusters, type = "sc")
          # Plot the centroids
          plot(clusters, type = "centroids")
          # print information about the clusters
          .sits_cluster_info (clusters)

          return (clusters)
     }
     # for each band, find a significant set of clusters
     clusters.lst <- data.tb %>%
          sits_bands() %>%
          purrr::map (function (b) cluster_partitional (b, data.tb, n_clusters))

     # retrieve the significant clusters as a list
     cluster.tb <- .sits_fromClusters (data.tb, clusters.lst)

     return (cluster.tb)
}

#------------------------------------------------------------------
#' Returns a list of centroids and its metadata.
#'
#' \code{.sits_fromClusters} reads a list of clusters provided by the dtwclust
#' package and produces a sits table.
#'
#' A sits table has the metadata and data for each time series centroids
#' <longitude, latitude, start_date, end_date, label, coverage, time_series>
#'
#'
#' @param data.tb   a tibble with input data of dtwclust.
#' @param clusters.lst   a list of cluster structure returned from dtwclust. Each element correspond to a band attribute.
#' @return table     tibble  - a SITS table
#' @keywords SITS
#' @family   SITS main functions
#' @examples sits_fromClusters (savanna_s.tb, clusters.lst)

.sits_fromClusters <-  function (data.tb, clusters.lst) {

     centroids.lst <- clusters.lst %>%
          purrr::map (function (clu) {
               # what is the name of the band?
               band <- tools::file_path_sans_ext(names(clu@centroids)[1])

               # get only the statistically significant clusters' id
               total <- length(clu@cluster)

               # get the unique id of the clusters
               uniq_clust <- unique(clu@cluster)

               # get the proportions for each cluster (0 to 100% of samples)
               partitions <- purrr::map (uniq_clust, function (c) { sum (clu@cluster == c)/total})

               # select only clusters with more than 10% of the samples
               relevants <- tibble(clust_id=uniq_clust, partitions) %>%
                    dplyr::filter(partitions >= 0.1)

               # get respective clusters centroids ids
               series_id <- as.integer(tools::file_ext(names(clu@centroids))[relevants$clust_id]) + 1

               # copy centroids values
               centroids <- data.tb[series_id,]

               # select only cluster band (drops all other bands)
               for (j in 1:nrow(centroids)) {
                    centroids$time_series[[j]] <- dplyr::select(centroids$time_series[[j]], one_of(c("Index", band)))
               }

               return (centroids)
          })
     nmin <- Inf
     for (i in (1:length (centroids.lst))) {
          t <- centroids.lst [[i]]
          if (nrow(t) < nmin) nmin <- nrow(t)
     }
     centroids.tb  <- centroids.lst[[1]][1:nmin,]
     centroids.lst <- centroids.lst[-1]
     centroids.tb  <- centroids.lst %>%
          map (function (t) {
               centroids.tb <- sits_merge (centroids.tb, t[1:nmin,])
               return (centroids.tb)
          })
     return(centroids.tb[[1]])
}

#------------------------------------------------------------------
#' Returns a list of centroids and its metadata.
#'
#' \code{.sits_cluster_info} prints information about the cluster
#'
#' @param clusters   a cluster structure returned from dtwclust.
#' @return
#' @keywords SITS
#' @family   SITS auxiliary functions
#' @examples .sits_cluster_info (clusters.)


.sits_cluster_info <- function (clusters) {

     cat ("-------------------------------\n")
     # what is the name of the band?
     band <- tools::file_path_sans_ext(names(clusters@centroids)[1])
     cat (paste ("Band :", band, "\n", sep = ""))

     # print the size of each cluster
     df <- clusters@clusinfo
     cat ("Cluster sizes\n")
     for (i in 1:nrow(df)) {
               cat (paste ("Cluster ", i,": ",df[i,"size"], "\n", sep = ""))
     }
     return (clusters)
}
