# -----------------------------------------------------------
#' Cluster a set of time series
#'
#' \code{sits_cluster} cluster time series with dtw distance
#'
#' This function uses package "dtwclust" to do time series clustering.
#' See "dtwclust" documentation for more details
#'
#' @param data.tb      a SITS tibble the list of time series to be clustered
#' @param label_groups a list containing all labels (or groups of it in a `vector`) to be splitted up and clusterized apart.
#' @param type         string - either "dendogram" or "centroids"
#' @param n_clusters   the number of clusters to be identified.
#' @return clusters.tb a SITS tibble with the clusters
#' @family  STIS cluster functions
#' @export
sits_cluster <- function (data.tb, label_groups, type = "dendogram", n_clusters = 4){
     # select cluster option
     switch (type,
             "dendogram" = {
                  cluster.tb <- sits_table()
                  label_groups %>%
                       purrr::map (function (label) {
                            cluster.tb <<- dplyr::bind_rows(cluster.tb, sits_dendogram (data.tb, label, n_clusters))
                       })
                  },
             "centroids" = {
                  cluster.tb <- sits_table()
                  label_groups %>%
                       purrr::map (function (label) {
                            cluster.tb <<- dplyr::bind_rows(cluster.tb, sits_centroids (data.tb, label, n_clusters))
                       })
                  },
             { message (paste ("sits_cluster: valid cluster methods are dendogram or centroids",
                             "\n", sep = ""))
               stop(cond)}
     )
     return (cluster.tb)
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
#' @param labels       the label or a vector of labels to be clusterized together.
#' @param n_clusters   the number of clusters to be identified
#' @return clusters.tb a SITS tibble with the clusters
#' @keywords STIS
#' @family   STIS cluster functions
#' @export
#'
sits_dendogram <- function (data.tb, labels, n_clusters = 4) {
     cluster_dendogram <- function (data.tb, band, n_clusters){
          # get the values of the various time series for this band
          values.tb <- data.tb %>%
               sits_values_rows (band)
          clusters  <- dtwclust::dtwclust (values.tb,
                                 type     = "hierarchical",
                                 k        = n_clusters,
                                 distance = "dtw_basic",
                                 seed     = 899)

          #plot the dendogram
          graphics::plot (clusters)

          # Plot the series and the obtained prototypes
          graphics::plot (clusters, type = "sc")

          # Plot the centroids
          graphics::plot (clusters, type = "centroids")

          # print information about the clusters
          .sits_cluster_info (clusters)

          return (clusters)
     }

     data_label.tb <- data.tb %>%
          dplyr::filter(label %in% labels)

     # print current clusterised label(s)
     message (paste("\n", "Label(s): ", labels))

     cluster.lst <- data_label.tb %>%
          sits_bands() %>%
          purrr::map (function (b) cluster_dendogram (data_label.tb, b, n_clusters))

     cluster.tb <- .sits_fromClusters (data_label.tb, cluster.lst)
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
#' @param label        the label or a vector of labels to be clusterized together.
#' @param n_clusters   the number of clusters to be identified
#' @return clusters.tb a SITS tibble with the clusters
#' @keywords STIS
#' @family   STIS cluster functions
#' @export
#'
sits_centroids <- function (data.tb, label, n_clusters = 4) {
     cluster_partitional <- function (data.tb, label, band, n_clusters) {
          values.tb <- data.tb %>%
               dplyr::filter(label %in% label) %>%
               sits_values_rows (band)
          clusters  <- dtwclust::dtwclust (values.tb,
                                 type     = "partitional",
                                 k        = n_clusters,
                                 distance = "dtw_basic",
                                 centroid = "pam",
                                 seed     = 899)

          # Plot the series and the obtained prototypes
          graphics::plot (clusters, type = "sc")
          # Plot the centroids
          graphics::plot (clusters, type = "centroids")
          # print information about the clusters
          .sits_cluster_info (clusters)

          return (clusters)
     }
     # for each band, find a significant set of clusters
     clusters.lst <- data.tb %>%
          sits_bands() %>%
          purrr::map (function (b) cluster_partitional (data.tb, label, n, n_clusters))

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
#' @param data.tb          a tibble with input data of dtwclust.
#' @param clusters.lst     a list of cluster structure returned from dtwclust. Each element correspond to a band attribute.
#' @return centroids.tb    a SITS table with the clusters
#' @keywords SITS
#' @examples .sits_fromClusters (savanna_s.tb, clusters.lst)

.sits_fromClusters <-  function (data.tb, clusters.lst) {
     centroids.lst <- clusters.lst %>%
          purrr::map (function (clu) {
               # what is the name of the band?
               band <- tools::file_path_sans_ext(names(clu@centroids)[1])

               # how many clusters have more than 10% of the total samples?
               num <- clu@clusinfo$size %>%
                    .[. > as.integer (0.1 * length(clu@cluster))] %>%
                    length()

               # get centroids ids
               # extract the cluster names (e.g, "ndvi.21", "ndiv.35")
               # get only the numbers as integers (e.g., 21 35)
               ids <- as.integer(tools::file_ext(names (clu@centroids)))

               ids [is.na(ids)] <- 0  # filter NAs
               ids <- ids + 1         # add 1 (dtwclust numbers begin at 0)
               ids <- ids[1:num]      # get only those numbers who are centroids of big clusters

               # select centroids values
               centroids <- data.tb[ids,]

               # select only cluster band (drops all other bands)
               for (j in 1:nrow(centroids)) {
                    centroids$time_series[[j]] <- dplyr::select(centroids$time_series[[j]], dplyr::one_of(c("Index", band)))
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

     centroids.lst %>%
          purrr::map (function (clust_band.tb) {
               # we need store the cross join result into `centroids.tb` from outer scope (so the <<- operator)
               # outer scope `centroids.tb` variable is updated on next map iteration (this transform our code in a recursive one)
               # (note that this does NOT create a global variable!)
               centroids.tb <<- sits_cross (centroids.tb, clust_band.tb[1:nmin,])
          })
     return(centroids.tb)
}

#------------------------------------------------------------------
#' Returns a list of centroids and its metadata.
#'
#' \code{.sits_cluster_info} prints information about the cluster
#'
#' @param clusters   a cluster structure returned from dtwclust.
#' @return clusters  a cluster structure from dtwclust
#' @keywords SITS
#' @family   SITS auxiliary functions
#' @examples .sits_cluster_info (clusters.)

.sits_cluster_info <- function (clusters) {
     cat ("-------------------------------\n")
     # what is the name of the band?
     band <- tools::file_path_sans_ext(names(clusters@centroids)[1])
     cat (paste ("Band: ", band, "\n", sep = ""))

     # print the size of each cluster
     df <- clusters@clusinfo
     cat ("Cluster sizes\n")
     for (i in 1:nrow(df)) {
               cat (paste ("Cluster ", i,": ", df[i,"size"], "\n", sep = ""))
     }
     return (clusters)
}
