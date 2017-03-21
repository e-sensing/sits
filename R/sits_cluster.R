# -----------------------------------------------------------
#' Cluster a set of time series
#'
#' \code{sits_cluster} cluster time series with dtw distance
#'
#' This function uses package "dtwclust" to do time series clustering.
#' See "dtwclust" documentation for more details
#'
#' @param data.tb      a SITS tibble the list of time series to be clustered
#' @param label_groups a list containing groups of labels to be clusterized apart. A group of labels may be an label alone (str) or a vector of labels.
#' @param type         string - either "dendogram" or "centroids"
#' @param n_clusters   the number of clusters to be identified.
#' @return clusters.tb a SITS tibble with the clusters
#' @family  STIS cluster functions
#' @export
sits_cluster <- function (data.tb, label_groups, band_groups, type = "dendogram", n_clusters = 4, ignore_clusters_prop = 0.1, do_plot = TRUE){
     # select cluster option
     switch (type,
             "dendogram" = {
                  # creates the resulting table
                  cluster.tb <- sits_table()

                  # for each group of labels computes clusters for all bands
                  label_groups %>%
                       purrr::map (function (labels) {

                            # if no band group is provided create a group for each band
                            if (is.null(band_groups)) {
                                 band_groups <- sits_bands(data.tb)
                            }

                            # computes clusters for all bands (This variable stores a list of tibles that stores clusters for each band)
                            centroids_band.lst <- band_groups %>%
                                 # computes clusters for a given band
                                 purrr::map (function (bands) {
                                      sits_dendogram (data.tb, labels, bands, n_clusters, ignore_clusters_prop, do_plot)
                                 })

                            # creates an empty sits_table to stores all clusters of a label group
                            cluster_label.tb <- sits_table()

                            # proceed with cross join operation between bands of a given cluster
                            centroids_band.lst %>%
                                 purrr::map (function (clust_band.tb) {
                                      # we need store the cross join result into `centroids.tb` from outer scope (so the <<- operator)
                                      # outer scope `centroids.tb` variable is updated on next map iteration (this transform our code in a recursive one)
                                      # (note that this does NOT create a global variable!)
                                      cluster_label.tb <<- sits_cross (cluster_label.tb, clust_band.tb)
                                 })

                            # appends the resulting sits_table with crossed-band clusters of the current label group
                            cluster.tb <<- dplyr::bind_rows(cluster.tb, cluster_label.tb)
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
#' @param data.tb              a tibble the list of time series to be clustered
#' @param labels               the label or a vector of labels to be clusterized together.
#' @param band                 the band to be clusterized
#' @param n_clusters           the number of clusters to be identified
#' @param ignore_clusters_prop ignores those clusters that have proportionally less samples than `ignore_clusters_prop`.
#' @return clusters.tb a SITS tibble with the clusters
#' @keywords STIS
#' @family   STIS cluster functions
#' @export
#'
sits_dendogram <- function (data.tb, labels, bands, n_clusters = 4, ignore_clusters_prop = 0.1, do_plot = TRUE) {

     # get the values of the various time series for this band group (bands)
     # data_label.tb is used later to get the centroids
     data_label.tb <- data.tb %>%
          dplyr::filter(label %in% labels)

     # put data_label.tb in a format that dtwclust works
     values.lst <- data_label.tb %>%
          sits_values_rows (bands)

     # computes dendogram clustering
     clusters  <- dtwclust::dtwclust (values.lst,
                                      type     = "hierarchical",
                                      k        = n_clusters,
                                      distance = "dtw_basic")

     # print current clusterised label(s)
     message (paste("\n", "Label(s): ", labels))

     if (do_plot) {
          #plot the dendogram
          graphics::plot (clusters)

          # Plot the series and the obtained prototypes
          graphics::plot (clusters, type = "sc")

          # Plot the centroids
          graphics::plot (clusters, type = "centroids")
     }

     # print information about the clusters
     .sits_cluster_info (clusters)

     # how many clusters have more than 10% of the total samples?
     num <- clusters@clusinfo$size %>%
          .[. > as.integer (ignore_clusters_prop * length(clusters@cluster))] %>%
          length()

     # get centroids ids
     # extract the cluster names (e.g, "ndvi.21", "ndvi.35")
     # get only the numbers as integers (e.g., 21 35)
     ids <- as.integer(tools::file_ext(names (clusters@centroids)))
     ids [is.na(ids)] <- 0  # the firs name has no number, so fill NA with 0
     ids <- ids + 1         # add 1 (because dtwclust numbers begins at 0 and R begins at 1)
     ids <- ids[order(clusters@clusinfo$size, decreasing = TRUE)][1:num]      # get only those numbers who are centroids of big clusters

     # select only significant centroids
     clusters.tb <- data_label.tb[ids,]

     # select only cluster band (drops all other bands)
     clusters.tb$time_series <- clusters.tb$time_series %>%
          purrr::map (function (ts) dplyr::select(ts, dplyr::one_of(c("Index", bands))))

     return (clusters.tb)

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

.sits_fromClusters <-  function (data.tb, clusters.lst, ignore_clusters_bellow_prop = 0.1) {
     centroids.lst <- clusters.lst %>%
          purrr::map (function (clu) {
               # what is the name of the band?
               band <- tools::file_path_sans_ext(names(clu@centroids)[1])

               # how many clusters have more than 10% of the total samples?
               num <- clu@clusinfo$size %>%
                    .[. > as.integer (ignore_clusters_bellow_prop * length(clu@cluster))] %>%
                    length()

               # get centroids ids
               # extract the cluster names (e.g, "ndvi.21", "ndvi.35")
               # get only the numbers as integers (e.g., 21 35)
               ids <- as.integer(tools::file_ext(names (clu@centroids)))
               ids [is.na(ids)] <- 0  # the firs name has no number, so fill NA with 0
               ids <- ids + 1         # add 1 (because dtwclust numbers begins at 0 and R begins at 1)
               ids <- ids[order(clu@clusinfo$size, decreasing = TRUE)][1:num]      # get only those numbers who are centroids of big clusters

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
