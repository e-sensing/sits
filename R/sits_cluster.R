#' @title Cluster a set of satellite image time series
#' @name sits_cluster
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description This function uses package "dtwclust" to do time series clustering.
#' There are two options: "dendogram" (hierarchical clustering) and "controids" (positional
#' clustering)
#' @references "dtwclust" package (https://CRAN.R-project.org/package=dtwclust)
#'
#' @param data.tb      a SITS tibble the list of time series to be clustered
#' @param method       string - either "dendogram" or "centroids"
#' @param n_clusters   the number of clusters to be identified
#' @param perc         the minimum percentage for a cluster to be valid
#' @param show         (boolean) should the results be shown?
#' @return clusters.tb a SITS tibble with the clusters
#' @export
sits_cluster <- function (data.tb, method = "dendogram", n_clusters = 4, perc = 0.10, show = TRUE){

     ensurer::ensure_that(method, (. == "dendogram" || . == "centroids"), err_desc = "sits_cluster: valid cluster methods are dendogram or centroids")
     # create a table to store the results
     cluster.tb <- sits_table()
     # how many different labels are there?
     labels <- dplyr::distinct (data.tb, label)

     # for each label, find a desired number of clusters
     cluster.tb <- labels %>%
          dplyr::rowwise() %>%
          dplyr::do ({
               # filter those rows with the same label
               label.tb <- dplyr::filter (data.tb, label == as.character (.$label))
               # apply the clustering method
               if (method == "dendogram")
                    clu.tb <- sits_dendogram (label.tb, n_clusters = n_clusters, perc = perc, show = show)
               else
                    clu.tb <- sits_centroids (label.tb, n_clusters = n_clusters, perc = perc, show = show)
               # get the result
               cluster.tb <- dplyr::bind_rows(cluster.tb, clu.tb)
               return (cluster.tb)
          })
     return (cluster.tb)
}

#' @title Cluster a set of time series using hierarchical clustering
#' @name sits_dendogram
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Cluster time series in hierarchical mode. Hierarchical clustering, as its name suggests,
#' is an algorithm that tries to create a hierarchy
#' of groups in which, as the level in the hierarchy increases, clusters are created by merging
#' the clusters from the next lower level, such that an ordered sequence of groupings is obtained.
#' The similarity measure used to group time series in a cluster is the dtw metric.
#' The procedure is deterministic, so it will always give the same
#' result for a chosen set of similarity measures (taken from the DTWCLUST package docs).
#'
#' @references "dtwclust" package (https://CRAN.R-project.org/package=dtwclust)
#'
#' @param data.tb      a tibble the list of time series to be clustered
#' @param n_clusters   the number of clusters to be identified
#' @param perc         the minimum percentage for a cluster to be valid
#' @param show         (boolean) should the results be shown?
#' @return clusters.tb a SITS tibble with the clusters
#' @export
#'
sits_dendogram <- function (data.tb, n_clusters = 4, perc = 0.10, show = TRUE) {
     cluster_dendogram <- function (data.tb, band, n_clusters){
          # get the values of the various time series for this band
          values.tb <- sits_values_rows (data.tb, band)
          clusters  <- dtwclust::dtwclust (values.tb,
                                 type     = "hierarchical",
                                 k        = n_clusters,
                                 distance = "dtw_basic",
                                 seed     = 899)
          if (show) {
               # Plot the series and the obtained prototypes
               graphics::plot (clusters, type = "sc")
               # Plot the centroids
               graphics::plot (clusters, type = "centroids")
               # print information about the clusters
               .sits_cluster_info (clusters)
          }
          return (clusters)
     }
     # apply the cluster function to each of the bands
     cluster.lst <- data.tb %>%
          sits_bands() %>%
          purrr::map (function (b) cluster_dendogram (data.tb, b, n_clusters))

     # merge the clusters centres for each band to get a cluster centre with all bands
     cluster.tb <- .sits_fromClusters (data.tb, cluster.lst, perc)

     return (cluster.tb)
}

#' @title Cluster a set of time series using partitional clustering
#' @name sits_centroids
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Partitional clustering assigns the data to one and only
#' one cluster out of k total clusters.
#' First, k centroids are randomly initialized, usually by choosing k objects
#' from the dataset at random. these are assigned to individual clusters.
#' The distance between all objects in the data and all centroids
#' is calculated, and each object is assigned to the cluster of its closest centroid.
#' A prototyping function is applied to each cluster to update the corresponding centroid.
#' Then, distances and centroids are updated iteratively until a certain number of iterations have elapsed,
#' or no object changes clusters anymore (description extracted from dtwclust package)
#' @references "dtwclust" package (https://CRAN.R-project.org/package=dtwclust)
#'
#' @param data.tb      a tibble the list of time series to be clustered
#' @param n_clusters   the number of clusters to be identified
#' @param perc         the minimum percentage for a cluster to be valid
#' @param show         (boolean) should the results be shown?
#' @return clusters.tb a SITS tibble with the clusters
#' @export
sits_centroids <- function (data.tb, n_clusters = 4, perc = 0.10, show = TRUE) {
     cluster_partitional <- function (band, data.tb, n_clusters) {
          values.tb <- sits_values_rows (data.tb, band)
          clusters  <- dtwclust::dtwclust (values.tb,
                                 type     = "partitional",
                                 k        = n_clusters,
                                 distance = "dtw_basic",
                                 centroid = "pam",
                                 seed     = 899)
          # display the results?
          if (show) .sits_show_clusters (clusters)
          return (clusters)
     }
     # for each band, find a significant set of clusters
     clusters.lst <- data.tb %>%
          sits_bands() %>%
          purrr::map (function (b) cluster_partitional (b, data.tb, n_clusters))

     # retrieve the significant clusters as a list
     cluster.tb <- .sits_fromClusters (data.tb, clusters.lst, perc)

     return (cluster.tb)
}

#' @title Returns a list of centroids and its metadata.
#' @name .sits_fromClusters
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description reads a list of clusters provided by the dtwclust
#' package and produces a sits table.
#' @references "dtwclust" package (https://CRAN.R-project.org/package=dtwclust)
#'
#' @param data.tb          a tibble with input data of dtwclust.
#' @param clusters.lst     a list of cluster structure returned from dtwclust. Each element correspond to a band attribute.
#' @return centroids.tb    a SITS table with the clusters
.sits_fromClusters <-  function (data.tb, clusters.lst, perc) {
     centroids.lst <- clusters.lst %>%
          purrr::map (function (clu) {
               # what is the name of the band?
               band <- tools::file_path_sans_ext(names(clu@centroids)[1])

               # how many clusters have more than 10% of the total samples?
               num <- clu@clusinfo$size %>%
                    .[. > as.integer (perc * length(clu@cluster))] %>%
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
               print (centroids[1,]$time_series)

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

#' @title Plots a cluster produced by the dtwclust package
#' @name .sits_fromClusters
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description plots a cluster produced by the dtwclust package
#' @references "dtwclust" package (https://CRAN.R-project.org/package=dtwclust)
#' @param clusters     a list of cluster structure returned from dtwclust.
.sits_show_clusters <- function (clusters) {
     # Plot the series and the obtained prototypes
     graphics::plot (clusters, type = "sc")
     # Plot the centroids
     graphics::plot (clusters, type = "centroids")
     # print information about the clusters
     .sits_cluster_info (clusters)
}

#' @title Prints information about the cluster generated by the dtwclust package
#' @name .sits_cluster_info
#'
#' @description The dtwclust package produces a cluster object. This function
#' prints information about this object
#' @references "dtwclust" package (https://CRAN.R-project.org/package=dtwclust)
#' @param clusters   a cluster structure returned from dtwclust.
#' @return clusters  a cluster structure from dtwclust

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

