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
#' @param label_groups a list containing groups of labels to be clusterized. A group of labels may be a label alone (str) or a vector of labels.
#' @param band_groups  a list containing groups of bands to be clusterized. A group of bands may be a band alone (str) or a vector of bands.
#' @param method       string - either "dendogram" or "centroids"
#' @param n_clusters   the number of clusters to be identified
#' @param perc         the minimum percentage for a cluster to be valid
#' @param show         (boolean) should the results be shown?
#' @return clusters.tb a SITS tibble with the clusters
#' @export
sits_cluster <- function (data.tb, label_groups = NULL, band_groups = NULL, method = "dendogram", n_clusters = 4, perc = 0.10, show = TRUE){
     ensurer::ensure_that(method, (. == "dendogram" || . == "centroids"),
                          err_desc = "sits_cluster: valid cluster methods are dendogram or centroids")

     # if no label group is provided create a group for EACH label
     if (is.null(label_groups)) {
          label_groups <- dplyr::distinct (data.tb, label)$label
     }

     # if no band group is provided create a group for ALL bands
     if (is.null(band_groups)) {
          band_groups <- list(sits_bands(data.tb))
     }

     # creates the resulting table
     cluster.tb <- sits_table()

     label_groups %>%
          purrr::map(function (labels) {
               # filter only those rows with the same labels
               label.tb <- dplyr::filter (data.tb, label %in% labels)

               if (show) {
                    # print current clusterised label(s)
                    message (paste0("\n", "Label(s): ", labels))
               }

               # apply the clustering method
               if (method == "dendogram")
                    clu.tb <- sits_dendogram (label.tb, band_groups = band_groups, n_clusters = n_clusters, perc = perc, show = show)
               else
                    clu.tb <- sits_centroids (label.tb, band_groups = band_groups, n_clusters = n_clusters, perc = perc, show = show)

               # append the result
               # note that operator `<<-` is used to access outer scope variable `cluster.tb` and does not create a global variable.
               cluster.tb <<- dplyr::bind_rows(cluster.tb, clu.tb)
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
#' @param band_groups  a list containing groups of bands to be clusterized. A group of bands may be a band alone (str) or a vector of bands.
#' @param n_clusters   the number of clusters to be identified
#' @param perc         the minimum percentage for a cluster to be valid
#' @param show         (boolean) should the results be shown?
#' @return clusters.tb a SITS tibble with the clusters
#' @export
#'
sits_dendogram <- function (data.tb, band_groups = NULL, n_clusters = 4, perc = 0.10, show = TRUE) {
     # returns a tibble with all clusters centroids of a given band group
     cluster_dendogram <- function (data.tb, bands, n_clusters, perc){
          # get the values of the various time series for this band group
          values.tb <- sits_values_rows (data.tb, bands)
          clusters  <- dtwclust::dtwclust (values.tb,
                                 type     = "hierarchical",
                                 k        = n_clusters,
                                 distance = "dtw_basic")

          # Plot the series and the obtained prototypes
          if (show) .sits_show_clusters (clusters)

          return (.sits_fromClusters (data.tb, clusters, perc))
     }


     # if no band group is provided create a group for ALL bands
     if (is.null(band_groups)) {
          band_groups <- list(sits_bands(data.tb))
     }

     # get all clusters as a list of tibbles
     centroids.lst <- band_groups %>%
          purrr::map (function (bands) cluster_dendogram (data.tb, bands, n_clusters, perc))

     # creates an empty sits_table to populate result
     cluster.tb <- sits_table()

     # proceed with cross join operation between bands of a given cluster
     centroids.lst %>%
          purrr::map (function (clu.tb) {
               # we need store the cross join result into `cluster.tb` from outer scope (so the <<- operator)
               # outer scope `cluster.tb` variable is updated on next map iteration (this transform our code in a recursive one)
               # (note that this does NOT create a global variable!)
               cluster.tb <<- sits_cross (cluster.tb, clu.tb)
          })

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
#' @param label        the label or a vector of labels to be clusterized together.
#' @param n_clusters   the number of clusters to be identified
#' @param perc         the minimum percentage for a cluster to be valid
#' @param show         (boolean) should the results be shown?
#' @return clusters.tb a SITS tibble with the clusters
#' @export
#'
sits_centroids <- function (data.tb, band_groups = NULL, n_clusters = 4, perc = 0.10, show = TRUE) {
     cluster_partitional <- function (data.tb, bands, n_clusters, perc) {
          # get the values of the various time series for this band group
          values.tb <- sits_values_rows (data.tb, bands)
          clusters  <- dtwclust::dtwclust (values.tb,
                                 type     = "partitional",
                                 k        = n_clusters,
                                 distance = "dtw_basic",
                                 centroid = "pam",
                                 seed     = 899)

          # Plot the series and the obtained prototypes
          if (show) .sits_show_clusters (clusters)

          return (.sits_fromClusters (data.tb, clusters, perc))

     }


     # for each band, find a significant set of clusters
     centroids.lst <- band_groups %>%
          purrr::map (function (bands) cluster_partitional (data.tb, bands, n_clusters))

     # creates an empty sits_table to populate result
     cluster.tb <- sits_table()

     # proceed with cross join operation between bands of a given cluster
     centroids.lst %>%
          purrr::map (function (clu.tb) {
               # we need store the cross join result into `cluster.tb` from outer scope (so the <<- operator)
               # outer scope `cluster.tb` variable is updated on next map iteration (this transform our code in a recursive one)
               # (note that this does NOT create a global variable!)
               cluster.tb <<- sits_cross (cluster.tb, clu.tb)
          })

     return (cluster.tb)
}

#------------------------------------------------------------------
#' @title Returns a tibble of centroids and its metadata.
#' @name .sits_fromClusters
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description reads a list of clusters provided by the dtwclust
#' package and produces a sits table.
#' @references "dtwclust" package (https://CRAN.R-project.org/package=dtwclust)
#'
#' @param data.tb          a tibble with input data of dtwclust.
#' @param clusters         a cluster structure returned from dtwclust.
#' @param perc             ignores those clusters linked with less than some percentage of total samples.
#' @return centroids.tb    a SITS table with the clusters
.sits_fromClusters <-  function (data.tb, clusters, perc) {

     # what is the name(s) of the band(s)?
     bands <- tools::file_path_sans_ext(names(clusters@centroids)[1])

     # how many clusters have more than 10% of the total samples?
     num <- clusters@clusinfo$size %>%
          .[. > as.integer (perc * length(clusters@cluster))] %>%
          length()

     # get centroids ids
     # extract the cluster names (e.g, "ndvi.21", "ndvi.35")
     # get only the numbers as integers (e.g., 21 35)
     ids <- as.integer(tools::file_ext(names (clusters@centroids)))
     ids [is.na(ids)] <- 0  # the firs name has no number, so fill NA with 0
     ids <- ids + 1         # add 1 (because dtwclust numbers begins at 0 and R begins at 1)
     ids <- ids[order(clusters@clusinfo$size, decreasing = TRUE)][1:num]      # get only those numbers who are centroids of big clusters

     # select only significant centroids
     centroids.tb <<- data.tb[ids,]

     # select only cluster bands of group (drops all other bands)
     centroids.tb$time_series <- centroids.tb$time_series %>%
          purrr::map (function (ts) dplyr::select(ts, dplyr::one_of(c("Index", strsplit(bands, "$", fixed = TRUE)[[1]]))))

     if (nrow(centroids.tb) > 1)
          centroids.tb <- centroids.tb %>% dplyr::mutate(label = paste(.$label, ".", as.character(1:nrow(.)), sep = ""))

     return (centroids.tb)
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

     # Plot dendogram/default dtwclust plot
     graphics::plot (clusters)

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
     cat (paste ("Band(s): ", band, "\n", sep = ""))

     # print the size of each cluster
     df <- clusters@clusinfo
     cat ("Cluster sizes\n")
     for (i in 1:nrow(df)) {
               cat (paste ("Cluster ", i,": ", df[i,"size"], "\n", sep = ""))
     }
     return (clusters)
}

