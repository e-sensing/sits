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
#' @param bands        the bands to be clusterized.
#' @param method       string - either "dendogram" or "centroids"
#' @param n_clusters   the number of clusters to be identified
#' @param min_clu_perc the minimum percentage for a cluster to be valid
#' @param show         (boolean) should the results be shown?
#' @return clusters.tb a SITS tibble with the clusters
#' @export
sits_cluster <- function (data.tb, label_groups = NULL, band_groups = NULL, method = "dendogram", n_clusters = 4, min_clu_perc = 0.10, show = TRUE){
     ensurer::ensure_that(method, (. == "dendogram" || . == "centroids"),
                          err_desc = "sits_cluster: valid cluster methods are dendogram or centroids")

     # if no label group is provided create a group for EACH label
     if (is.null(label_groups)) {
          labels <- dplyr::distinct (data.tb, label)$label
          label_groups <- as.list(labels)
          names(label_groups) <- labels
     }

     # define label groups' name if they are not defined
     if (is.null(names(label_groups)))
          names(label_groups) <- purrr::map(label_groups, 1)

     # if no band group is provided create one group for ALL bands
     if (is.null(band_groups))
          band_groups <- list(sits_bands(data.tb))

     # creates the resulting table
     cluster.tb <- sits_table()

     purrr::map2(names(label_groups), label_groups, function (group, labels) {
          # filter only those rows with the same labels
          label.tb <- dplyr::filter (data.tb, label %in% labels)

          # print current clusterised label(s)
          if (show) message (paste0("\n", "Label(s): ", labels))

          # apply the clustering method
          clu.tb <- sits_dtwclust (label.tb, bands = bands, method, n_clusters = n_clusters, min_clu_perc = min_clu_perc,
                                   label_prefix = group, show = show)

          # append the result
          # note that operator `<<-` is used to access outer scope variable `cluster.tb` and does not create a global variable.
          cluster.tb <<- dplyr::bind_rows(cluster.tb, clu.tb)
     })

          return (cluster.tb)
}

#' @title Cluster a set of time series
#' @name sits_dtwclust
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description This function provides an interface to the DTWCLUST package.
#'
#' @references "dtwclust" package (https://CRAN.R-project.org/package=dtwclust)
#'
#' @param data.tb      a tibble the list of time series to be clustered
#' @param bands        a vector the bands to be clusterized..
#' @param method       string - either "dendogram" or "centroids"
#' @param n_clusters   the number of clusters to be identified
#' @param min_clu_perc the minimum percentage of members for a cluster to be valid
#' @param show         (boolean) should the results be shown?
#' @return clusters.tb a SITS tibble with the clusters
#' @export
#'
sits_dtwclust <- function (data.tb, bands = NULL, method = "dendogram",  n_clusters = 4, min_clu_perc = 0.10,
                           label_prefix = NULL, show = TRUE) {

     # if no bands are selected, use all bands
     if (purrr::is_null (bands)) bands <- sits_bands(data.tb)

     # obtain the cluster list
     if (method == "dendogram")
          cluster.tb <- .sits_cluster_dendogram (data.tb, bands, n_clusters, min_clu_perc, label_prefix, show)
     else
          cluster.tb <- .sits_cluster_partitional (data.tb, bands, n_clusters, min_clu_perc, label_prefix, show)


     return (cluster.tb)

}
#' @title Cluster a set of time series using hierarchical clustering
#' @name .sits_cluster_dendogram
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
#' @param bands        a vector the bands to be clusterized.
#' @param n_clusters   the number of clusters to be identified
#' @param min_clu_perc the minimum percentage of members for a cluster to be valid
#' @param label_prefix a default label name to give to each created cluster. The final name will be `<label_prefix>.<#>`.
#' @param show         (boolean) should the results be shown?
#' @return clusters.tb a SITS tibble with the clusters
#' @export
#'
sits_dendogram <- function (data.tb, band_groups = NULL, n_clusters = 4, min_clu_perc = 0.10, label_prefix = NULL, show = TRUE) {
     # if no band group is provided create a group for ALL bands
     if (is.null(band_groups)) {
          band_groups <- list(sits_bands(data.tb))
     }

     # get all clusters as a list of tibbles
     centroids.lst <- band_groups %>%
          purrr::map (function (bands) .sits_cluster_dendogram (data.tb, bands, n_clusters, min_clu_perc, label_prefix))

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

# returns a tibble with all clusters centroids of a given band group
.sits_cluster_dendogram <- function (data.tb, bands, n_clusters, min_clu_perc, label_prefix = NULL, show){
     # get the values of the various time series for this band group
     values.tb <- sits_values_rows (data.tb, bands)
     clusters  <- dtwclust::dtwclust (values.tb,
                                      type     = "hierarchical",
                                      k        = n_clusters,
                                      distance = "dtw_basic")

     # Plot the series and the obtained prototypes
     if (show) .sits_show_clusters (clusters)

     return (.sits_fromClusters (data.tb, clusters, min_clu_perc, label_prefix))
}


#' @title Cluster a set of time series using partitional clustering
#' @name .sits_cluster_partitional
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
#' @param band_groups  a list containing groups of bands to be clusterized. A group of bands may be a band alone (str) or a vector of bands.
#' @param n_clusters   the number of clusters to be identified
#' @param min_clu_perc the minimum percentage of members (relative to the total) for a cluster to be valid
#' @param label_prefix a default label name to give to each created cluster. The final name will be `<label_prefix>.<#>`.
#' @param show         (boolean) should the results be shown?
#' @return clusters.tb a SITS tibble with the clusters
#' @export
#'
sits_centroids <- function (data.tb, band_groups = NULL, n_clusters = 4, perc = 0.10, label_prefix = NULL, show = TRUE) {
     if (is.null(band_groups)) {
          band_groups <- list(sits_bands(data.tb))
     }

     # for each band, find a significant set of clusters
     centroids.lst <- band_groups %>%
          purrr::map (function (bands) .sits_cluster_partitional (data.tb, bands, n_clusters, perc, label_prefix))

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

.sits_cluster_partitional <- function (data.tb, bands, n_clusters, min_clu_perc, label_prefix = NULL, show) {
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

     return (.sits_fromClusters (data.tb, clusters, min_clu_perc, label_prefix))
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
#' @param min_clu_perc     the minimum percentage of members (relative to the total) for a cluster to be valid
#' @return centroids.tb    a SITS table with the clusters
.sits_fromClusters <-  function (data.tb, clusters, min_clu_perc, label_prefix = NULL) {

     # what is the name(s) of the band(s)?
     bands <- colnames(clusters@centroids[[1]])

     # how many clusters have more than 10% of the total samples?
     num <- clusters@clusinfo$size %>%
          .[. > as.integer (min_clu_perc * length(clusters@cluster))] %>%
          length()

     # if no prefix was provided get the label from data.tb's first record
     if (is.null(label_prefix))
          label_prefix <- data.tb[1,]$label[[1]]

     # select only significant centroids
     oids.tb <- centroids.tb %>% dplyr::mutate(label = paste(.$label, ".", as.character(1:nrow(.)), sep = ""))

     centroids.lst <- clusters@centroids[order(clusters@clusinfo$size, decreasing = TRUE)][1:num]

     centroids.tb <- sits_table()
     purrr::map2(centroids.lst, seq_along(centroids.lst), function (ts, i) {
          new_ts <- dplyr::select(data.tb[1,]$time_series[[1]], Index)
          new_ts <- dplyr::bind_cols(new_ts, tibble::as_tibble(ts))
          centroids.tb <<- tibble::add_row (centroids.tb,
                           longitude    = 0.0,
                           latitude     = 0.0,
                           start_date   = data.tb[1,]$start_date[[1]],
                           end_date     = data.tb[1,]$end_date[[1]],
                           label        = paste0(label_prefix, ".", i),
                           coverage     = data.tb[1,]$coverage[[1]],
                           time_series  = list(new_ts))
     })

     return (centroids.tb)
}

#' @title Plots a cluster produced by the dtwclust package
#' @name .sits_showClusters
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

