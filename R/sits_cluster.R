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
#' @param data.tb         a SITS tibble the list of time series to be clustered
#' @param bands           the bands to be clusterized.
#' @param method          string - either "dendogram" or "centroids"
#' @param n_clusters      the number of clusters to be identified
#' @param grouping_method the agglomeration method to be used. Any `hclust` method (see `hclust`).
#' @param min_clu_perc    the minimum percentage for a cluster to be valid
#' @param grid_xdim       x dimension of the SOM grid
#' @param grid_ydim       y dimension of the SOM grid
#' @param rlen            the number of times the complete data set will be presented to the SOM grid
#' @param alpha           learning rate, a vector of two numbers indicating the amount of change. Default is to decline linearly from 0.05 to 0.01 over rlen updates.
#' @param return_members  (boolean) should the results be the clusters' members instead of clusters' centroids?
#' @param show            (boolean) should the results be shown?
#' @return clusters.tb a SITS tibble with the clusters
#' @export
sits_cluster <- function (data.tb, bands, method = "dendogram", n_clusters = 4, grouping_method = "ward.D2", min_clu_perc = 0.10,
                          grid_xdim = 5, grid_ydim = 5, rlen = 100, alpha = c(0.05, 0.01),
                          return_members = FALSE, show = TRUE){
     ensurer::ensure_that(method, (. == "dendogram" || . == "centroids" || . == "kohonen" || . == "koho&dogram"),
                          err_desc = "sits_cluster: valid cluster methods are 'dendogram', 'centroids', 'kohonen', or 'koho&dogram'.")

     # creates the resulting table
     cluster.tb <- sits_table()

     # how many different labels are there?
     labels <- dplyr::distinct (data.tb, label)

     labels %>%
          dplyr::rowwise() %>%
          dplyr::do({
               # filter only those rows with the same labels
               # cut time series to fit in one year
               label.tb <- dplyr::filter (data.tb, label == .$label) %>% sits_prune()


               # apply the clustering method
               if (method == "dendogram")
                    clu.tb <- .sits_cluster_dendogram (label.tb, bands, n_clusters, grouping_method,
                                                       min_clu_perc, return_members, show)
               else if (method == "centroids")
                    clu.tb <- .sits_cluster_partitional (label.tb, bands, n_clusters, grouping_method,
                                                         min_clu_perc, return_members, show)
               else if (method == "kohonen")
                    clu.tb <- .sits_cluster_kohonen (label.tb, bands, grid_xdim, grid_ydim, rlen, alpha, return_members, show)
               else if (method == "koho&dogram")
                    clu.tb <- .sits_cluster_kohodogram (label.tb, bands, grid_xdim, grid_ydim, rlen, alpha,
                                                        min_clu_perc, return_members, show)
                    # clu.tb <- .sits_cluster_kohonen (label.tb, bands, grid_xdim, grid_ydim, rlen, alpha, min_clu_perc, return_members = FALSE, show) %>%
                    #      .sits_cluster_dendogram (label.tb, bands, n_clusters, grouping_method, min_clu_perc = 0.0,
                    #                               return_members = return_members, show = FALSE)

               # append the result
               cluster.tb <<- dplyr::bind_rows(cluster.tb, clu.tb)
          })
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
#' @param data.tb         a tibble the list of time series to be clustered
#' @param bands           a vector the bands to be clusterized.
#' @param n_clusters      the number of clusters to be identified
#' @param grouping_method the agglomeration method to be used. Any `hclust` method (see `hclust`).
#' @param min_clu_perc    the minimum percentage of members for a cluster to be valid
#' @param return_members  (boolean) should the results be the clusters' members instead of clusters' centroids?
#' @param show            (boolean) should the results be shown?
#' @return clusters.tb a SITS tibble with the clusters
.sits_cluster_dendogram <- function (data.tb, bands, n_clusters, grouping_method, min_clu_perc, return_members, show){
     # get the values of the various time series for this band group
     values.tb <- sits_values (data.tb, bands, format = "cases_dates_bands")
     clusters  <- dtwclust::dtwclust (values.tb,
                                      type     = "hierarchical",
                                      k        = n_clusters,
                                      distance = "dtw_basic",
                                      method   = grouping_method)

     # Plot the series and the obtained prototypes
     if (show) .sits_dtwclust_show (clusters)

     # if return_members parameter is TRUE, returns a sits samples table with updated labels
     # else, returns cluster's centroids
     return (.sits_from_dtwclust (data.tb, clusters, min_clu_perc, return_members))
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
#' @param data.tb        a tibble the list of time series to be clustered
#' @param bands          a vector the bands to be clusterized.
#' @param n_clusters     the number of clusters to be identified
#' @param grouping_method the agglomeration method to be used. Any `hclust` method (see `hclust`).
#' @param min_clu_perc   the minimum percentage of members for a cluster to be valid
#' @param return_members (boolean) should the results be the clusters' members instead of clusters' centroids?
#' @param show           (boolean) should the results be shown?
#' @return clusters.tb a SITS tibble with the clusters
.sits_cluster_partitional <- function (data.tb, bands, n_clusters, grouping_method, min_clu_perc, return_members, show) {
     # get the values of the various time series for this band group
     values.tb <- sits_values (data.tb, bands, format = "cases_dates_bands")
     clusters  <- dtwclust::dtwclust (values.tb,
                                      type     = "partitional",
                                      k        = n_clusters,
                                      distance = "dtw_basic",
                                      method   = grouping_method,
                                      centroid = "pam",
                                      seed     = 899)

     # Plot the series and the obtained prototypes
     if (show) .sits_dtwclust_show (clusters)

     # if return_members parameter is TRUE, returns a sits samples table with updated labels
     # else, returns cluster's centroids
     return (.sits_from_dtwclust (data.tb, clusters, min_clu_perc, return_members))
}
#' @title Cluster a set of time series using kohonen clustering -- self organized maps (SOM)
#' @name .sits_cluster_kohonen
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Lorena Alves, \email{lorena.santos@@inpe.br}
#'
#' @description Cluster time series in SOM mode. SOMs can be thought
#' of as a spatially constrained form of k-means clustering.
#' Even with identical settings, repeated training
#' of a SOM will lead to sometimes even quite different mappings, because of the random initialisation.
#' However, in our experience the conclusions drawn from the map remain remarkably
#' consistent, which makes it a very useful tool in many different circumstances. Nevertheless,
#' it is always wise to train several maps before jumping to conclusions (taken from `kohonen` package docs).
#'
#' @references "kohonen" package (https://CRAN.R-project.org/package=kohonen)
#'
#' @param data.tb        a tibble the list of time series to be clustered
#' @param bands          a vector the bands to be clusterized.
#' @param grid_xdim      x dimension of the SOM grid
#' @param grid_ydim      y dimension of the SOM grid
#' @param rlen           the number of times the complete data set will be presented to the SOM grid
#' @param alpha          learning rate, a vector of two numbers indicating the amount of change.
#'                       Default is to decline linearly from 0.05 to 0.01 over rlen updates.
#' @param return_members (boolean) should the results be the clusters' members instead of clusters' centroids?
#' @param show           (boolean) should the results be shown?
#' @return clusters.tb a SITS tibble with the clusters
.sits_cluster_kohonen <- function (data.tb, bands, grid_xdim, grid_ydim, rlen, alpha, return_members, show){

     # get the values of the various time series for this band group
     values.tb <- sits_values (data.tb, bands, format = "bands_cases_dates")

     grid <- kohonen::somgrid(xdim = grid_xdim, ydim = grid_ydim, topo = "rectangular")
     kohonen_obj  <- kohonen::supersom (values.tb, grid=grid,
                                        rlen = rlen, alpha = alpha,
                                        keep.data = TRUE,
                                        dist.fcts = "euclidean")

     # Plot the series and the obtained prototypes
     if (show) .sits_kohonen_show (kohonen_obj)

     # if return_members parameter is TRUE, returns a sits samples table with updated labels else, returns cluster's centroids
     return (.sits_from_kohonen (data.tb, kohonen_obj, return_members))
}
#' @title Cluster a set of time series using kohonen clustering followed by a hierarchical clustering over resulting neurons
#' @name .sits_cluster_kohodogram
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Lorena Alves, \email{lorena.santos@@inpe.br}
#'
#' @description Cluster time series in SOM mode followed by a hierqarchical clustering.
#'
#' @references `kohonen` package (https://CRAN.R-project.org/package=kohonen), `dtwclust` package (https://CRAN.R-project.org/package=dtwclust)
#'
#' @param data.tb         a tibble the list of time series to be clustered
#' @param bands           a vector the bands to be clusterized.
#' @param grid_xdim       x dimension of the SOM grid
#' @param grid_ydim       y dimension of the SOM grid
#' @param rlen            the number of times the complete data set will be presented to the SOM grid
#' @param alpha           learning rate, a vector of two numbers indicating the amount of change.
#'                        Default is to decline linearly from 0.05 to 0.01 over rlen updates.
#' @param n_clusters      the number of clusters to be identified
#' @param grouping_method the agglomeration method to be used. Any `hclust` method (see `hclust`).
#' @param return_members  (boolean) should the results be the clusters' members instead of clusters' centroids?
#' @param show            (boolean) should the results be shown?
#' @return clusters.tb a SITS tibble with the clusters
.sits_cluster_kohodogram <- function (data.tb, bands, grid_xdim, grid_ydim, rlen, alpha, n_clusters, grouping_method, return_members, show){

     # get the values of the various time series for this band group
     values.tb <- sits_values (data.tb, bands, format = "bands_cases_dates")

     grid <- kohonen::somgrid(xdim = grid_xdim, ydim = grid_ydim, topo = "rectangular")
     kohonen_obj  <- kohonen::supersom (values.tb, grid=grid,
                                        rlen = rlen, alpha = alpha,
                                        keep.data = TRUE,
                                        dist.fcts = "euclidean")

     # Plot the series and the obtained prototypes
     if (show) .sits_kohonen_show (kohonen_obj)

     # if return_members parameter is TRUE, returns a sits samples table with updated labels else, returns cluster's centroids
     result.tb <- .sits_from_kohonen (data.tb, kohonen_obj, return_members = FALSE)

     ##########################
}
#------------------------------------------------------------------
#' @title Returns a tibble of centroids and its metadata.
#' @name .sits_from_dtwclust
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description reads a list of clusters provided by the dtwclust
#' package and produces a sits table.
#' @references "dtwclust" package (https://CRAN.R-project.org/package=dtwclust)
#'
#' @param data.tb          a tibble with input data of dtwclust.
#' @param clusters         a cluster structure returned from dtwclust.
#' @param min_clu_perc     the minimum percentage of members for a cluster to be valid
#' @param return_members   (boolean) should the results be the clusters' members instead of clusters' centroids?
#' @return result.tb       a SITS table with the clusters or clusters' members
.sits_from_dtwclust <-  function (data.tb, clusters, min_clu_perc, return_members) {

     # # cut time series to fit in one year
     # data.tb <- sits_prune(data.tb)

     # how many clusters have more than 10% of the total samples?
     num <- clusters@clusinfo$size %>%
          .[. > as.integer (min_clu_perc * length(clusters@cluster))] %>%
          length()

     label_prefix <- data.tb[1,]$label[[1]]

     # select only significant centroids
     cluster_ordering <- order(clusters@clusinfo$size, decreasing = TRUE)


     if (return_members){
          # create a table to store the results
          members.tb <- data.tb
          members.tb$label <- paste0(label_prefix, ".", cluster_ordering[clusters@cluster])

          # returns only the members of significant clusters
          result.tb <- members.tb[which(clusters@cluster %in% cluster_ordering[1:num]),]

     } else {
          # select only the significant clusters' centroids
          centroids.lst <- clusters@centroids[cluster_ordering][1:num]

          # create a table to store the results
          result.tb <- sits_table()

          # populates the result table with centroids
          purrr::map2(centroids.lst, cluster_ordering, function (ts, i) {
               new_ts <- dplyr::select(data.tb[1,]$time_series[[1]], Index)
               new_ts <- dplyr::bind_cols(new_ts, tibble::as_tibble(ts))
               result.tb <<- tibble::add_row (result.tb,
                                              longitude    = 0.0,
                                              latitude     = 0.0,
                                              start_date   = data.tb[1,]$start_date[[1]],
                                              end_date     = data.tb[1,]$end_date[[1]],
                                              label        = paste0(label_prefix, ".", i),
                                              coverage     = data.tb[1,]$coverage[[1]],
                                              time_series  = list(new_ts))
          })
     }

     return (result.tb)
}
#------------------------------------------------------------------
#' @title Returns a tibble of centroids and its metadata.
#' @name .sits_from_kohonen
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Lorena Alves, \email{lorena.santos@@inpe.br}
#'
#' @description reads a list of clusters provided by the dtwclust
#' package and produces a sits table.
#' @references `kohonen` package (https://CRAN.R-project.org/package=kohonen)
#'
#' @param data.tb          a tibble with input data of `kohonen`.
#' @param kohonen_obj      a kohonen structure returned from `kohonen::supersom`.
#' @param return_members   (boolean) should the results be the clusters' members instead of clusters' centroids?
#' @return result.tb       a SITS table with the clusters or clusters' members
.sits_from_kohonen <-  function (data.tb, kohonen_obj, return_members) {

     # get label prefix to form sub-class label result
     label_prefix <- data.tb[1,]$label[[1]]

     if (return_members){
          # create a table to store the results
          result.tb <- data.tb
          result.tb$label <- paste0(label_prefix, ".", kohonen_obj$unit.classif)

     } else {
          # select only significant centroids
          neurons_size <- rle(sort(kohonen_obj$unit.classif))
          neurons_ordering <- neurons_size$values[order(neurons_size$length, decreasing = TRUE)]

          # select only the significant clusters' centroids
          neurons.lst <- purrr::map(kohonen_obj$codes, function (ts) ts[neurons_ordering,] %>% t() %>% as.data.frame())

          # populates the result table with centroids
          result_band.lst <- purrr::map2(neurons.lst, names(neurons.lst), function (neurons.df, band) {
               # create a table to store the results
               result_band.tb <- sits_table()

               purrr::map2(neurons.df, neurons_ordering, function (ts, i) {
                    new_ts <- dplyr::select(data.tb[1,]$time_series[[1]], Index)
                    ts.tb <- tibble::as_tibble(ts)
                    names(ts.tb) <- band
                    new_ts <- dplyr::bind_cols(new_ts, ts.tb)
                    result_band.tb <<- tibble::add_row (result_band.tb,
                                                        longitude    = 0.0,
                                                        latitude     = 0.0,
                                                        start_date   = data.tb[1,]$start_date[[1]],
                                                        end_date     = data.tb[1,]$end_date[[1]],
                                                        label        = paste0(label_prefix, ".", i),
                                                        coverage     = data.tb[1,]$coverage[[1]],
                                                        time_series  = list(new_ts))
               })
               return (result_band.tb)
          })

          # merge all bands together
          result.tb <- sits_table()
          result_band.lst %>% purrr::map(function (result_band.tb){
               result.tb <<- sits_merge(result.tb, result_band.tb)
          })
     }

     return (result.tb)
}
#' @title Plots a cluster produced by the dtwclust package
#' @name .sits_dtwclust_show
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description plots a cluster produced by the dtwclust package
#' @references "dtwclust" package (https://CRAN.R-project.org/package=dtwclust)
#' @param clusters     a cluster structure returned from dtwclust.
.sits_dtwclust_show <- function (clusters) {
     # Plot the series and the obtained prototypes
     graphics::plot (clusters, type = "sc")

     # Plot the centroids
     graphics::plot (clusters, type = "centroids")

     # Plot dendogram/default dtwclust plot
     graphics::plot (clusters)

     # print information about the clusters
     .sits_dtwclust_info (clusters)
}

#' @title Prints information about the cluster generated by the dtwclust package
#' @name .sits_dtwclust_info
#'
#' @description The dtwclust package produces a cluster object. This function
#' prints information about this object
#' @references "dtwclust" package (https://CRAN.R-project.org/package=dtwclust)
#' @param clusters   a cluster structure returned from dtwclust.

.sits_dtwclust_info <- function (clusters) {
     cat ("-------------------------------\n")
     # what is the name of the band?
     band <- tools::file_path_sans_ext(names(clusters@centroids)[1])
     cat (paste ("Band(s): ", band, "\n", sep = ""))

     # print the size of each cluster
     df <- clusters@clusinfo
     cat ("Clusters' sizes\n")
     for (i in 1:nrow(df))
          cat (paste ("Cluster ", i,": ", df[i,"size"], "\n", sep = ""))
}
#' @title Plots a map produced by the kohonen package
#' @name .sits_kohonen_show
#' @author Lorena Alves, \email{lorena.santos@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description plots kohonen clusters produced by the kohonen package
#' @references `kohonen` package (https://CRAN.R-project.org/package=kohonen)
#' @param kohonen_obj     a kohonen structure returned from kohonen::supersom.
.sits_kohonen_show <- function (kohonen_obj) {
     # plot neurons in a map
     graphics::plot (kohonen_obj, type = "codes")

     # plot how many members for each neuron
     graphics::plot (kohonen_obj, type = "counts")

     # plot the distances among imediate neuron neighbors
     graphics::plot (kohonen_obj, type = "dist.neighbours")

     # print information about the kohonen
     .sits_kohonen_info (kohonen_obj)
}
#' @title Prints information about the kohonen neurons generated by the kohonen package
#' @name .sits_kohonen_info
#'
#' @description The kohonen package produces a kohonen object. This function
#' prints information about this object
#' @references `kohonen` package (https://CRAN.R-project.org/package=kohonen)
#' @param kohonen_obj   a kohonen object returned from kohonen::supersom.
.sits_kohonen_info <- function (kohonen_obj) {
     # cat ("-------------------------------\n")
     # # what is the name of the band?
     # band <- tools::file_path_sans_ext(names(clusters@centroids)[1])
     # cat (paste ("Band(s): ", band, "\n", sep = ""))
     #
     # # print the size of each cluster
     # df <- clusters@clusinfo
     # cat ("Clusters' sizes\n")
     # for (i in 1:nrow(df))
     #      cat (paste ("Cluster ", i,": ", df[i,"size"], "\n", sep = ""))
}
