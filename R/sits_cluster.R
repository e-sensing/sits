#' @title Cluster a set of satellite image time series
#' @name sits_cluster
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description This function uses package "dtwclust" to do time series clustering.
#' There are four options: "dendogram" (hierarchical clustering), "controids" (positional
#' clustering), "kohonen" (self-organized maps), and "kohonen-dendogram" (self-organized maps fallowed by a dendogram).
#' @references `dtwclust` package (https://CRAN.R-project.org/package=dtwclust), `kohonen` package (https://CRAN.R-project.org/package=kohonen)
#'
#' @param data.tb         a SITS tibble the list of time series to be clustered
#' @param bands           the bands to be clusterized.
#' @param method          string - either 'dendogram', 'centroids', 'kohonen', or 'kohonen-dendogram'.
#' @param n_clusters      the number of clusters to be croped from hierarchical clustering (ignored in `kohonen` method). Default is 2.
#' @param dist_method     A supported distance from proxy's dist, e.g. \code{TWDTW}.
#' @param grouping_method the agglomeration method to be used. Any `hclust` method (see `hclust`) (ignored in `kohonen` method). Default is 'ward.D2'.
#' @param koh_xgrid       x dimension of the SOM grid (used only in `kohonen` or `kohonen-dendogram` methods). Defaul is 5.
#' @param koh_ygrid       y dimension of the SOM grid (used only in `kohonen` or `kohonen-dendogram` methods). Defaul is 5.
#' @param koh_rlen        the number of times the complete data set will be presented to the SOM grid
#' (used only in `kohonen` or `kohonen-denddogram` methods). Default is 100.
#' @param koh_alpha       learning rate, a vector of two numbers indicating the amount of change.
#' Default is to decline linearly from 0.05 to 0.01 over rlen updates.
#' @param return_members  (boolean) should the results be the clusters' members instead of clusters' centroids? Default is FALSE.
#' @param unsupervised    (boolean) should labels be ignored in clustering algorithms?
#' If `return_members` parameter is TRUE, resulting sits table will gain an extra column called `original_label` with all original labels.
#' This column may be useful to measure confusion between clusters' members. Default is FALSE.
#' @param show            (boolean) should the results be shown? Default is TRUE.
#' @param ...             Other arguments to pass to the distance method \code{dist_method}, see \code{\link[dtwclust]{dtwclust}} for details.
#' @return clusters.tb a SITS tibble with the clusters time series or cluster' members time series according to return_member parameter.
#' If return_members are FALSE, the returning SITS table will contain a new collumn called `n_members` informing how many members has each cluster.
#' @export
sits_cluster <- function (data.tb, bands, method = "dendogram", n_clusters = 2, dist_method = "dtw_basic",
                          grouping_method = "ward.D2",koh_xgrid = 5, koh_ygrid = 5, koh_rlen = 100,
                          koh_alpha = c(0.05, 0.01), return_members = FALSE, unsupervised = FALSE, show = TRUE, ...) {

     ensurer::ensure_that(method, (. == "dendogram" || . == "centroids" || . == "kohonen" || . == "kohonen-dendogram"),
                          err_desc = "sits_cluster: valid cluster methods are 'dendogram', 'centroids', 'kohonen', or 'kohonen-dendogram'.")

     # creates the resulting table
     cluster.tb <- sits_table()

     # if unsupervised cluster is enabled, change all input labels.
     if (unsupervised) {
          # if return_members is True, create an new column called `old_label`
          if (return_members)
               data.tb$original_label <- data.tb$label
          data.tb$label <- "Class"
     }

     # how many different labels are there?
     labels <- dplyr::distinct (data.tb, label)$label

     #
     message("Clustering...")

     # add a progress bar
     progress_bar <- utils::txtProgressBar(min = 0, max = length(labels), style = 3)

     # traverse labels
     purrr::map2(labels, seq_along(labels), function (lb, i){
          # filter only those rows with the same labels
          # cut time series to fit in one year
          label.tb <- dplyr::filter (data.tb, label == lb) #%>%
               #sits_prune()  ## FIX-ME! sits_prune() returns a singular time series dates for specific cases!

          # apply the clustering method
          if (method == "dendogram")
               clu.tb <- .sits_cluster_dendogram (label.tb, bands=bands, n_clusters=n_clusters, dist_method=dist_method, grouping_method=grouping_method,
                                                  return_members=return_members, show=show, ...)
          else if (method == "centroids")
               clu.tb <- .sits_cluster_partitional (label.tb, bands=bands, n_clusters=n_clusters, dist_method=dist_method, grouping_method=grouping_method,
                                                    return_members=return_members, show=show, ...)
          else if (method == "kohonen")
               clu.tb <- .sits_cluster_kohonen (label.tb, bands=bands, grid_xdim=koh_xgrid, grid_ydim=koh_ygrid,
                                                rlen=koh_rlen, alpha=koh_alpha, return_members=return_members, show=show)
          else if (method == "kohonen-dendogram")
               clu.tb <- .sits_cluster_kohodogram (label.tb, bands=bands, n_clusters=n_clusters, grouping_method=grouping_method,
                                                   grid_xdim=koh_xgrid, grid_ydim=koh_ygrid,
                                                   rlen=koh_rlen, alpha=koh_alpha, return_members=return_members, show=show)

          # append the result
          cluster.tb <<- dplyr::bind_rows(cluster.tb, clu.tb)

          # update progress bar
          utils::setTxtProgressBar(progress_bar, i)
     })

     close(progress_bar)
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
#' @references `dtwclust` package (https://CRAN.R-project.org/package=dtwclust)
#'
#' @param data.tb         a tibble the list of time series to be clustered
#' @param bands           a vector the bands to be clusterized.
#' @param n_clusters      the number of clusters to be identified
#' @param grouping_method the agglomeration method to be used. Any `hclust` method (see `hclust`).
#' @param return_members  (boolean) should the results be the clusters' members instead of clusters' centroids?
#' @param show            (boolean) should the results be shown?
#' @return clusters.tb a SITS tibble with the clusters
.sits_cluster_dendogram <- function (data.tb, bands, n_clusters, dist_method, grouping_method, return_members, show, ...){
     # get the values of the various time series for this band group
     values.tb <- sits_values (data.tb, bands, format = "cases_dates_bands")
     clusters  <- dtwclust::dtwclust (values.tb,
                                      type     = "hierarchical",
                                      k        = n_clusters,
                                      distance = dist_method,
                                      method   = grouping_method, ...)

     # Plot the series and the obtained prototypes
     if (show) .sits_dtwclust_show (clusters)

     # if return_members parameter is TRUE, returns a sits samples table with updated labels
     # else, returns cluster's centroids
     return (.sits_from_dtwclust (data.tb, clusters = clusters, return_members = return_members))
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
#' @references `dtwclust` package (https://CRAN.R-project.org/package=dtwclust)
#'
#' @param data.tb        a tibble the list of time series to be clustered
#' @param bands          a vector the bands to be clusterized.
#' @param n_clusters     the number of clusters to be identified
#' @param grouping_method the agglomeration method to be used. Any `hclust` method (see `hclust`).
#' @param return_members (boolean) should the results be the clusters' members instead of clusters' centroids?
#' @param show           (boolean) should the results be shown?
#' @return clusters.tb a SITS tibble with the clusters
.sits_cluster_partitional <- function (data.tb, bands, n_clusters, dist_method, grouping_method, return_members, show, ...) {
     # get the values of the various time series for this band group
     values.tb <- sits_values (data.tb, bands, format = "cases_dates_bands")
     clusters  <- dtwclust::dtwclust (values.tb,
                                      type     = "partitional",
                                      k        = n_clusters,
                                      distance = dist_method,
                                      method   = grouping_method,
                                      centroid = "pam",
                                      seed     = 899, ...)

     # Plot the series and the obtained prototypes
     if (show) .sits_dtwclust_show (clusters)

     # if return_members parameter is TRUE, returns a sits samples table with updated labels
     # else, returns cluster's centroids
     return (.sits_from_dtwclust (data.tb, clusters = clusters, return_members = return_members))
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
#' @references `kohonen` package (https://CRAN.R-project.org/package=kohonen)
#'
#' @param data.tb        a tibble the list of time series to be clustered
#' @param bands          a vector the bands to be clusterized.
#' @param grid_xdim      x dimension of the SOM grid
#' @param grid_ydim      y dimension of the SOM grid
#' @param rlen           the number of times the complete data set will be presented to the SOM grid
#' @param alpha          learning rate, a vector of two numbers indicating the amount of change.
#' Default is to decline linearly from 0.05 to 0.01 over rlen updates.
#' @param return_members (boolean) should the results be the clusters' members instead of clusters' centroids?
#' @param show           (boolean) should the results be shown?
#' @return clusters.tb a SITS tibble with the clusters
.sits_cluster_kohonen <- function (data.tb, bands, grid_xdim, grid_ydim, rlen, alpha, return_members, show){

     # recalculate grid dimension if the number of neurons is greater than the number of data input cases
     #### TO-DO: Document this recalculation!
     if ((grid_xdim * grid_ydim) >= nrow(data.tb)){
          grid_xdim <- trunc(max(sqrt(nrow(data.tb) / (grid_xdim * grid_ydim) / 2) * grid_xdim, 2))
          grid_ydim <- trunc(max(sqrt(nrow(data.tb) / (grid_xdim * grid_ydim) / 2) * grid_ydim, 2))
          message(paste("Kohonen grid dimension reduced to", "grid_xdim =", grid_xdim, "and grid_ydim =", grid_ydim))
     }

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
     return (.sits_from_kohonen (data.tb, kohonen_obj = kohonen_obj, return_members = return_members))
}
#' @title Cluster a set of time series using kohonen clustering followed by a hierarchical clustering over resulting neurons
#' @name .sits_cluster_kohodogram
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Lorena Alves, \email{lorena.santos@@inpe.br}
#'
#' @description Cluster time series in SOM mode followed by a hierqarchical clustering. (see `.sits_cluster_kohonen` description.)
#'
#' @references `kohonen` package (https://CRAN.R-project.org/package=kohonen), `dtwclust` package (https://CRAN.R-project.org/package=dtwclust)
#'
#' @param data.tb         a tibble the list of time series to be clustered.
#' @param bands           a vector the bands to be clusterized.
#' @param grid_xdim       x dimension of the SOM grid.
#' @param grid_ydim       y dimension of the SOM grid.
#' @param rlen            the number of times the complete data set will be presented to the SOM grid.
#' @param alpha           learning rate, a vector of two numbers indicating the amount of change.
#' Default is to decline linearly from 0.05 to 0.01 over rlen updates.
#' @param n_clusters      the number of clusters to be identified.
#' @param grouping_method the agglomeration method to be used. Any `hclust` method (see `hclust`).
#' @param return_members  (boolean) should the results be the clusters' members instead of clusters' centroids?
#' @param show            (boolean) should the results be shown?
#' @return result.tb a SITS tibble with the clusters
.sits_cluster_kohodogram <- function (data.tb, bands, grid_xdim, grid_ydim, rlen, alpha, n_clusters, grouping_method, return_members, show){

     # recalculate grid dimension if the number of neurons is greater than the number of data input cases
     #### TO-DO: Document this recalculation!
     if ((grid_xdim * grid_ydim) >= nrow(data.tb)){
          grid_xdim <- trunc(max(sqrt(nrow(data.tb) / (grid_xdim * grid_ydim) / 2) * grid_xdim, 2))
          grid_ydim <- trunc(max(sqrt(nrow(data.tb) / (grid_xdim * grid_ydim) / 2) * grid_ydim, 2))
          message(paste("Kohonen grid dimension reduced to", "grid_xdim =", grid_xdim, "and grid_ydim =", grid_ydim))
     }

     # get the values of the various time series for this band group
     values.tb <- sits_values (data.tb, bands, format = "bands_cases_dates")

     # create a initial random grid
     grid <- kohonen::somgrid(xdim = grid_xdim, ydim = grid_ydim, topo = "rectangular")

     kohonen_obj  <- kohonen::supersom (values.tb, grid=grid,
                                        rlen = rlen, alpha = alpha,
                                        keep.data = TRUE,
                                        dist.fcts = "euclidean")

     # Plot the series and the obtained prototypes
     if (show) .sits_kohonen_show (kohonen_obj)

     # returns kohonen's neurons
     neurons.tb <- .sits_from_kohonen (data.tb, kohonen_obj, return_members = FALSE)

     # proceed with unsupervised hierarchical cluster.
     neurons.tb$label <- tools::file_path_sans_ext(neurons.tb[1,]$label[[1]])

     # pass neurons to dendogram clustering
     clusters.tb <- .sits_cluster_dendogram (neurons.tb, bands = bands, n_clusters = n_clusters,
                                             grouping_method = grouping_method, return_members = return_members, show = FALSE)

     # return a sits table with all input data with new labels
     if (return_members) {
          # set clusters' labels to result data
          result.tb <- data.tb
          result.tb$label <- clusters.tb$label[kohonen_obj$unit.classif]
          result.tb$original_label <- clusters.tb$original_label[kohonen_obj$unit.classif]
     # return a sits table with clusters' centroids
     } else
          result.tb <- dplyr::select(clusters.tb, longitude, latitude, start_date, end_date, label, coverage, time_series, original_label, n_members)

     return (result.tb)
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
#' @param return_members   (boolean) should the results be the clusters' members instead of clusters' centroids?
#' @return result.tb       a SITS table with the clusters or clusters' members
.sits_from_dtwclust <-  function (data.tb, clusters, return_members) {

     # get prefix label to be used in clusters' labels
     label_prefix <- data.tb[1,]$label[[1]]

     # return a sits table with all input data with new labels
     if (return_members){
          # create a table to store the results
          result.tb <- data.tb

          # create 'original_cluster' collumn
          result.tb$original_label <- result.tb$label

          # apply new labels according to clusters' id
          result.tb$label <- paste0(label_prefix, ".", clusters@cluster)

     # return a sits table with clusters' centroids
     } else {

          # computes num of members for each case. If no previous n_members, initialize with ones.
          if (!any("n_members" %in% names(data.tb)))
               data.tb$n_members <- 1

          # create a table to store the results
          result.tb <- sits_table()

          # return the number of each cluster members
          result.tb <- tibble::add_column(result.tb, original_label = character(), n_members = integer())

          # populates the result table with centroids
          purrr::map2(clusters@centroids, seq(clusters@centroids), function (ts, i) {
               new_ts <- dplyr::select(data.tb[1,]$time_series[[1]], Index)
               new_ts <- dplyr::bind_cols(new_ts, tibble::as_tibble(ts))
               result.tb <<- tibble::add_row (result.tb,
                                              longitude      = 0.0,
                                              latitude       = 0.0,
                                              start_date     = data.tb[1,]$start_date[[1]],
                                              end_date       = data.tb[1,]$end_date[[1]],
                                              label          = paste0(label_prefix, ".", i),
                                              coverage       = data.tb[1,]$coverage[[1]],
                                              time_series    = list(new_ts),
                                              original_label = label_prefix,
                                              n_members      = sum(data.tb$n_members[which(clusters@cluster == i)], na.rm = TRUE))
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

          # create 'original_cluster' column
          result.tb$original_label <- result.tb$label

          # assign new labels
          result.tb$label <- paste0(label_prefix, ".", kohonen_obj$unit.classif)

     } else {
          # get num of neurons' members
          neurons_size <- rle(sort(kohonen_obj$unit.classif))$lengths

          # unpack output data provided by kohonen_obj
          neurons.lst <- purrr::map(kohonen_obj$codes, function (ts) ts %>% t() %>% as.data.frame())

          # populates the result table with centroids
          # traverse neurons bands
          result_band.lst <- purrr::map2(neurons.lst, names(neurons.lst), function (neurons.df, band) {
               # create a table to store the results
               result_band.tb <- sits_table()

               # return the number of each cluster members
               result_band.tb <- tibble::add_column(result_band.tb, original_label = character(), n_members = integer())

               # traverse neurons time series
               purrr::map2(neurons.df, seq_along(neurons.df), function (ts, i) {

                    # populates result
                    new_ts <- dplyr::select(data.tb[1,]$time_series[[1]], Index)
                    ts.tb <- tibble::as_tibble(ts)
                    names(ts.tb) <- band
                    new_ts <- dplyr::bind_cols(new_ts, ts.tb)
                    result_band.tb <<- tibble::add_row (result_band.tb,
                                                        longitude      = 0.0,
                                                        latitude       = 0.0,
                                                        start_date     = data.tb[1,]$start_date[[1]],
                                                        end_date       = data.tb[1,]$end_date[[1]],
                                                        label          = paste0(label_prefix, ".", i),
                                                        coverage       = data.tb[1,]$coverage[[1]],
                                                        time_series    = list(new_ts),
                                                        original_label = label_prefix,
                                                        n_members      = neurons_size[i])
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

#' @title compare clusters against original labels and computes a segregation matrix.
#' @name sits_cluster_segregation
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Given a sits table with `original_label` column, computes a segregation matrix
#' between the original labels (`original_label` column) and new labels. This is useful
#' to analyse the separability of samples for a given clustering algorithm.
#'
#' @param  data.tb        a SITS table with the samples to be validated
#' @return result.tb      a tibble with segregation matrix.
#' @export
sits_cluster_segregation <- function (data.tb){
     ensurer::ensure_that(data.tb, !purrr::is_null(.),
                          err_desc = "sits_cluster_segregation: SITS table not provided")
     # do the input data have the `original_label` column?
     ensurer::ensure_that(data.tb, "original_label" %in% colnames(data.tb),
                          err_desc = "sits_cluster_segregation: informed SITS table has not an `original_label` column.")

     result.tb <- data.tb %>%
          dplyr::group_by(original_label, label) %>%
          dplyr::summarise(count = n()) %>%
          tidyr::spread(key = label, value = count) %>%
          dplyr::ungroup()

     return (result.tb)
}

#' @title computes a segregation measure from a clusterized SITS table data.
#' @name sits_segregation_measure
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Computes an measure of seggregation from SITS table based on mean relative Shannon Entropy,
#' where maximum entropy is 1.0 and minimum entropy is 0.0, from each cluster label against
#' original_label occorrences in it. To obtain this measure, first we compute Shannon
#' information entropy from counts of each cluster label. To get the relative entropy, we
#' divide this value by the maximum entropy. After that, we compute the weighted mean relative
#' to the frequency of each cluster label. The final values represents an average of
#' maximum entropy fraction. This is useful to assess the separability of samples for a given clustering algorithm.
#'
#' @param  data.tb        a SITS table with the samples to be validated
#' @param  per_cluster    (boolean) should return a total average segregation measure, or a per cluster measure?
#' @return result         a segregation measure.
#' @export
sits_segregation_measure <- function (data.tb, per_cluster = FALSE){
     ensurer::ensure_that(data.tb, !purrr::is_null(.),
                          err_desc = "sits_segregation_measure: SITS table not provided")
     # do the input data have the `original_label` column?
     ensurer::ensure_that(data.tb, "original_label" %in% colnames(data.tb),
                          err_desc = "sits_segregation_measure: informed SITS table has not an `original_label` column.")

     # do we have at least two original labels?
     labels_count <- length(table(data.tb$original_label))
     if (labels_count == 1)
          return (0.0)

     result.tb <- data.tb %>%
          dplyr::group_by(original_label, label) %>%
          dplyr::summarise(count = n()) %>%
          dplyr::ungroup() %>%
          dplyr::group_by(label) %>%
          dplyr::summarise(segr = entropy::entropy(count) / log(labels_count) * sum(count, na.rm = TRUE) / nrow(data.tb))

     if (per_cluster)
          return (result.tb)

     return (dplyr::summarise(result.tb, mean_segr = sum(segr, na.rm = TRUE)) %>% .$mean_segr)

}
