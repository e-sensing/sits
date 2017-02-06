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
sits_cluster <- function (data.tb, type = "dendogram", n_clusters = 6){
     if (type != "centroids" && type != "centroids") {
          message (paste ("sits_cluster: valid cluster methods are dendogram or centroids", "\n", sep = ""))
     }

     # plot all points joined in time
     if (type == "dendogram") {
          sits_dendogram (data.tb, n_clusters)
     }else if (type == "centroids") {
          sits_centroids(data.tb, n_clusters)
     }
}




