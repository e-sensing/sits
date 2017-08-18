#------------------------------------------------------------------
#' @title Cuts a cluster tree produced by sits_dendrogram
#' @name sits_cluster
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description reads a list of clusters provided by the dtwclust
#' package and produces a sits table.
#' @references "dtwclust" package (https://CRAN.R-project.org/package=dtwclust)
#'
#' @param data.tb          a tibble with input data of dtwclust.
#' @param clusters         a cluster structure returned from dtwclust.
#' @param k                the desired number of clusters
#' @param height           the height of dendrogram cutting
#' @return result.tb       a SITS table with the clusters or clusters' members
#' @export
sits_cluster <-  function (data.tb, clusters, k = NULL, height = NULL) {

    # create a table to store the results
    result.tb <- data.tb

    # cut the tree
    result.tb$cluster <- stats::cutree(clusters, k, height)

    return (result.tb)
}

