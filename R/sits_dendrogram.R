#' @title Compute a dendrogram using hierarchical clustering
#' @name sits_dendrogram
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Cluster time series in hierarchical mode. Hierarchical clustering, as its name suggests,
#' is an algorithm that tries to create a hierarchy of groups in which, as the level in the hierarchy increases, clusters are created by merging
#' the clusters from the next lower level, such that an ordered sequence of groupings is obtained.
#' The similarity measure used to group time series in a cluster is the dtw metric.
#' The procedure is deterministic, so it will always give the same
#' result for a chosen set of similarity measures (see \code{\link[dtwclust]{tsclust}}).
#'
#' @references `dtwclust` package (https://CRAN.R-project.org/package=dtwclust)
#'
#' @param data.tb         A SITS tibble to be used to generate the dendrogram.
#' @param bands           a vector the bands to be clusterized.
#' @param dist_method     A supported distance from proxy's dist, e.g. \code{TWDTW}.
#' @param linkage         the agglomeration method to be used. Any `hclust` method (see `hclust`) Default is 'ward.D2'..
#' @param  ...            any additional parameters to be passed to dtwclust::tsclust() function
#' @return dendro.obj     a full dendrogram tree for data analysis
#'
#' @examples
#' \donttest{
#' # Load the "dtwclust" package
#' library (dtwclust)
#' # load a simple data set with two classes
#' data(cerrado_2classes)
#' # calculate the dendrogram
#' clusters <- sits_dendrogram (cerrado_2classes, bands = c("ndvi", "evi"))
#' # plot the dendrogram
#' sits_plot_dendrogram (cerrado_2classes, clusters)
#' }
#' @export
sits_dendrogram <- function (data.tb, bands = NULL,
                             dist_method = "dtw_basic",
                             linkage = "ward.D2", ...){

    # verifies if dtwclust package is installed
    if (!requireNamespace("dtwclust", quietly = TRUE)) {
        stop("dtwclust needed for this function to work. Please install it.", call. = FALSE)
    }

    # does the input data exist?
    .sits_test_tibble(data.tb)

    # if no bands informed, get all bands available in SITS tibble
    if (purrr::is_null(bands))
        bands <- sits_bands(data.tb)

    # get the values of the time series
    values.tb <- sits_values (data.tb, bands, format = "cases_dates_bands")

    # call dtwclust and get the resulting dendrogram
    dendro.obj  <- dtwclust::tsclust (values.tb,
                                      type     = "hierarchical",
                                      k        = NROW (data.tb),
                                      distance = dist_method,
                                      control  = dtwclust::hierarchical_control(method = linkage), ...)

    # return the dendrogram
    return (dendro.obj)
}

#' @title Compute validity indexes to a range of cut height
#' @name sits_dendro_bestcut
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description reads a dendrogram object and its corresponding SITS tibble and
#' computes the best number of clusters that maximizes the adjusted Rand index.
#'
#' @references
#' Lawrence Hubert and Phipps Arabie. Comparing partitions.
#' Journal of Classification, 2, p.193--218, 1985.
#'
#' See \link[flexclust]{randIndex} for implementation details.
#'
#' @param data.tb          The same SITS tibble used to generate `dendro.obj`.
#' @param dendro.obj       a dendrogram object returned from \code{\link[sits]{sits_dendrogram}}.
#'
#' @return
#' A vector with the best number of clusters (k) and its respective heigh.
#'
#' @examples
#' \donttest{
#' # load a simple data set with two classes
#' data(cerrado_2classes)
#' # calculate the dendrogram
#' dendro.obj <- sits_dendrogram (cerrado_2classes, bands = c("ndvi", "evi"))
#' # include the cluster info in the SITS tibble
#' sits_dendro_bestcut (cerrado_2classes, dendro.obj)
#' }
#'
#' @export
sits_dendro_bestcut <-  function (data.tb, dendro.obj) {

    # compute range
    k_range <- seq(1, length(dendro.obj$height))

    # compute ARI for each k
    ari.vec <-
        k_range %>%
        purrr::map(function(k) {
            flexclust::randIndex(stats::cutree(dendro.obj, k = k),
                                 factor(data.tb$label),
                                 correct = TRUE)
        }) %>%
        unlist()

    # get the best ARI result
    k_result <- k_range[which.max(ari.vec)]

    # compute each height corresponding to `k_result`
    h_result <- c(0, dendro.obj$height)[length(dendro.obj$height) - k_result + 2]

    # create a named vector and return
    result.vec <- structure(c(k_result, h_result), .Names = c("k", "height"))
    return (result.vec)
}
