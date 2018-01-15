#' @title Compute a dendrogram using hierarchical clustering
#' @name sits_dendro
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
#' @param data.tb         a tibble the list of time series to be clustered
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
#' clusters <- sits_dendro (cerrado_2classes, bands = c("ndvi"))
#' # plot the dendrogram
#' sits_plot_dendro (cerrado_2classes, clusters)
#' }
#' @export
sits_dendro <- function (data.tb, bands = NULL,
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
#' @name sits_dendro_validity
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description reads a list of clusters provided by the dtwclust
#' package,  and produces a sits tibble with an added "cluster" column
#' @references "dtwclust" package (https://CRAN.R-project.org/package=dtwclust)
#'
#' @param data.tb          a tibble the list of time series to be clustered
#' @param dendro.obj       a dendrogram object returned from `sits_dendro()`.
#' @param from_k           start range of the desired number of clusters to compute indexes
#' @param to_k             stop range of the desired number of clusters to compute indexes
#' @return result.tb       a SITS tibble with the clusters or clusters' members
#' @export
sits_dendro_validity <-  function (data.tb, dendro.obj, from_k, to_k) {

    # compute range
    k_range <- seq(from_k, to_k)

    # compute clusters
    clusters.mx <- stats::cutree(dendro.obj, k = k_range)

    # create a tibble to store the results
    result.tb <- tibble::tibble(k = k_range,
                                height = c(0, dendro.obj$height)[length(dendro.obj$height) - k_range + 2])

    # compute CVIs and prepare it to return
    result.tb <-
        result.tb %>%
        list(
            lapply(
                lapply(
                    lapply(seq_along(k_range),
                           function(i) {
                               dtwclust::cvi(a = clusters.mx[,i],
                                             b = factor(data.tb$label),
                                             type = "external",
                                             log.base = 10)
                           }),
                    function(v) lapply(v, function(e) e)
                ),
                function(l) tibble::as.tibble(l)
            ) %>% dplyr::bind_rows()) %>%
        dplyr::bind_cols()

    return (result.tb)
}
