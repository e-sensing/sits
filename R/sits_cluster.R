#------------------------------------------------------------------
#' @title Cuts a cluster tree produced by sits_dendrogram
#' @name sits_cluster
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Takes a SITS tibble and produces a sits tibble with an added "cluster" column
#' This is done in several steps:
#' 1. Calculation of the dendogram
#' 2. Selection of the validity index for best cluster using the adjusted Rand Index
#' 3. Cut the dendogram using the chosen validity index
#'
#' @references "dtwclust" package (https://CRAN.R-project.org/package=dtwclust)
#'
#' @param data.tb          A tibble with input data of dtwclust.
#' @param bands           Bands to be used in the clustering
#' @param dist_method     String with one of the supported distance from proxy's dist, e.g. \code{TWDTW}.
#' @param linkage         String with agglomeration method to be used. Can be any `hclust` method (see `hclust`). Default is 'ward.D2'.
#' @param k               The desired number of clusters (overrides default value)
#' @param colors          A color scheme as showed in `sits_color_name` function.
#' @param silent          Should output be provided?
#' @param  ...            Any additional parameters to be passed to dtwclust::tsclust() function.
#' @return A tibble with the clusters or clusters' members.
#'
#' @examples
#' \donttest{
#' # Load the "dtwclust" package
#' # library(dtwclust)
#' # load a simple data set with two classes
#' data(cerrado_2classes)
#' # calculate the dendrogram and the best clusters
#' cluster.tb <- sits_cluster (cerrado_2classes, bands = c("ndvi", "evi"))
#' }
#' @export
sits_cluster <-  function(data.tb, bands = NULL, dist_method = "dtw_basic",
                          linkage = "ward.D2",
                          k = NULL,
                          colors = "RdYlGn",
                          silent = FALSE, ...) {

    # backward compatibility
    if ("coverage" %in% names(data.tb))
        data.tb <- .sits_tibble_rename(data.tb)


    # create a tibble to store the results
    result.tb <- data.tb

    # calculate the dendrogram
    if (!silent) message("calculating dendrogram...")
    dendro.obj <- .sits_dendrogram(data.tb, bands, dist_method, linkage, ...)

    # find the best cut for the dendrogram
    if (!silent) message("finding the best cut for the dendrogram...")
    cut.vec <- .sits_dendro_bestcut(data.tb, dendro.obj)
    msg1 <- paste0("best number of clusters = ", cut.vec["k"])
    msg2 <- paste0("best height for cutting the dendrogram = ", cut.vec["height"])
    if (!silent) message(msg1)
    if (!silent) message(msg2)

    # cut the tree (user-defined value overrides default)
    if (!silent) message("cutting the tree...")
    if (!purrr::is_null(k)) {
        if (k != cut.vec["k"]) {
            msg_k <- paste0("Caveat: desired number of clusters (", k, ") overrides best value")
            if (!silent) message(msg_k)
            cut.vec["k"] <- k
            cut.vec["height"] <- c(0, dendro.obj$height)[length(dendro.obj$height) - k + 2]
        }
    }
    result.tb$cluster <- stats::cutree(dendro.obj, cut.vec["k"], cut.vec["height"])

    # plot the dendrogram
    if (!silent) message("Plotting dendrogram...")
    .sits_plot_dendrogram(data.tb, dendro.obj, cut.vec["height"], colors)

    # return the result
    if (!silent) message("result is a tibble with cluster indexes...")
    return(result.tb)
}

#' @title Cluster validity indices
#' @name .sits_cluster_validity
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Compute different cluster validity indices. This function needs
#' as input a sits tibble with `cluster` column.
#' It is a front-end to `dtwclust::cvi` function. That function computes five indices:
#' 1) adjusted Rand index; 2) Rand index; 3) Jaccard index; 4) Fowlkes-Mallows; and 5) Variation of Information index
#' Please refer to the documentation in that package for more details.
#'
#' @references "dtwclust" package (https://CRAN.R-project.org/package=dtwclust)
#'
#' @param data.tb   A tibble with `cluster` column.
#'
#' @return A vector with four external validity indices.
#'
.sits_cluster_validity <-  function(data.tb) {
    # verifies if dtwclust package is installed
    if (!requireNamespace("dtwclust", quietly = TRUE)) {
        stop("dtwclust needed for this function to work. Please install it.", call. = FALSE)
    }

    # is the input data the result of a cluster function?
    ensurer::ensure_that(data.tb, "cluster" %in% names(.),
                         err_desc = "sits_cluster_validity: input data does not contain cluster column")

    # compute CVIs and return
    result.vec <- dtwclust::cvi(a = factor(data.tb$cluster),
                                b = factor(data.tb$label),
                                type = "external",
                                log.base = 10)
    return(result.vec)
}

#' @title Cluster contigency table
#' @name sits_cluster_frequency
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Computes the contingency table between labels and clusters.
#' This function needs as input a sits tibble with `cluster` column.
#'
#' @param data.tb          A tibble with `cluster` column.
#' @return A matrix containing all frequencies of labels in clusters.
#' @examples
#' \donttest{
#' # Load the "dtwclust" package
#' # library(dtwclust)
#' # load a simple data set with two classes
#' data(cerrado_2classes)
#' # create clusters by cutting a dendrogram
#' clusters.tb <- sits_cluster(cerrado_2classes, bands = c("ndvi", "evi"))
#' # show clusters samples frequency
#' sits_cluster_frequency(clusters.tb)
#' }
#' @export
sits_cluster_frequency <-  function(data.tb) {
    # is the input data the result of a cluster function?
    ensurer::ensure_that(data.tb, "cluster" %in% names(.),
                         err_desc = "sits_cluster_contigency: input data does not contain cluster column")

    # compute frequency table
    result.mtx <- table(data.tb$label, data.tb$cluster)

    # compute total row and col
    result.mtx <- stats::addmargins(result.mtx, FUN = list(Total = sum), quiet = TRUE)
    return(result.mtx)
}

#' @title Cluster cleaner
#' @name sits_cluster_clean
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Removes sits tibble samples of labels that are minority in each cluster.
#'
#' @param data.tb           Tibble with `cluster` column.
#' @return A tibble with all selected samples.
#' @examples
#' \donttest{
#' # Load the "dtwclust" package
#' # library(dtwclust)
#' # load a simple data set with two classes
#' data(cerrado_2classes)
#' # calculate the dendrogram and the best clusters
#' cluster.tb <- sits_cluster (cerrado_2classes, bands = c("ndvi", "evi"))
#' # show clusters samples frequency
#' sits_cluster_frequency(cluster.tb)
#' # remove cluster 3 from the samples
#' clusters_new.tb <- dplyr::filter(cluster.tb, cluster != 3)
#' # show clusters samples frequency
#' sits_cluster_frequency(clusters_new.tb)
#' # clean all remaining clusters
#' cleaned.tb <- sits_cluster_clean(clusters_new.tb)
#' # show clusters samples frequency
#' sits_cluster_frequency(cleaned.tb)
#' }
#' @export
sits_cluster_clean <- function(data.tb) {
    # verify if data.tb has data
    .sits_test_tibble(data.tb)

    # is the input data the result of a cluster function?
    ensurer::ensure_that(data.tb, "cluster" %in% names(.),
                         err_desc = "sits_cluster_clean: input data does not contain cluster column")

    # compute frequency table
    result.mtx <- table(data.tb$label, data.tb$cluster)

    # list of the clusters of the data table
    num_cls <- unique(data.tb$cluster)
    # get the labels of the data
    lbs <- unique(data.tb$label)
    # for each cluster, get the label with the maximum number of samples
    lbs_max <- lbs[as.vector(apply(result.mtx, 2, which.max))]

    # compute the resulting table
    row.lst <- purrr::map2(lbs_max, num_cls, function(lb, cl)
        {
            partial.tb <- dplyr::filter(data.tb, label == lb & cluster == cl)
            return(partial.tb)
        })
    result.tb <- dplyr::bind_rows(row.lst)
    return(result.tb)
}
