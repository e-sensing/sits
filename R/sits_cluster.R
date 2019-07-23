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
#' library(dtwclust)
#' # load a simple data set with two classes
#' data(cerrado_2classes)
#' # calculate the dendrogram and the best cluster
#' # include the cluster info in the sits tibble
#' clustered.tb <- sits_cluster (cerrado_2classes, bands = c("ndvi"))
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

    # # Cluster validity indices
    # message("cluster validity indexes...")
    # val_indexes.vec <- .sits_cluster_validity(result.tb)
    # msg3 <- paste0("adjusted Rand index = ", val_indexes.vec["ARI"])
    # msg4 <- paste0("Rand index = ", val_indexes.vec["RI"])
    # msg5 <- paste0("Jaccard index = ", val_indexes.vec["J"])
    # msg6 <- paste0("Fowlkes-Mallows index = ", val_indexes.vec["FM"])
    # msg7 <- paste0("Variation of Information index = ", val_indexes.vec["VI"])
    # index_msgs <- c(msg3, msg4, msg5, msg6, msg7)
    # index_msgs <- index_msgs %>% purrr::map(function(mg) message(mg))

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
#' library(dtwclust)
#' # load a simple data set with two classes
#' data(cerrado_2classes)
#' # create clusters by cutting a dendrogram
#' clusters.tb <- sits_cluster(cerrado_2classes, bands = c("ndvi"))
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
#' The function removes samples according to a percentage threshold "min_perc".
#' If the method "intracluster" is chosen, the "min_perc" parameter
#' controls the relative percentage of labels inside each cluster. If the number of samples
#' of a given label inside a cluster are below this limit, then all those label samples are
#' removed from that cluster. If the method "intercluster" is set, the "min_perc"
#' parameter is a threshold that controls the minimum
#' percentage of each label in all clusters. If the percentage of samples of a label in a cluster
#' is less than this "min_perc", all thise label samples are removed from that cluster.
#'
#' @param data.tb           Tibble with `cluster` column.
#' @param min_perc          Minimum percentage of label inside a cluster for the label to remain in cluster.
#' @param method            String with "intracluster" or "intercluster" or both
#' @return A tibble with all selected samples.
#' @examples
#' \donttest{
#' # Load the "dtwclust" package
#' library(dtwclust)
#' # load a simple data set with two classes
#' data(cerrado_2classes)
#' # calculate the dendrogram
#' dendro <- sits_dendrogram (cerrado_2classes, bands = c("ndvi"))
#' # create 6 clusters by cutting the dendrogram
#' clusters.tb <- sits_cluster(cerrado_2classes, dendro, k = 6)
#' # show clusters samples frequency
#' sits_cluster_frequency(clusters.tb)
#' # clear those clusters that are less that 25% of each cluster
#' cleaned.tb <- sits_cluster_clean(clusters.tb, min_perc = 0.25)
#' # show clusters samples frequency
#' sits_cluster_frequency(cleaned.tb)
#' }
#' @export
sits_cluster_clean <- function(data.tb, min_perc = 0.05, method = "intracluster") {
    # verify if data.tb has data
    .sits_test_tibble(data.tb)

    ensurer::ensure_that(method, (.) %in% c("intercluster", "intracluster"),
                         err_desc = "sits_cluster_cleaner: chosen method is invalid")

    # is the input data the result of a cluster function?
    ensurer::ensure_that(data.tb, "cluster" %in% names(.),
                         err_desc = "sits_cluster_cleaner: input data does not contain cluster column")

    # compute frequency table
    result.mtx <- table(data.tb$label, data.tb$cluster)

    # compute frequency in each cluster according to the corresponding method
    if (method == "intracluster")
        # compute relative frequency
        freq.mtx <- prop.table(result.mtx, margin = 2)
    else
        # compute frequency in each label
        freq.mtx <- prop.table(result.mtx, margin = 1)

    # get those indexes whose labels represents more than `min_perc`
    index.mtx <- which(freq.mtx > min_perc, arr.ind = TRUE, useNames = TRUE)

    # return only those samples that satisfies the `min_perc` condition
    filter_condition <- paste0(purrr::map2(rownames(index.mtx), index.mtx[,2],
                                            function(lb, clu) paste0("(label=='", lb, "' & cluster==", clu, ")")),
                                collapse = " | ")
    # filter result and return
    result.tb <- dplyr::filter_(data.tb, filter_condition)
    return(result.tb)
}

#' @title Remove cluster with mixed classes
#' @name sits_cluster_remove
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description
#' The function removes clusters according to a percentage threshold "min_perc".
#' A cluster is considered good when the most frequent class has a percentage
#' of samples greater than the "min_perc" threshold.
#'
#' @param data.tb           A tibble with `cluster` column.
#' @param min_perc          Minimum percentage of the most frequent label inside a cluster for the cluster not to be deleted.
#' @return A tibble with all selected samples.
#' @examples
#' \donttest{
#' # Load the "dtwclust" package
#' library(dtwclust)
#' # load a simple data set with two classes
#' data(cerrado_2classes)
#' # calculate the dendrogram
#' dendro <- sits_dendrogram (cerrado_2classes, bands = c("ndvi"))
#' # create 6 clusters by cutting the dendrogram
#' clusters.tb <- sits_cluster(cerrado_2classes, dendro, k = 6)
#' # show clusters samples frequency
#' sits_cluster_frequency(clusters.tb)
#' # clear those clusters that are less that 25% of each cluster
#' cleaned.tb <- sits_cluster_remove(clusters.tb, min_perc = 0.70)
#' # show clusters samples frequency
#' sits_cluster_frequency(cleaned.tb)
#' }
#' @export
sits_cluster_remove <- function(data.tb, min_perc = 0.90) {
    # verify if data.tb has data
    .sits_test_tibble(data.tb)

    # is the input data the result of a cluster function?
    ensurer::ensure_that(data.tb, "cluster" %in% names(.),
                         err_desc = "sits_cluster_cleaner: input data does not contain cluster column")

    # compute frequency table
    result.mtx <- table(data.tb$label, data.tb$cluster)

    # compute relative frequency
    freq.mtx <- prop.table(result.mtx, margin = 2)

    # get those indexes whose labels represents more than `min_perc`
    index.mtx <- which(freq.mtx > min_perc, arr.ind = TRUE, useNames = TRUE)

    # filter result and return
    result.tb <- dplyr::filter(data.tb, cluster %in% as.integer(index.mtx[,"col"]))
    return(result.tb)
}
