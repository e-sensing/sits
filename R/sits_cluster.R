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
#' @return result.tb       a SITS table with the clusters or clusters' members
#' @export
sits_cluster <-  function (data.tb, clusters, k) {

    # create a table to store the results
    result.tb <- data.tb

    # cut the tree
    result.tb$cluster <- stats::cutree(clusters, k)

    return (result.tb)
}

#' @title Cluster validity indices
#' @name sits_cluster_validity
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Compute different cluster validity indices. This function needs
#' as input a SITS tibble with `cluster` column. It is a front-end to
#' `dtwclust::cvi` function. Please refer to the documentation in that package for more details.
#' @references "dtwclust" package (https://CRAN.R-project.org/package=dtwclust)
#'
#' @param data.tb          a SITS tibble with `cluster` column.
#' @param type             character vector indicating which indices are to be computed. (Default "valid")
#' @return result.vec      vectors with chosen CVIs
#' @export
sits_cluster_validity <-  function (data.tb, type = "valid") {

    # is the input data the result of a cluster function?
    ensurer::ensure_that(data.tb, "cluster" %in% names (.), err_desc = "sits_cluster_validity: input data does not contain cluster column")

    # compute CVIs and return
    result.vec <- dtwclust::cvi(a = data.tb$cluster, b = factor(data.tb$label), type = type, log.base = 10)

    return (result.vec)
}

#' @title Cluster frequency
#' @name sits_cluster_frequency
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Computes the frequency of labels in each cluster.
#' This function needs as input a SITS tibble with `cluster` column.
#'
#' @param data.tb          a SITS tibble with `cluster` column.
#' @param relative         (boolean) return relative frequency?
#' @return result.mtx      matrix containing all frequencies of labels in clusters
#' @export
sits_cluster_frequency <-  function (data.tb, relative = FALSE) {

    # is the input data the result of a cluster function?
    ensurer::ensure_that(data.tb, "cluster" %in% names (.), err_desc = "sits_cluster_frequency: input data does not contain cluster column")

    # compute frequency table
    result.mtx <- table(data.tb$label, data.tb$cluster)

    # compute relative frequency
    if (relative)
        result.mtx <- prop.table(result.mtx, margin = 2)

    # compute total row and col
    result.mtx <- stats::addmargins(result.mtx, FUN = list(Total = sum), quiet = TRUE)
    return (result.mtx)
}

#' @title Cluster cleaner
#' @name sits_cluster_cleaner
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Removes SITS tibble samples of labels that are minority in each cluster.
#' This function needs as input a SITS tibble with `cluster` column.
#'
#' @param data.tb          a SITS tibble with `cluster` column.
#' @param min_clu_perc     minimum percentage of representativeness inside a cluster to remain in cluster.
#' @return result.tb       a SITS tibble with all selected samples
#' @export
sits_cluster_cleaner <-  function (data.tb, min_clu_perc) {

    # is the input data the result of a cluster function?
    ensurer::ensure_that(data.tb, "cluster" %in% names (.), err_desc = "sits_cluster_cleaner: input data does not contain cluster column")

    # compute frequency in each cluster
    freq.mtx <- sits_cluster_frequency(data.tb, relative = TRUE)

    # get those indexes whose labels represents more than `min_clu_perc`
    index.mtx <- which(freq.mtx[1:NROW(freq.mtx) - 1,1:NCOL(freq.mtx) - 1] > min_clu_perc, arr.ind = TRUE, useNames = TRUE)

    # return only those samples that satisfies the `min_clu_perc` condition
    filter_condition <- paste0(purrr::map2(rownames(index.mtx), index.mtx[,2],
                                           function(lb, clu) paste0("label=='", lb, "' & cluster==", clu)),
                               collapse = " | ")

    # if no index selescted, return none
    filter_condition <- ifelse(filter_condition != "", filter_condition, "FALSE")

    # filter result and return
    result.tb <- dplyr::filter_(data.tb, filter_condition)
    return (result.tb)
}

#' @title Cluster cleaner
#' @name sits_cluster_relabel
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Renames the labels of SITS tibble samples according to its respective cluster.
#' This function needs as input a SITS tibble with `cluster` column.
#'
#' @param data.tb          a SITS tibble with `cluster` column.
#' @param cluster_names    character vector informing all cluster names. If unnamed vector is informed,
#'                         the index of each name will be treated as cluster code
#' @return result.tb       SITS tibble with relabeled samples
#' @export
sits_cluster_relabel <-  function (data.tb, cluster_names) {

    # is the input data the result of a cluster function?
    ensurer::ensure_that(data.tb, "cluster" %in% names (.), err_desc = "sits_cluster_relabel: input data does not contain cluster column")

    # compute new clusters names
    # if an unnamed vector is given
    if (is.null(names(cluster_names)))
        data_cluster_names.vec <- cluster_names[data.tb$cluster]
    # if a named vector is given
    else
        data_cluster_names.vec <- cluster_names[as.character(data.tb$cluster)]

    # fill not renamed entries with original value
    data_cluster_names.vec <- ifelse(is.na(data_cluster_names.vec), data.tb$cluster, data_cluster_names.vec)
    names(data_cluster_names.vec) <- NULL

    # relabel result and return
    result.tb <- data.tb
    result.tb$label <- data_cluster_names.vec
    return (result.tb)
}
