#------------------------------------------------------------------
#' @title Cuts a cluster tree produced by sits_dendrogram
#' @name sits_cluster
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description reads a list of clusters provided by the dtwclust
#' package and produces a sits tibble.
#' @references "dtwclust" package (https://CRAN.R-project.org/package=dtwclust)
#'
#' @param data.tb          a tibble with input data of dtwclust.
#' @param clusters         a cluster structure returned from dtwclust.
#' @param k                the desired number of clusters
#' @param height           the desired height to cut the dendrogram. At least one of k or height must be specified, k overrides height if both are given.
#' @return result.tb       a SITS tibble with the clusters or clusters' members
#' @export
sits_cluster <-  function (data.tb, clusters, k = NULL, height = NULL) {

    #verifies if either k or height were informed
    ensurer::ensure_that(k, !(is.null(.) & is.null(height)),
                         err_desc = "sits_cluster: you must provide at least k or height")

    # create a tibble to store the results
    result.tb <- data.tb

    # cut the tree
    result.tb$cluster <- stats::cutree(clusters, k, height)

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
#' @param margin           number indicating how to compute relative frequency (1 regarding labels, 2 regarding clusters) (default 2)
#' @return result.mtx      matrix containing all frequencies of labels in clusters
#' @export
sits_cluster_frequency <-  function (data.tb, relative = FALSE, margin = 2) {

    # is the input data the result of a cluster function?
    ensurer::ensure_that(data.tb, "cluster" %in% names (.), err_desc = "sits_cluster_frequency: input data does not contain cluster column")

    # compute frequency table
    result.mtx <- table(data.tb$label, data.tb$cluster)

    # compute relative frequency
    if (relative)
        result.mtx <- prop.table(result.mtx, margin = margin)

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
#' @param min_clu_perc     minimum percentage of labels inside a cluster to remain in cluster.
#' @param min_lab_perc     minimum percentage of labels regarding its total to be keeped in cluster.
#' @return result.tb       a SITS tibble with all selected samples
#' @export
sits_cluster_cleaner <-  function (data.tb, min_clu_perc = 0.0, min_lab_perc = 0.0) {

    # verify if data.tb has data
    .sits_test_tibble (data.tb)

    # is the input data the result of a cluster function?
    ensurer::ensure_that(data.tb, "cluster" %in% names (.), err_desc = "sits_cluster_cleaner: input data does not contain cluster column")

    # compute frequency in each cluster
    freq.mtx <- sits_cluster_frequency(data.tb, relative = TRUE, margin = 2)

    # get those indexes whose labels represents more than `min_clu_perc`
    index.mtx <- which(freq.mtx[1:NROW(freq.mtx) - 1,1:NCOL(freq.mtx) - 1] > min_clu_perc, arr.ind = TRUE, useNames = TRUE)

    # return only those samples that satisfies the `min_clu_perc` condition
    filter_condition_clu <- paste0(purrr::map2(rownames(index.mtx), index.mtx[,2],
                                               function(lb, clu) paste0("label=='", lb, "' & cluster==", clu)),
                                   collapse = " | ")

    # compute frequency in each label
    freq.mtx <- sits_cluster_frequency(data.tb, relative = TRUE, margin = 1)

    # get those indexes whose labels represents more than `min_lab_perc`
    index.mtx <- which(freq.mtx[1:NROW(freq.mtx) - 1,1:NCOL(freq.mtx) - 1] > min_lab_perc, arr.ind = TRUE, useNames = TRUE)

    # return only those samples that satisfies the `min_lab_perc` condition
    filter_condition_lab <- paste0(purrr::map2(rownames(index.mtx), index.mtx[,2],
                                               function(lb, clu) paste0("label=='", lb, "' & cluster==", clu)),
                                   collapse = " | ")

    # if no index selected, return none
    filter_condition <- ifelse(filter_condition_clu != "",
                               ifelse(filter_condition_lab != "", paste0("(", filter_condition_clu, ") & (", filter_condition_lab, ")"), "FALSE"),
                               "FALSE")

    # filter result and return
    result.tb <- dplyr::filter_(data.tb, filter_condition)
    return (result.tb)
}

#' @title Cluster label
#' @name sits_cluster_names
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Renames the labels of SITS tibble samples according to its respective cluster.
#' This function needs as input a SITS tibble with `cluster` column.
#'
#' @param data.tb          a SITS tibble with `cluster` column.
#' @return cluster_names   character vector informing all cluster names. If unnamed vector is informed,
#'                         the index of each name will be treated as cluster code
#' @export
sits_cluster_names <- function (data.tb) {

    # is the input data the result of a cluster function?
    ensurer::ensure_that(data.tb, "cluster" %in% names (.), err_desc = "sits_cluster_names: input data does not contain cluster column")

    # compute clusters names and return
    cluster_names <- sort(base::unique(data.tb$cluster))
    return (cluster_names)
}

#' @title Cluster names
#' @name `sits_cluster_names<-`
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Renames the labels of SITS tibble samples according to its respective cluster.
#' This function needs as input a SITS tibble with `cluster` column.
#'
#' @param data.tb          a SITS tibble with `cluster` column.
#' @param value            character vector informing all cluster names. If unnamed vector is informed,
#'                         the index of each name will be treated as cluster code
#' @return data.tb         SITS tibble with relabeled samples
#' @export
`sits_cluster_names<-` <-  function (data.tb, value) {

    # is the input data the result of a cluster function?
    ensurer::ensure_that(data.tb, "cluster" %in% names (.),
                         err_desc = "sits_cluster_names: input data does not contain cluster column")

    # verify if the informed cluster names has the same length of clusters names
    ensurer::ensure_that(data.tb, length(sits_cluster_names(.)) == length(value),
                         err_desc = "sits_cluster_name: informed names has length different of the number of clusters")

    # compute new clusters names
    data_cluster_names.vec <- value[data.tb$cluster] %>% unlist()

    # relabel result and return
    data.tb$label <- data_cluster_names.vec
    return (data.tb)
}
