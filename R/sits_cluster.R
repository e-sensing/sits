#------------------------------------------------------------------
#' @title Cuts a cluster tree produced by sits_dendrogram
#' @name sits_cluster
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description reads a list of clusters provided by the dtwclust
#' package,  and produces a sits tibble with an added "cluster" column
#' @references "dtwclust" package (https://CRAN.R-project.org/package=dtwclust)
#'
#' @param data.tb          a tibble with input data of dtwclust.
#' @param clusters         a cluster structure returned from dtwclust.
#' @param k                the desired number of clusters
#' @param height           the desired height to cut the dendrogram. At least one of k or height must be specified, k overrides height if both are given.
#' @return result.tb       a SITS tibble with the clusters or clusters' members
#'
#' @examples
#' \donttest{
#' # Load the "dtwclust" package
#' library(dtwclust)
#' # load a simple data set with two classes
#' data(cerrado_2classes)
#' # calculate the dendrogram
#' clusters <- sits_dendrogram (cerrado_2classes, bands = c("ndvi"))
#' # include the cluster info in the SITS tibble
#' clustered.tb <- sits_cluster (cerrado_2classes, clusters, k = 6)
#' }
#'
#' @export
sits_cluster <-  function(data.tb, clusters, k = NULL, height = NULL) {

    #verifies if either k or height were informed
    ensurer::ensure_that(k, !(is.null(.) & is.null(height)),
                         err_desc = "sits_cluster: you must provide at least k or height")

    # create a tibble to store the results
    result.tb <- data.tb

    # cut the tree
    result.tb$cluster <- stats::cutree(clusters, k, height)

    return(result.tb)
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
#' @param type             the type of analysis to perform
#' @return result.vec      vectors with chosen CVIs
#' @export
sits_cluster_validity <-  function(data.tb, type = "valid") {

    # verifies if dtwclust package is installed
    if (!requireNamespace("dtwclust", quietly = TRUE)) {
        stop("dtwSat needed for this function to work. Please install it.", call. = FALSE)
    }

    # is the input data the result of a cluster function?
    ensurer::ensure_that(data.tb, "cluster" %in% names(.), err_desc = "sits_cluster_validity: input data does not contain cluster column")

    # rename clusters to correspond the more frequent class
    max_labels_names.vec <-
        apply(sits_cluster_frequency(data.tb), 2, which.max)
    new_clusters_names.vec <-
        sits_labels(data.tb)$label[max_labels_names.vec]
    data.tb <- sits_cluster_rename(data.tb, new_clusters_names.vec)

    # compute CVIs and return
    result.vec <- dtwclust::cvi(a = factor(data.tb$cluster), b = factor(data.tb$label), type = "external", log.base = 10)

    return(result.vec)
}

#' @title Cluster frequency
#' @name sits_cluster_frequency
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Computes the frequency of labels in each cluster.
#' This function needs as input a SITS tibble with `cluster` column.
#'
#' @param data.tb          a SITS tibble with `cluster` column.
#' @return result.mtx      matrix containing all frequencies of labels in clusters
#' @export

sits_cluster_frequency <-  function (data.tb) {

    # is the input data the result of a cluster function?
    ensurer::ensure_that(data.tb, "cluster" %in% names (.), err_desc = "sits_cluster_frequency: input data does not contain cluster column")

    # compute frequency table
    result.mtx <- table(data.tb$label, data.tb$cluster)

    # compute total row and col
    result.mtx <- stats::addmargins(result.mtx, FUN = list(Total = sum), quiet = TRUE)
    return (result.mtx)
}

#' @title Cluster cleaner
#' @name sits_cluster_cleaner
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Removes SITS tibble samples of labels that are minority in each cluster.
#' The function sets a percentage threshold for removal of samples of a label
#' from a cluster to be excluded from that cluster
#' If the method "intracluster" is chosen, the "min_perc" parameter
#' controls the relative percentage of labels inside each cluster. If samples
#' of a given label inside one cluster are below this limit, then the samples of this label are
#' removed from the clustar. If the method intercluster" is set, the "min_perc"
#' parameter is used a threshold that controls the minimum
#' percentage of each label in all clusters. If the percentage of samples of a label is a cluster is less
#' than this value, all samples from this label are removed from the cluster.
#'
#'
#' @param data.tb           a SITS tibble with `cluster` column.
#' @param min_perc          The minimum percentage of label inside a cluster for the label to remain in cluster.
#' @param method            Either "intracluster" or "intercluster"
#' @return result.tb        a SITS tibble with all selected samples
#' @export
sits_cluster_cleaner <-  function(data.tb, min_perc = 0.05, method = "intracluster") {

    # verify if data.tb has data
    .sits_test_tibble(data.tb)

    ensurer::ensure_that(method, (.) %in% c("intercluster", "intracluster"),
                         err_desc = "sits_cluster_cleaner: chosen method is invalid")

    # is the input data the result of a cluster function?
    ensurer::ensure_that(data.tb, "cluster" %in% names (.), err_desc = "sits_cluster_cleaner: input data does not contain cluster column")

    # compute frequency table
    result.mtx <- table(data.tb$label, data.tb$cluster)

    if (method == "intracluster")
        # compute frequency in each cluster
        # compute relative frequency
        freq.mtx <- prop.table(result.mtx, margin = 2)
    else
        # compute frequency in each label
        freq.mtx <- prop.table(result.mtx, margin = 1)

    # get those indexes whose labels represents more than `min_perc`
    index.mtx <- which(freq.mtx[1:NROW(freq.mtx),1:NCOL(freq.mtx)] > min_perc, arr.ind = TRUE, useNames = TRUE)

    # return only those samples that satisfies the `min_perc` condition
    filter_condition <- paste0(purrr::map2(rownames(index.mtx), index.mtx[,2],
                                               function(lb, clu) paste0("label=='", lb, "' & cluster==", clu)),
                                   collapse = " | ")

    # filter result and return
    result.tb <- dplyr::filter_(data.tb, filter_condition)
    return (result.tb)
}

#' @title Cluster rename
#' @name sits_cluster_rename
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Rename the SITS tibble clusters names according to a given value.
#' This function needs as input a SITS tibble with `cluster` column.
#'
#' @param data.tb          a SITS tibble with `cluster` column.
#' @param values           character vector informing all cluster names. If unnamed vector is informed,
#'                         the index of each name will be treated as cluster code
#' @return result.tb       SITS tibble with relabeled samples
#' @export
sits_cluster_rename <-  function (data.tb, values) {

    # is the input data the result of a cluster function?
    ensurer::ensure_that(data.tb, "cluster" %in% names(.),
                         err_desc = "sits_cluster_names: input data does not contain cluster column")

    # verify if the informed cluster names has the same length of clusters names
    ensurer::ensure_that(data.tb, length(base::unique(.$cluster)) == length(values),
                         err_desc = "sits_cluster_name: informed names has length different of the number of clusters")

    # compute new clusters names
    data_cluster_names.vec <- values[data.tb$cluster] %>% unlist()

    # relabel result and return
    result.tb <- data.tb
    result.tb$cluster <- data_cluster_names.vec
    return (result.tb)
}
