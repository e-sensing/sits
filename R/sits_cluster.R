#' @title Find clusters in time series samples
#' @name sits_cluster_dendro
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#'
#' @description These functions support hierarchical agglomerative clustering in
#' sits. They provide support from creating a dendrogram and using it for
#' cleaning samples.
#'
#' \code{link[sits]{sits_cluster_dendro()}} takes a tibble with time series and
#' produces a sits tibble with an added "cluster" column. The function first
#' calculates a dendrogram and obtains a validity index for best clustering
#' using the adjusted Rand Index. After cutting the dendrogram using the chosen
#' validity index, it assigns a cluster to each sample.
#'
#' \code{link[sits]{sits_cluster_frequency()}} computes the contingency
#' table between labels
#' and clusters and produces a matrix.
#' Its input is a tibble produced by \code{link[sits]{sits_cluster_dendro()}}.
#'
#' \code{link[sits]{sits_cluster_clean()}} takes a tibble with time series
#' that has an additional `cluster` produced by
#' \code{link[sits]{sits_cluster_dendro()}}
#' and removes labels that are minority in each cluster.
#'
#' @references "dtwclust" package (https://CRAN.R-project.org/package=dtwclust)
#'
#' @param samples         Tibble with input set of time series (class "sits").
#' @param bands           Bands to be used in the clustering
#'                        (character vector)
#' @param dist_method     One of the supported distances (single char vector)
#'                        "dtw": DTW with a Sakoe-Chiba constraint.
#'                        "dtw2": DTW with L2 norm and Sakoe-Chiba constraint.
#'                        "dtw_basic": A faster DTW with less functionality.
#'                        "lbk": Keogh's lower bound for DTW.
#'                        "lbi": Lemire's lower bound for DTW.
#' @param linkage         Agglomeration method to be used (single char vector)
#'                        One of "ward.D", "ward.D2", "single", "complete",
#'                        "average", "mcquitty", "median" or "centroid".
#' @param k               Desired number of clusters (overrides default value)
#' @param palette         Color palette as per `grDevices::hcl.pals()` function.
#' @param  ...            Additional parameters to be passed
#'                        to dtwclust::tsclust() function.
#' @return                Tibble with "cluster" column (class "sits_cluster").
#'
#' @note
#' Please refer to the sits documentation available in
#' <https://e-sensing.github.io/sitsbook/> for detailed examples.
#' @examples
#' if (sits_run_examples()) {
#'     # default
#'     clusters <- sits_cluster_dendro(cerrado_2classes)
#'     # with parameters
#'     clusters <- sits_cluster_dendro(cerrado_2classes,
#'                 bands = "NDVI", k = 5)
#' }
#'
#' @export
sits_cluster_dendro <- function(samples,
                                bands = NULL,
                                dist_method = "dtw_basic",
                                linkage = "ward.D2",
                                k = NULL,
                                palette = "RdYlGn",
                                ...) {
    .check_set_caller("sits_cluster_dendro")
    # needs package dtwclust
    .check_require_packages("dtwclust")
    # verify if data is OK
    .check_samples_train(samples)
    # bands in sits are uppercase
    bands <- .default(bands, .samples_bands(samples))
    bands <- .tibble_bands_check(samples, bands)
    # check k (number of clusters)
    if (.has(k)) {
        .check_int_parameter(k, min = 2,  max = 200)
    }
    # check distance method
    .check_dist_method(dist_method)
    # check linkage
    .check_linkage_method(linkage)
    # check palette
    .check_palette(palette)
    # calculate the dendrogram object
    cluster <- .cluster_dendrogram(
        samples = samples,
        bands = bands,
        dist_method = dist_method,
        linkage = linkage, ...
    )
    # find the best cut for the dendrogram
    best_cut <- .cluster_dendro_bestcut(samples, cluster)
    message(.conf("messages", "sits_cluster_dendro_best_number"),
            best_cut[["k"]]
    )
    message(.conf("messages", "sits_cluster_dendro_best_height"),
            best_cut[["height"]]
    )
    # cut the tree (user-defined value overrides default)
    k <- .default(k, best_cut[["k"]])
    if (k != best_cut[["k"]]) {
        message(.conf("messages", "sits_cluster_dendro_best_cut"))
        best_cut[["k"]] <- k
        best_cut[["height"]] <-
            c(0, cluster[["height"]])[length(cluster[["height"]]) - k + 2]
    }
    samples[["cluster"]] <- stats::cutree(
        cluster,
        best_cut[["k"]],
        best_cut[["height"]]
    )
    # change the class
    class(samples) <- c("sits_cluster", class(samples))
    # plot the dendrogram
    plot(
        x = samples,
        cluster = cluster,
        cutree_height = best_cut[["height"]],
        palette = palette
    )
    return(samples)
}
#'
#' @title Show label frequency in each cluster produced by dendrogram analysis
#' @name sits_cluster_frequency
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @param samples         Tibble with input set of time series with additional
#'                        cluster information produced
#'                        by \code{link[sits]{sits_cluster_dendro}}.
#' @return                A matrix containing frequencies
#'                        of labels in clusters.
#' @examples
#' if (sits_run_examples()) {
#'     clusters <- sits_cluster_dendro(cerrado_2classes)
#'     freq <- sits_cluster_frequency(clusters)
#'     freq
#' }
#' @export
sits_cluster_frequency <- function(samples) {
    # set caller to show in errors
    .check_set_caller("sits_cluster_frequency")
    # is the input data the result of a cluster function?
    .check_samples_cluster(samples)
    # compute frequency table (matrix)
    result <- table(samples[["label"]], samples[["cluster"]])
    # compute total row and col
    stats::addmargins(result,
        FUN = list(Total = sum),
        quiet = TRUE
    )
}

#' @title Removes labels that are minority in each cluster.
#' @name sits_cluster_clean
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @description Takes a tibble with time series
#' that has an additional `cluster` produced by
#' \code{link[sits]{sits_cluster_dendro()}}
#' and removes labels that are minority in each cluster.
#'
#' @param samples         Tibble with set of time series with additional
#'                        cluster information produced
#'                        by \code{link[sits]{sits_cluster_dendro()}}
#' @return                Tibble with time series (class "sits")
#' @examples
#' if (sits_run_examples()) {
#'     clusters <- sits_cluster_dendro(cerrado_2classes)
#'     freq1 <- sits_cluster_frequency(clusters)
#'     freq1
#'     clean_clusters <- sits_cluster_clean(clusters)
#'     freq2 <- sits_cluster_frequency(clean_clusters)
#'     freq2
#' }
#' @export
sits_cluster_clean <- function(samples) {
    # set caller to show in errors
    .check_set_caller("sits_cluster_clean")
    # is the input data the result of a cluster function?
    .check_samples_cluster(samples)
    # compute frequency table (matrix)
    result <- table(samples[["label"]], samples[["cluster"]])
    # list of number of clusters
    num_cls <- unique(samples[["cluster"]])
    # get the labels of the data
    lbs <- unique(samples[["label"]])
    # for each cluster, get the label with the maximum number of samples
    lbs_max <- lbs[as.vector(apply(result, 2, which.max))]
    # compute the resulting table
    purrr::map2_dfr(
        lbs_max, num_cls,
        function(lb, cl) {
            dplyr::filter(
                samples,
                .data[["label"]] == lb,
                .data[["cluster"]] == cl
             )
        }
    )
}
