#' @title Clusters a set of time series
#'        using aglomerative hierarchical clustering
#' @name sits_cluster_dendro
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Takes a SITS tibble and produces a sits tibble
#' with an added "cluster" column.
#' This is done in several steps:
#' 1. Calculation of the dendrogram;
#' 2. Get validity index for best cluster using the adjusted Rand Index;
#' 3. Cut the dendrogram using the chosen validity index.
#'
#' @references "dtwclust" package (https://CRAN.R-project.org/package=dtwclust)
#'
#' @param samples         A tibble with input set of time series
#' @param bands           Bands to be used in the clustering
#' @param dist_method     String with one of the supported distances.
#' @param linkage         String with agglomeration method to be used.
#'                        Can be any `hclust` method (see `hclust`).
#'                        Default is 'ward.D2'.
#' @param k               Desired number of clusters (overrides default value)
#' @param colors          Color scheme as per `sits_color_name` function.
#' @param .plot           Plot the dendrogram?
#' @param  ...            Additional parameters to be passed
#'                        to dtwclust::tsclust() function.
#' @return A tibble with the clusters or clusters' members.
#'
#' @examples
#' \dontrun{
#' # Load the "dtwclust" package
#' # library(dtwclust)
#' # load a simple data set with two classes
#' data(cerrado_2classes)
#' # calculate the dendrogram and the best clusters
#' clusters <- sits_cluster_dendro(cerrado_2classes, bands = c("NDVI", "EVI"))
#' }
#' @export
sits_cluster_dendro <- function(samples = NULL,
                                bands = NULL,
                                dist_method = "dtw_basic",
                                linkage = "ward.D2",
                                k = NULL,
                                colors = "RdYlGn",
                                .plot = TRUE, ...) {

    # backward compatibility
    samples <- .sits_tibble_rename(samples)

    # verify if data is OK
    .sits_test_tibble(samples)

    # bands in sits are uppercase
    bands <- .sits_samples_bands_check(samples, bands)

    # calculate the dendrogram object
    message("calculating dendrogram...")
    cluster <- .sits_cluster_dendrogram(
        samples = samples,
        bands = bands,
        dist_method = dist_method,
        linkage = linkage, ...
    )

    # find the best cut for the dendrogram
    message("finding the best cut...")
    best_cut <- .sits_cluster_dendro_bestcut(samples, cluster)
    message(paste0("best number of clusters = ", best_cut["k"]))
    message(paste0("best height for cutting the dendrogram = ",
        best_cut["height"]))

    # cut the tree (user-defined value overrides default)
    message("cutting the tree...")
    if (!purrr::is_null(k)) {
        if (k != best_cut["k"]) {
            message(paste0("Caveat: desired number of clusters (", k, ")
                            overrides best value"))
            best_cut["k"] <- k
            best_cut["height"] <-
                c(0, cluster$height)[length(cluster$height) - k + 2]
        }
    }
    samples$cluster <- stats::cutree(
        cluster, best_cut["k"],
        best_cut["height"]
    )

    # plot the dendrogram
    message("Plotting dendrogram...")
    if (.plot)
        .sits_plot_dendrogram(samples, cluster, best_cut["height"], colors)

    # return the result
    message("result is a tibble with cluster indexes...")
    return(samples)
}



#' @title Cluster contigency table
#' @name sits_cluster_frequency
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Computes the contingency table between labels and clusters.
#' This function needs as input a sits tibble with `cluster` column.
#'
#' @param samples          A tibble with `cluster` column.
#' @return A matrix containing all frequencies of labels in clusters.
#' @examples
#' \dontrun{
#' # Load the "dtwclust" package
#' # library(dtwclust)
#' # create clusters by cutting a dendrogram
#' clusters <- sits_cluster_dendro(cerrado_2classes, bands = c("NDVI", "EVI"))
#' # show clusters samples frequency
#' sits_cluster_frequency(clusters)
#' }
#' @export
sits_cluster_frequency <- function(samples) {
    # is the input data the result of a cluster function?
    assertthat::assert_that("cluster" %in% names(samples),
        msg = "sits_cluster_contigency: missing cluster column"
    )

    # compute frequency table (matrix)
    result <- table(samples$label, samples$cluster)

    # compute total row and col
    result <- stats::addmargins(result,
        FUN = list(Total = sum),
        quiet = TRUE
    )
    return(result)
}

#' @title Cluster cleaner
#' @name sits_cluster_clean
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Removes labels that are minority in each cluster.
#'
#' @param samples          Tibble with `cluster` column.
#' @return A tibble with all selected samples.
#' @examples
#' \dontrun{
#' # Load the "dtwclust" package
#' # library(dtwclust)
#' # calculate the dendrogram and the best clusters
#' clusters <- sits_cluster_dendro(cerrado_2classes, bands = c("NDVI", "EVI"))
#' # show clusters samples frequency
#' sits_cluster_frequency(clusters)
#' # remove cluster 3 from the samples
#' clusters_new <- dplyr::filter(clusters, cluster != 3)
#' # show clusters samples frequency
#' sits_cluster_frequency(clusters_new)
#' # clean all remaining clusters
#' cleaned <- sits_cluster_clean(clusters_new)
#' # show clusters samples frequency
#' sits_cluster_frequency(cleaned)
#' }
#' @export
sits_cluster_clean <- function(samples) {

    # is the input data the result of a cluster function?
    assertthat::assert_that("cluster" %in% names(samples),
        msg = "sits_cluster_clean: input data does not contain cluster column"
    )

    # compute frequency table (matrix)
    result <- table(samples$label, samples$cluster)
    # list of number of clusters
    num_cls <- unique(samples$cluster)
    # get the labels of the data
    lbs <- unique(samples$label)
    # for each cluster, get the label with the maximum number of samples
    lbs_max <- lbs[as.vector(apply(result, 2, which.max))]

    # compute the resulting table
    rows <- purrr::map2(lbs_max, num_cls, function(lb, cl) {
        partial <- dplyr::filter(samples, label == lb & cluster == cl)
        return(partial)
    })
    # join the list to get all cleaned clusters in a tibble
    clean_clusters <- dplyr::bind_rows(rows)
    return(clean_clusters)
}

#' @title Cluster validity indices
#' @name .sits_cluster_validity
#' @keywords internal
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Compute different cluster validity indices. This function needs
#' as input a sits tibble with `cluster` column.
#' It is a front-end to `dtwclust::cvi` function.
#' That function computes five indices:
#' 1) adjusted Rand index; 2) Rand index; 3) Jaccard index;
#' 4) Fowlkes-Mallows; and 5) Variation of Information index
#' Please refer to the documentation in that package for more details.
#'
#' @references "dtwclust" package (https://CRAN.R-project.org/package=dtwclust)
#'
#' @param samples   A tibble with `cluster` column.
#'
#' @return A vector with four external validity indices.
#'
.sits_cluster_validity <- function(samples) {
    # verifies if dtwclust package is installed
    if (!requireNamespace("dtwclust", quietly = TRUE)) {
        stop("dtwclust needed for this function to work.
             Please install it.", call. = FALSE)
    }

    # is the input data the result of a cluster function?
    assertthat::assert_that("cluster" %in% names(samples),
        msg = "sits_cluster_validity: input data does not have cluster column"
    )

    # compute CVIs and return
    result <- dtwclust::cvi(
        a = factor(samples$cluster),
        b = factor(samples$label),
        type = "external",
        log.base = 10
    )
    return(result)
}

#' @title Compute a dendrogram using hierarchical clustering
#' @name .sits_cluster_dendrogram
#' @keywords internal
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Cluster time series in hierarchical mode.
#' Hierarchical clustering, as its name suggests,
#' is an algorithm that tries to create a hierarchy of groups in which,
#' as the level in the hierarchy increases, clusters are created by merging
#' the clusters from the next lower level, producing
#' an ordered sequence of groupings. The similarity measure used to
#' group time series in a cluster is the dtw metric.
#' The procedure is deterministic, so it will always give the same
#' result for a chosen set of similarity measures
#' (see \code{\link[dtwclust]{tsclust}}).
#'
#' @references `dtwclust` package (https://CRAN.R-project.org/package=dtwclust)
#'
#' @param samples         Time series data and metadata
#'                        to be used to generate the dendrogram.
#' @param bands           Vector of bands to be clustered.
#' @param dist_method     String with one of the supported distance
#'                        from proxy's dist, e.g. \code{TWDTW}.
#' @param linkage         String with agglomeration method to be used.
#'                        Can be any `hclust` method (see `hclust`).
#'                        Default is 'ward.D2'.
#' @param  ...            Any additional parameters to be passed
#'                        to dtwclust::tsclust() function.
#' @return A full dendrogram tree for data analysis.
#'
.sits_cluster_dendrogram <- function(samples,
                                     bands,
                                     dist_method = "dtw_basic",
                                     linkage = "ward.D2", ...) {
    # verifies if dtwclust package is installed
    if (!requireNamespace("dtwclust", quietly = TRUE)) {
        stop("dtwclust needed for this function to work.
             Please install it.", call. = FALSE)
    }

    # get the values of the time series
    values <- sits_values(samples, bands, format = "cases_dates_bands")

    # call dtwclust and get the resulting dendrogram
    dendro <- dtwclust::tsclust(values,
        type = "hierarchical",
        k = max(NROW(samples) - 1, 2),
        distance = dist_method,
        control = dtwclust::hierarchical_control(method = linkage),
        ...
    )

    # return the dendrogram
    return(dendro)
}

#' @title Compute validity indexes to a range of cut height
#' @name .sits_cluster_dendro_bestcut
#' @keywords internal
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Reads a dendrogram object and its corresponding sits tibble and
#' computes the best number of clusters that maximizes the adjusted Rand index.
#'
#' @references
#' Lawrence Hubert and Phipps Arabie. Comparing partitions.
#' Journal of Classification, 2, p.193--218, 1985.
#'
#' See \link[flexclust]{randIndex} for implementation details.
#'
#' @param samples          Input set of time series.
#' @param dendro           Dendrogram object returned from
#'                         \code{\link[sits]{.sits_cluster_dendrogram}}.
#' @return Vector with best number of clusters (k) and its respective height.
#'
.sits_cluster_dendro_bestcut <- function(samples, dendro) {

    # verifies if flexclust package is installed
    if (!requireNamespace("flexclust", quietly = TRUE)) {
        stop("flexclust needed for this function to work.
             Please install it.", call. = FALSE)
    }
    # compute range
    k_range <- seq(2, max(length(dendro$height) - 1, 2))

    # compute ARI for each k (vector)
    ari <-
        k_range %>%
        purrr::map(function(k) {
            flexclust::randIndex(stats::cutree(dendro, k = k),
                factor(samples$label),
                correct = TRUE
            )
        }) %>%
        unlist()

    # get the best ARI result
    k_result <- k_range[which.max(ari)]

    # compute each height corresponding to `k_result`
    h_result <- c(0, dendro$height)[length(dendro$height) - k_result + 2]

    # create a named vector and return
    best_cut <- structure(c(k_result, h_result), .Names = c("k", "height"))
    return(best_cut)
}
