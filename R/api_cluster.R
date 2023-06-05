#' @title Cluster validity indices
#' @name .cluster_validity
#' @keywords internal
#' @noRd
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
#' @return          A vector with four validity indices.
#'
.cluster_validity <- function(samples) {

    # set caller to show in errors
    .check_set_caller(".cluster_validity")

    # verifies if dtwclust package is installed
    .check_require_packages("dtwclust")

    # is the input data the result of a cluster function?
    .check_samples_cluster(samples)

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
#' @name .cluster_dendrogram
#' @keywords internal
#' @noRd
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
#' @param dist_method     One of the supported distance
#'                        from proxy's dist, e.g. \code{TWDTW}.
#' @param linkage         Agglomeration method to be used.
#'                        Can be any `hclust` method (see `hclust`).
#'                        Default is 'ward.D2'.
#' @param  ...            Any additional parameters to be passed
#'                        to dtwclust::tsclust() function.
#' @return                Full dendrogram tree for data analysis.
#'
.cluster_dendrogram <- function(samples,
                                bands,
                                dist_method = "dtw_basic",
                                linkage = "ward.D2", ...) {
    # verifies if dtwclust package is installed
    .check_require_packages("dtwclust")

    # get the values of the time series
    values <- sits_values(samples, bands, format = "cases_dates_bands")

    # call dtwclust and get the resulting dendrogram
    dendro <- dtwclust::tsclust(
        values,
        type = "hierarchical",
        k = max(nrow(samples) - 1, 2),
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
#' @noRd
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Reads a dendrogram object and its corresponding sits tibble and
#' computes the best number of clusters that maximizes the adjusted Rand index.
#'
#' @references
#' Lawrence Hubert and Phipps Arabie. Comparing partitions.
#' Journal of Classification, 2, p.193--218, 1985.
#'
#'
#' @param samples          Input set of time series.
#' @param dendro           Dendrogram object returned from
#'                         \code{\link[sits]{.cluster_dendrogram}}.
#' @return                 Vector with best number of clusters (k)
#'                         and its respective height.
#'
.cluster_dendro_bestcut <- function(samples, dendro) {

    # compute range
    k_range <- seq(2, max(length(dendro$height) - 1, 2))

    # compute ARI for each k (vector)
    ari <-
        k_range |>
        purrr::map(function(k) {
            x <- stats::cutree(dendro, k = k)
            y <- factor(samples$label)
            .cluster_rand_index(table(x, y))
        }) |>
        unlist()

    # get the best ARI result
    k_result <- k_range[which.max(ari)]

    # compute each height corresponding to `k_result`
    h_result <- c(0, dendro$height)[length(dendro$height) - k_result + 2]

    # create a named vector and return
    best_cut <- structure(c(k_result, h_result), .Names = c("k", "height"))
    return(best_cut)
}

.cluster_rand_index <- function(x, correct = TRUE) {
    if (length(dim(x)) != 2) {
        stop("Argument x needs to be a 2-dimensional table.")
    }

    n <- sum(x)
    ni <- apply(x, 1, sum)
    nj <- apply(x, 2, sum)
    n2 <- choose(n, 2)

    rand <- NULL
    if (correct) {
        nis2 <- sum(choose(ni[ni > 1], 2))
        njs2 <- sum(choose(nj[nj > 1], 2))
        rand <- c(ARI = c(sum(choose(x[x > 1], 2)) -
            (nis2 * njs2) / n2) / ((nis2 + njs2) / 2 - (nis2 * njs2) / n2))
    } else {
        rand <- c(rand, RI = 1 + (sum(x^2) - (sum(ni^2) + sum(nj^2)) / 2) / n2)
    }

    return(rand)
}
