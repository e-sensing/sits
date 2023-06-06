#' @title Find clusters in time series samples
#' @name sits_clustering
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description These functions support hierarchical agglomerative clustering in
#' sits. They provide support from creating a dendrogram and using it for
#' cleaning samples.
#'
#' \code{sits_cluster_dendro()} takes a tibble containing time series and
#' produces a sits tibble with an added "cluster" column. The function first
#' calculates a dendrogram and obtains a validity index for best clustering
#' using the adjusted Rand Index. After cutting the dendrogram using the chosen
#' validity index, it assigns a cluster to each sample.
#'
#' \code{sits_cluster_frequency()} computes the contingency table between labels
#' and clusters and produces a matrix.
#' It needs as input a tibble produced by \code{sits_cluster_dendro()}.
#'
#' \code{sits_cluster_clean()} takes a tibble with time series
#' that has an additional `cluster` produced by \code{sits_cluster_dendro()}
#' and removes labels that are minority in each cluster.
#'
#' @references "dtwclust" package (https://CRAN.R-project.org/package=dtwclust)
#'
#' @param samples         Tibble with input set of time series.
#' @param bands           Bands to be used in the clustering.
#' @param dist_method     Distance method.
#' @param linkage         Agglomeration method.
#'                        Can be any `hclust` method (see `hclust`).
#'                        Default is 'ward.D2'.
#' @param k               Desired number of clusters (overrides default value)
#' @param color_palette   Color palette as per `grDevices::hcl.pals()` function.
#' @param .plot           Plot the dendrogram?
#' @param  ...            Additional parameters to be passed
#'                        to dtwclust::tsclust() function.
#' @return                Tibble with added "cluster" column.
#'
#' @rdname sits_clustering
#' @note
#' Please refer to the sits documentation available in
#' <https://e-sensing.github.io/sitsbook/> for detailed examples.
#' @examples
#' if (sits_run_examples()) {
#'     clusters <- sits_cluster_dendro(cerrado_2classes)
#' }
#'
#' @export
sits_cluster_dendro <- function(samples = NULL,
                                bands = NULL,
                                dist_method = "dtw_basic",
                                linkage = "ward.D2",
                                k = NULL,
                                color_palette = "RdYlGn",
                                .plot = TRUE, ...) {
    # needs package dtwclust
    .check_require_packages("dtwclust")
    # verify if data is OK
    .check_samples_train(samples)

    # bands in sits are uppercase
    bands <- .tibble_bands_check(samples, bands)

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
    message(paste0(
        "best height for cutting the dendrogram = ",
        best_cut["height"]
    ))

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
    if (.plot) {
        .plot_dendrogram(
            data = samples,
            cluster = cluster,
            cutree_height = best_cut["height"],
            color_palette = color_palette
        )
    }

    # return the result
    message("result is a tibble with cluster indexes...")
    return(samples)
}

#'
#' @title Show label frequency in each cluster produced by dendrogram analysis
#' @name sits_cluster_frequency
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @param samples         Tibble with input set of time series with additional
#'                        cluster information produced
#'                        by \code{sits::sits_cluster_dendro}.
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
    result <- table(samples$label, samples$cluster)

    # compute total row and col
    result <- stats::addmargins(result,
        FUN = list(Total = sum),
        quiet = TRUE
    )
    return(result)
}

#' @title Removes labels that are minority in each cluster.
#' @name sits_cluster_clean
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description Takes a tibble with time series
#' that has an additional `cluster` produced by \code{sits_cluster_dendro()}
#' and removes labels that are minority in each cluster.
#'
#' @param samples         Tibble with input set of time series with additional
#'                        cluster information produced
#'                        by \code{sits::sits_cluster_dendro()}.
#' @return                Tibble with time series where clusters have been
#'                        cleaned of labels that were in a minority at each
#'                        cluster.
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
    result <- table(samples$label, samples$cluster)
    # list of number of clusters
    num_cls <- unique(samples$cluster)
    # get the labels of the data
    lbs <- unique(samples$label)
    # for each cluster, get the label with the maximum number of samples
    lbs_max <- lbs[as.vector(apply(result, 2, which.max))]

    # compute the resulting table
    clean_clusters <- purrr::map2_dfr(
        lbs_max, num_cls,
        function(lb, cl) {
            partial <- dplyr::filter(
                samples,
                .data[["label"]] == lb &
                    .data[["cluster"]] == cl
            )
            return(partial)
        }
    )
    return(clean_clusters)
}

#' @title Cluster validity indices
#' @name .sits_cluster_validity
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
.sits_cluster_validity <- function(samples) {

    # set caller to show in errors
    .check_set_caller(".sits_cluster_validity")

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
#' @name .sits_cluster_dendrogram
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
.sits_cluster_dendrogram <- function(samples,
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
#'                         \code{\link[sits]{.sits_cluster_dendrogram}}.
#' @return                 Vector with best number of clusters (k)
#'                         and its respective height.
#'
.sits_cluster_dendro_bestcut <- function(samples, dendro) {

    # compute range
    k_range <- seq(2, max(length(dendro$height) - 1, 2))

    # compute ARI for each k (vector)
    ari <-
        k_range %>%
        purrr::map(function(k) {
            x <- stats::cutree(dendro, k = k)
            y <- factor(samples$label)
            .sits_cluster_rand_index(table(x, y))
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

.sits_cluster_rand_index <- function(x, correct = TRUE) {
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
