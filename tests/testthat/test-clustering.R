test_that("Creating a dendrogram and clustering the results", {
    suppressMessages(library(dtwclust))
    data(cerrado_2classes)
    clusters <- suppressMessages(
        sits_cluster_dendro(
            cerrado_2classes,
            bands = c("NDVI", "EVI"),
            .plot = FALSE
        )
    )

    dendro <- .cluster_dendrogram(cerrado_2classes,
        bands = c("NDVI", "EVI")
    )
    expect_true(dendro@distmat[1, 2] > 3.0)

    vec <- .cluster_dendro_bestcut(cerrado_2classes, dendro)

    expect_true(vec["k"] == 6 && vec["height"] > 20.0)

    expect_equal(length(unique(clusters$cluster)), as.integer(vec["k"]))

    freq_clusters <- sits_cluster_frequency(clusters)
    expect_true(nrow(freq_clusters) ==
        (length(sits_labels(cerrado_2classes)) + 1))

    clusters_new <- dplyr::filter(clusters, cluster != 3)
    clean <- sits_cluster_clean(clusters_new)

    result <- .cluster_validity(clusters)
    expect_true(result["ARI"] > 0.30 && result["VI"] > 0.50)

    expect_true(all(unique(clean$cluster) %in%
        unique(clusters_new$cluster)))
    expect_true(sits_cluster_frequency(clusters_new)[3, 1] >
        sits_cluster_frequency(clean)[3, 1])
})

test_that("Creating a dendrogram with a fixed k value", {
    library(dtwclust)
    data(cerrado_2classes)
    clusters <- suppressMessages(
        sits_cluster_dendro(
            cerrado_2classes,
            k = 12,
            .plot = FALSE
        )
    )
    expect_true(length(unique(clusters$cluster)) == 12)
})
