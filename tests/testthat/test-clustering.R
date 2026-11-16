test_that("Creating a dendrogram and clustering the results", {
    suppressMessages(library(dtwclust))
    data(cerrado_2classes)
    clusters <- suppressMessages(
        sits_cluster_dendro(
            cerrado_2classes,
            bands = c("NDVI", "EVI")
        )
    )

    messages <- capture_messages({
        clusters2 <- sits_cluster_dendro(
            cerrado_2classes,
            bands = c("NDVI", "EVI"),
            k = 8
        )
    })
    # test message
    expect_true(grepl("desired", messages[3]))
    dendro <- .cluster_dendrogram(cerrado_2classes,
        bands = c("NDVI", "EVI")
    )
    expect_true(dendro@distmat[1] > 3.0)

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

test_that("Cluster cleaning - labels and clusters out of order", {
    data(cerrado_2classes)
    samples <- cerrado_2classes[1:5, ]
    samples[["id"]] <- 1:5
    # neither the labels nor the clusters first appear in sorted order
    samples[["label"]] <- c("y", "y", "x", "x", "x")
    samples[["cluster"]] <- c(2L, 2L, 1L, 1L, 2L)
    clean <- sits_cluster_clean(samples)
    # sample 5 is the minority of cluster 2
    expect_setequal(clean[["id"]], 1:4)
})
