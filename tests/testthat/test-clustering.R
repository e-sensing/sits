context("Clustering")

test_that("Creating a dendrogram and clustering the results",{
    #skip_on_cran()
    library(flexclust)
    library(dtwclust)
    data(cerrado_2classes)
    clustered.tb <- sits_cluster_dendro(cerrado_2classes,
                                        bands = c("ndvi", "evi"),
                                        silent = TRUE)
    expect_equal(length(dplyr::distinct(clustered.tb, cluster)$cluster), 6)

    dendro.obj <- sits:::.sits_cluster_dendrogram(cerrado_2classes,
                                                  bands = c("ndvi", "evi"))
    expect_true(dendro.obj@distmat[1,2] > 3.0)

    vec <- sits:::.sits_cluster_dendro_bestcut(cerrado_2classes, dendro.obj)

    expect_true(vec["k"] == 6 && vec["height"] > 20.0)

    result.mtx   <- sits_cluster_frequency(clustered.tb)
    expect_true(NROW(result.mtx)  ==
                              (length(sits_labels(cerrado_2classes)$label) + 1))

    clusters_new.tb <- dplyr::filter(clustered.tb, cluster != 3)
    clean.tb     <- sits_cluster_clean(clusters_new.tb)

    result <- sits:::.sits_cluster_validity(clustered.tb)
    expect_true(result["ARI"] > 0.30 && result["VI"] > 0.50)

    expect_true(all(unique(clean.tb$cluster) %in%
                                  unique(clusters_new.tb$cluster)))
    expect_true(sits_cluster_frequency(clusters_new.tb)[3,1]
              > sits_cluster_frequency(clean.tb)[3,1])
})

test_that("Creating a dendrogram with a fixed k value",{
    #skip_on_cran()
    library(flexclust)
    library(dtwclust)
    data(cerrado_2classes)
    clustered.tb <- sits_cluster_dendro(cerrado_2classes,
                                        silent = FALSE,
                                        k = 12)
    expect_equal(length(dplyr::distinct(clustered.tb, cluster)$cluster), 12)
})
