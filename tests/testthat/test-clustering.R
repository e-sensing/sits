context("Clustering")

test_that("Creating a dendrogram and clustering the results",{
    #skip_on_cran()
    library(flexclust)
    library(dtwclust)
    data(cerrado_2classes)
    clustered.tb <- sits_cluster(cerrado_2classes, bands = c("ndvi", "evi"), silent = TRUE)
    expect_equal(length(dplyr::distinct(clustered.tb, cluster)$cluster), 6)

    result.mtx   <- sits_cluster_frequency(clustered.tb)
    expect_true(NROW(result.mtx)  ==
                              (length(sits_labels(cerrado_2classes)$label) + 1))

    clusters_new.tb <- dplyr::filter(clustered.tb, cluster != 3)
    clean.tb     <- sits_cluster_clean(clusters_new.tb)
    expect_true(all(unique(clean.tb$cluster) %in%
                                  unique(clusters_new.tb$cluster)))
    expect_true(sits_cluster_frequency(clusters_new.tb)[3,1]
              > sits_cluster_frequency(clean.tb)[3,1])
})
