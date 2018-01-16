testthat::context("Clustering")
testthat::test_that("Creating a dendogram and clustering the results",{
    testthat::skip_on_cran()
    library(dtwclust)
    data(cerrado_2classes)
    clusters     <- sits_dendrogram(cerrado_2classes, bands = c("ndvi", "evi"))
    clustered.tb <- sits_cluster(cerrado_2classes, clusters, k = 6)
    result.mtx   <- sits_cluster_frequency(clustered.tb)
    clean.tb     <- sits_cluster_cleaner(clustered.tb, min_perc = 0.10)

    testthat::expect_true(NROW(clusters@clusinfo) == NROW(cerrado_2classes))
    testthat::expect_true(NROW(clustered.tb$cluster) == NROW(clusters@clusinfo))
    testthat::expect_true(NROW(result.mtx)  ==
                              (length(sits_labels(cerrado_2classes)$label) + 1))
    testthat::expect_true(all(unique(clean.tb$cluster) %in%
                                  unique(clustered.tb$cluster)))
})
