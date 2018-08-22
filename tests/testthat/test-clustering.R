testthat::context("Clustering")
testthat::test_that("Creating a dendogram and clustering the results",{
    testthat::skip_on_cran()
    library(flexclust)
    library(dtwclust)
    data(cerrado_2classes)
    dendro.obj   <- sits_dendrogram(cerrado_2classes, bands = c("ndvi", "evi"))
    results.vec  <- sits_dendro_bestcut(cerrado_2classes, dendro.obj)
    clustered.tb <- sits_cluster(cerrado_2classes, dendro.obj, k = results.vec["k"])
    result.mtx   <- sits_cluster_frequency(clustered.tb)
    clean.tb     <- sits_cluster_remove(clustered.tb, min_perc = 0.90)

    testthat::expect_true(NROW(dendro.obj@cldist) == NROW(cerrado_2classes))
    testthat::expect_true(dendro.obj$height[length(dendro.obj$height) - results.vec["k"] + 1] ==
                               as.numeric(results.vec["height"]))
    testthat::expect_true(NROW(result.mtx)  ==
                              (length(sits_labels(cerrado_2classes)$label) + 1))
    testthat::expect_true(all(unique(clean.tb$cluster) %in%
                                  unique(clustered.tb$cluster)))
})
