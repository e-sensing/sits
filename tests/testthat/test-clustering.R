context("Clustering")

test_that("Creating a dendrogram and clustering the results",{
    #skip_on_cran()
    library(flexclust)
    library(dtwclust)
    data(cerrado_2classes)
    dendro.obj <- sits_dendrogram(cerrado_2classes, bands = c("ndvi", "evi"))

    expect_equal(length(names(dendro.obj)), 7)
    expect_equal(length(dendro.obj$height), dim(dendro.obj$merge)[1])

    results.vec  <- sits_dendro_bestcut(cerrado_2classes, dendro.obj)
    clustered.tb <- sits_cluster(cerrado_2classes, dendro.obj, k = results.vec["k"])
    result.mtx   <- sits_cluster_frequency(clustered.tb)
    clean.tb     <- sits_cluster_remove(clustered.tb, min_perc = 0.90)

    expect_true(NROW(dendro.obj@cldist) == NROW(cerrado_2classes))
    expect_equal(sum(dendro.obj@cldist), 1.0259, tolerance = 1e-5)
    expect_true(dendro.obj$height[length(dendro.obj$height) - results.vec["k"] + 1] ==
                               as.numeric(results.vec["height"]))
    expect_true(NROW(result.mtx)  ==
                              (length(sits_labels(cerrado_2classes)$label) + 1))
    expect_true(all(unique(clean.tb$cluster) %in%
                                  unique(clustered.tb$cluster)))
})

test_that("Clean and Validity",{
    library(dtwclust)
    data(cerrado_2classes)
    dendro <- sits_dendrogram (cerrado_2classes, bands = c("ndvi"))
    clusters.tb <- sits_cluster(cerrado_2classes, dendro, k = 6)
    cl.freq <- sits_cluster_frequency(clusters.tb)
    cleaned.tb <- sits_cluster_clean(clusters.tb, min_perc = 0.25)
    cl.freq.clean <- sits_cluster_frequency(cleaned.tb)

    expect_true(cl.freq[3,7] > cl.freq.clean[3,7])

    val <- sits_cluster_validity(clusters.tb)
    expect_equal(length(names(val)), 5)
    expect_equal(val[[1]], 0.01772, tolerance = 0.001)
})
