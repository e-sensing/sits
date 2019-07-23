context("Clustering")

test_that("Creating a dendrogram and clustering the results",{
    #skip_on_cran()
    library(flexclust)
    library(dtwclust)
    data(cerrado_2classes)
    invisible(capture.output(clustered.tb <- sits_cluster(cerrado_2classes, bands = "ndvi")))

    expect_equal(length(dplyr::distinct(clustered.tb, cluster)$cluster), 8)
    result.mtx   <- sits_cluster_frequency(clustered.tb)
    clean.tb     <- sits_cluster_remove(clustered.tb, min_perc = 0.90)
    expect_true(NROW(result.mtx)  ==
                              (length(sits_labels(cerrado_2classes)$label) + 1))
    expect_true(all(unique(clean.tb$cluster) %in%
                                  unique(clustered.tb$cluster)))
})

test_that("Clean and Validity",{
    library(dtwclust)
    data(cerrado_2classes)
    clusters.tb  <- sits_cluster(cerrado_2classes, bands = c("ndvi"), silent = TRUE)
    cl.freq <- sits_cluster_frequency(clusters.tb)
    cleaned.tb <- sits_cluster_clean(clusters.tb, min_perc = 0.25)
    cl.freq.clean <- sits_cluster_frequency(cleaned.tb)

    expect_true(cl.freq[1,4] > cl.freq.clean[1,4])

})
