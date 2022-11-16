test_that("compute distances in time series for indices", {
    bands_samples <- sits_bands(samples_modis_ndvi)
    n_timeline <- length(sits_timeline(samples_modis_ndvi))

    sample_distances <- .sits_distances(samples_modis_ndvi)

    testthat::expect_equal(
        class(sample_distances),
        c("data.table", "data.frame")
    )

    testthat::expect_equal(
        sample_distances[10, "NDVI2"][[1]],
        0.3842
    )

    testthat::expect_equal(
        sample_distances[102, "label"][[1]],
        "Pasture"
    )

    testthat::expect_equal(
        sample_distances[500, "sample_id"][[1]],
        500
    )
})

