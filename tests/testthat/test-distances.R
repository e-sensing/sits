test_that("compute distances in time series for indices", {
    bands_samples <- sits_bands(samples_modis_ndvi)
    n_timeline <- length(sits_timeline(samples_modis_ndvi))

    sample_distances <- .predictors(samples_modis_ndvi)

    testthat::expect_equal(
        class(sample_distances),
        c("tbl_df", "tbl", "data.frame")
    )

    testthat::expect_equal(
        sample_distances[10, "NDVI2"][[1]],
        0.6223
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
