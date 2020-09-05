context("Indexes")

test_that("NDWI", {
    ndwi.tb <- sits_ndwi(samples_mt_4bands)

    expect_true("NDVI" %in% names(ndwi.tb$time_series[[1]]))
})

test_that("SAVI", {
    data (samples_mt_6bands)
    savi.tb <- sits_savi(samples_mt_6bands)

    expect_true("SAVI" %in% names(savi.tb$time_series[[1]]))
})
