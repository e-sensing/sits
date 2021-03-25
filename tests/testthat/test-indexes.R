context("Indexes")

test_that("NDWI", {
    ndwi <- sits_ndwi(samples_modis_4bands)

    expect_true("NDVI" %in% names(ndwi$time_series[[1]]))
})

test_that("SAVI", {
    data(samples_mt_6bands)
    savi <- sits_savi(samples_mt_6bands)

    expect_true("SAVI" %in% names(savi$time_series[[1]]))
})
