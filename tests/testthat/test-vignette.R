test_that("Test Data Handling", {
    expect_true(all(samples_modis_ndvi[1:3, ][["label"]] %in% "Pasture"))
    expect_identical(sum(summary(samples_modis_ndvi)[["prop"]]), 1)
    expect_true(all(sits_bands(samples_modis_ndvi) %in% "NDVI"))
})
