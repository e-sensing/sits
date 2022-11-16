test_that("Test Data Handling", {
    expect_true(all(samples_modis_ndvi[1:3, ]$label %in% c("Pasture")))
    expect_true(sum(sits_labels_summary(samples_modis_ndvi)$prop) == 1)
    expect_true(all(sits_bands(samples_modis_ndvi) %in%
        c("NDVI")))
})
