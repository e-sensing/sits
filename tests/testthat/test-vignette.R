test_that("Test Data Handling", {
  expect_true(all(samples_modis_4bands[1:3, ]$label %in% c("Pasture")))
  expect_true(sum(sits_labels_summary(samples_modis_4bands)$prop) == 1)
  expect_true(all(sits_bands(samples_modis_4bands) %in%
    c("NDVI", "EVI", "MIR", "NIR")))
})
