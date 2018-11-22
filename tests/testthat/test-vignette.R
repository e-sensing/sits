context("Vignette")
test_that("Test Data Handling", {
    expect_true(all(samples_mt_9classes[1:3,]$label %in% c("Pasture")))
    expect_true(sum(sits_labels(samples_mt_9classes)$freq) == 1)
    samples_ndvi.tb <- sits_select_bands(samples_mt_9classes, ndvi)
    expect_true(sits_bands(samples_ndvi.tb) %in% c("ndvi"))
})
