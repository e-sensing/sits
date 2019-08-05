context("Vignette")
test_that("Test Data Handling", {
    expect_true(all(samples_mt_ndvi[1:3,]$label %in% c("Pasture")))
    expect_true(sum(sits_labels(samples_mt_ndvi)$prop) == 1)
    expect_true(sits_bands(samples_mt_ndvi) %in% c("ndvi"))
})
