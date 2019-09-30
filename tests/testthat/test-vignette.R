context("Vignette")
test_that("Test Data Handling", {
    expect_true(all(samples_mt_4bands[1:3,]$label %in% c("Pasture")))
    expect_true(sum(sits_labels(samples_mt_4bands)$prop) == 1)
    expect_true(all(sits_bands(samples_mt_4bands) %in%
                        c("ndvi", "evi", "mir", "nir")))
})
