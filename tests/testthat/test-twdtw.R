context("TWDTW")
test_that("Classify time series with TWDTW method", {
    #skip_on_cran()
    data("samples_mt_4bands")
    data("point_mt_6bands")
    samples_mt_ndvi <- sits_select_bands(samples_mt_4bands, ndvi)
    point_mt_ndvi <- sits_select_bands(point_mt_6bands, ndvi)
    patterns <- sits_patterns(samples_mt_ndvi)
    matches <- sits_twdtw_classify(point_mt_ndvi, patterns, bands = "ndvi",
                                   alpha = -0.1, beta = 100,
                                   theta = 0.5, keep = TRUE)

    expect_true(all(unique(matches$predicted[[1]]$predicted) %in%
                        sits_labels(samples_mt_ndvi)$label))
})

test_that("Test special conditions in TWDTW method", {
    #skip_on_cran()
    patterns <- sits_patterns(cerrado_2classes)
    test <- cerrado_2classes[1:25,]
    matches <- sits_twdtw_classify(test, patterns, bands = "ndvi",
                                   alpha = -0.1, beta = 100,
                                   theta = 0.5, keep = TRUE, progress = FALSE)

    expect_true(all(unique(matches$predicted[[1]]$predicted) %in%
                        sits_labels(cerrado_2classes)$label))
})
