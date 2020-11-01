context("Classification of time series")
test_that("Classify a single time series with random forest - single core and multicore", {
    samples_mt_ndvi <- sits_select(samples_mt_4bands, bands = "NDVI")
    rfor_model <- sits_train(samples_mt_ndvi, sits_rfor(num_trees = 100))

    expect_type(rfor_model, "closure")

    class_ndvi.tb <- sits_classify(point_ndvi, rfor_model)

    expect_true(NROW(class_ndvi.tb$predicted[[1]]) == 16)
    expect_true(all(class_ndvi.tb$predicted[[1]]$class %in%
                                  sits_labels(samples_mt_ndvi)$label))

    class_ndvi.tb <- sits_classify(point_ndvi, rfor_model, multicores = 2)

    expect_true(NROW(class_ndvi.tb$predicted[[1]]) == 16)
    expect_true(all(class_ndvi.tb$predicted[[1]]$class %in%
                        sits_labels(samples_mt_ndvi)$label))
})

test_that("Classify a set time series with rfor + filter", {
    #single core
    samples_mt_2bands <- sits_select(samples_mt_4bands, bands = c("NDVI", "EVI"))
    samples_filt <- sits_sgolay(samples_mt_2bands, bands_suffix = "")
    svm_model <- sits_train(samples_filt, sits_rfor(num_trees = 100))

    class1.tb <- sits_classify(data = cerrado_2classes, ml_model = svm_model,
                               filter = sits_sgolay(bands_suffix = ""), multicores = 1)

    expect_true(class1.tb$predicted[[1]]$class %in% sits_labels(cerrado_2classes)$label)

})
test_that("Classify time series with TWDTW method", {
    testthat::skip_on_cran()
    samples_mt_ndvi <- sits_select(samples_mt_4bands, bands = "NDVI")
    point_mt_ndvi <- sits_select(point_mt_6bands, bands = "NDVI")
    patterns <- sits_patterns(samples_mt_ndvi)
    matches <- sits_twdtw_classify(point_mt_ndvi, patterns, bands = "NDVI",
                                   alpha = -0.1, beta = 100,
                                   theta = 0.5, keep = TRUE)

    expect_true(all(unique(matches$predicted[[1]]$predicted) %in%
                                  sits_labels(samples_mt_ndvi)$label))
})

test_that("Classify error bands 1", {

    samples_mt_ndvi <- sits_select(samples_mt_4bands, bands = "NDVI")

    model <- sits_train(samples_mt_ndvi, sits_svm())
    point.tb <- sits_select(point_mt_6bands, "EVI")

    expect_error(sits_classify(point.tb, model),
    "sits_normalize: bands in the data (EVI) do not match bands in the model (NDVI)",
                fixed = TRUE)
})

