context("Classification of time series")
test_that("Classify a single time series with random forest", {
    #skip_on_cran()
    samples_mt_ndvi <- sits_select_bands(samples_mt_4bands, ndvi)
    rfor_model <- sits_train(samples_mt_ndvi, sits_rfor(num_trees = 200))

    expect_type(rfor_model, "closure")

    class_ndvi.tb <- sits_classify(point_ndvi, rfor_model)

    expect_true(NROW(class_ndvi.tb$predicted[[1]]) == 16)
    expect_true(all(class_ndvi.tb$predicted[[1]]$class %in%
                                  sits_labels(samples_mt_ndvi)$label))
})
test_that("Classify a set time series with svm - single core and multicore", {
    #skip_on_cran()
    #single core
    samples_mt_2bands <- sits_select_bands(samples_mt_4bands, ndvi, evi)
    svm_model <- sits_train(samples_mt_2bands, sits_svm())

    expect_type(svm_model, "closure")

    class1.tb <- sits_classify(cerrado_2classes, svm_model, multicores = 1)

    expect_true(class1.tb$predicted[[1]]$class %in% sits_labels(cerrado_2classes)$label)

    # multicore
    class2.tb <- sits_classify(cerrado_2classes, svm_model, multicores = 2)
    c1 <- dplyr::bind_rows(class1.tb$predicted)$class
    c2 <- dplyr::bind_rows(class2.tb$predicted)$class
    expect_true(all(c1 == c2))
})
test_that("Classify time series with TWDTW method", {
    #skip_on_cran()
    samples_mt_ndvi <- sits_select_bands(samples_mt_4bands, ndvi)
    point_mt_ndvi <- sits_select_bands(point_mt_6bands, ndvi)
    patterns <- sits_patterns(samples_mt_ndvi)
    matches <- sits_twdtw_classify(point_mt_ndvi, patterns, bands = "ndvi",
                                   alpha = -0.1, beta = 100,
                                   theta = 0.5, keep = TRUE)

    expect_true(all(unique(matches$predicted[[1]]$predicted) %in%
                                  sits_labels(samples_mt_ndvi)$label))
})

test_that("Classify error bands 1", {

    samples_mt_ndvi <- sits_select_bands(samples_mt_4bands, ndvi)

    model <- sits_train(samples_mt_ndvi, sits_svm())
    point.tb <- sits_select_bands(point_mt_6bands, evi)

    expect_error(sits_classify(point.tb, model),
    "sits_normalize: bands in the data (evi) do not match bands in the model (ndvi)",
                fixed = TRUE)
})

