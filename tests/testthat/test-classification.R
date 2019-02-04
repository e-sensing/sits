context("Classification of time series")
test_that("Classify a time series with random forest", {
    #skip_on_cran()
    data(samples_mt_ndvi)
    data(point_ndvi)
    model <- sits_train(samples_mt_ndvi, sits_rfor())

    expect_type(model, "closure")

    class_ndvi.tb <- sits_classify(point_ndvi, model)

    # TODO: why does class_ndvi.tb has a label == "NoClass"?
    class_ndvi.tb$label

    expect_true(NROW(class_ndvi.tb$predicted[[1]]) == 16)
    expect_true(all(class_ndvi.tb$predicted[[1]]$class %in%
                                  sits_labels(samples_mt_ndvi)$label))
})

test_that("Classify time series with an svm model",{
    #skip_on_cran()

    data(point_mt_6bands)
    data(samples_mt_9classes)

    samples.tb <- sits_select_bands(samples_mt_9classes, ndvi, evi)
    model <- sits_train(samples.tb, sits_svm())

    class.tb <- sits_select_bands(point_mt_6bands, ndvi, evi) %>%
        sits_classify(model)

    expect_true(all(class.tb$predicted[[1]]$class %in%
                                  sits_labels(samples_mt_9classes)$label))
})

test_that("Classify time series with TWDTW method", {
    #skip_on_cran()
    patterns.tb <- sits_patterns(samples_mt_ndvi)
    point_mt_ndvi <- sits_select_bands(point_mt_6bands, ndvi)
    matches <- sits_twdtw_classify(point_mt_ndvi, patterns.tb, bands = "ndvi",
                                   alpha = -0.1, beta = 100, theta = 0.5, keep = TRUE)

    expect_true(all(unique(matches$predicted[[1]]$predicted) %in%
                                  sits_labels(samples_mt_ndvi)$label))
})

test_that("Classify error bands 1", {
    data(point_mt_6bands)
    data(samples_mt_9classes)

    samples.tb <- sits_select_bands(samples_mt_9classes[1:400,], ndvi, evi, red)
    model <- sits_train(samples.tb, sits_svm())
    point.tb <- sits_select_bands(point_mt_6bands, ndvi)

    expect_error(sits_classify(point.tb, model), "sits_normalize: bands in the data (ndvi) do not match bands in the model (ndvi, evi, red)", fixed = TRUE)
})

test_that("Classify error bands 2", {
    data(point_mt_6bands)
    data(samples_mt_9classes)

    samples.tb <- sits_select_bands(samples_mt_9classes[1:400,], ndvi, evi, red)

    # Model is empty
    #system.time({model <- sits_train(samples.tb, sits_svm(tolerance = 10))})

    model <- sits_train(samples.tb, sits_svm())
    point.tb <- sits_select_bands(point_mt_6bands, ndvi, red)

    expect_error(sits_classify(point.tb, model), "sits_normalize: bands in the data (ndvi, red) do not match bands in the model (ndvi, evi, red)", fixed = TRUE)
})

test_that("Classify error bands 3", {
    data(point_mt_6bands)
    data(samples_mt_9classes)

    samples.tb <- sits_select_bands(samples_mt_9classes[1:400,], ndvi, evi, red)
    model <- sits_train(samples.tb, sits_svm())
    point.tb <- sits_select_bands(point_mt_6bands, ndvi, evi)

    expect_error(sits_classify(point.tb, model), "sits_normalize: bands in the data (ndvi, evi) do not match bands in the model (ndvi, evi, red)", fixed = TRUE)
})
