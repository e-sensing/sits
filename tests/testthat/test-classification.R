context("Classification of time series")
test_that("Classify a time series with random forest", {
    #skip_on_cran()
    data(samples_MT_ndvi)
    data(point_ndvi)
    model <- sits_train(samples_MT_ndvi, sits_rfor())

    expect_type(model, "closure")

    class_ndvi.tb <- sits_classify(point_ndvi, model)

    # TODO: why does class_ndvi.tb has a label == "NoClass"?
    class_ndvi.tb$label

    expect_true(NROW(class_ndvi.tb$predicted[[1]]) == 16)
    expect_true(all(class_ndvi.tb$predicted[[1]]$class %in%
                                  sits_labels(samples_MT_ndvi)$label))
})

test_that("Classify time series with an svm model",{
    #skip_on_cran()

    data(point_MT_6bands)
    data(samples_MT_9classes)

    samples.tb <- sits_select(samples_MT_9classes,
                              bands = c("ndvi", "evi"))
    model <- sits_train(samples.tb, sits_svm())
    point.tb <- sits_select(point_MT_6bands, bands = c("ndvi", "evi"))

    class.tb <- sits_classify(point.tb, model)

    expect_true(all(class.tb$predicted[[1]]$class %in%
                                  sits_labels(samples_MT_9classes)$label))
})

test_that("Classify time series with TWDTW method",{
    #skip_on_cran()
    patterns.tb <- sits_patterns(samples_MT_ndvi)
    matches <- sits_TWDTW_classify(point_MT_6bands, patterns.tb, bands = "ndvi",
                                   alpha = -0.1, beta = 100, theta = 0.5, keep = TRUE)

    expect_true(all(unique(matches$predicted[[1]]$predicted) %in%
                                  sits_labels(samples_MT_ndvi)$label))
})
