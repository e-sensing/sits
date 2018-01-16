testthat::context("Classification of time series")
testthat::test_that("Classify a time series the simplest way", {
    testthat::skip_on_cran()
    data(samples_MT_ndvi)
    data(point_ndvi)
    class_ndvi.tb <-  sits_classify(point_ndvi, samples_MT_ndvi)

    testthat::expect_true(NROW(class_ndvi.tb$predicted[[1]]) == 16)
    testthat::expect_true(all(class_ndvi.tb$predicted[[1]]$class %in%
                                  sits_labels(samples_MT_ndvi)$label))
})
testthat::test_that("Classify time series with an explicit model",{
    testthat::skip_on_cran()
    data(samples_MT_ndvi)
    samples.tb <- sits_select(samples_MT_9classes,
                              bands = c("ndvi","evi"))
    model <- sits_train(samples.tb)
    data(ts_2000_2016)
    point.tb <- sits_select(ts_2000_2016, bands = c("ndvi","evi"))
    class.tb <- sits_classify_model(point.tb, samples.tb, model)

    testthat::expect_true(all(class.tb$predicted[[1]]$class %in%
                                  sits_labels(samples_MT_9classes)$label))
})

testthat::test_that("Classify time series with TWDTW method",{
    data("point_ndvi")
    data(samples_MT_ndvi)
    patterns.tb <- sits_patterns(samples_MT_ndvi)
    matches <- sits_TWDTW_classify(ts_2000_2016, patterns.tb, bands = c("ndvi"),
                                   alpha = -0.1, beta = 100, theta = 0.5, keep = TRUE)

    testthat::expect_true(all(unique(matches$predicted[[1]]$predicted) %in%
                                  sits_labels(samples_MT_ndvi)$label))
})
