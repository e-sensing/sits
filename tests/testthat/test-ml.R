testthat::context("Machine Learning")
testthat::test_that("SVM, Random Forest, LDA, QDA, LASSO model",{
    testthat::skip_on_cran()
    class.tb <- sits_classify(point_ndvi, samples_MT_ndvi,
        ml_method = sits_svm(kernel = "radial", cost = 10))

    testthat::expect_true(all(class.tb$predicted[[1]]$class %in%
                                  sits_labels(samples_MT_ndvi)$label))

    class.tb <- sits_classify(point_ndvi, samples_MT_ndvi,
                              ml_method = sits_rfor(ntree = 200))

    testthat::expect_true(all(class.tb$predicted[[1]]$class %in%
                                  sits_labels(samples_MT_ndvi)$label))

    class.tb <- sits_classify(point_ndvi, samples_MT_ndvi,
                              ml_method = sits_lda())

    testthat::expect_true(all(class.tb$predicted[[1]]$class %in%
                                  sits_labels(samples_MT_ndvi)$label))

    class.tb <- sits_classify(point_ndvi, samples_MT_ndvi,
                              ml_method = sits_qda())

    testthat::expect_true(all(class.tb$predicted[[1]]$class %in%
                                  sits_labels(samples_MT_ndvi)$label))
})


