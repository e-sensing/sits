testthat::context("Machine Learning")
testthat::test_that("SVM, Random Forest, LDA, QDA, LASSO model",{
    testthat::skip_on_cran()
    svm_model <- sits_train(samples_MT_ndvi, sits_svm(kernel = "radial", cost = 10))
    class.tb <- sits_classify(point_ndvi, samples_MT_ndvi, svm_model)

    testthat::expect_true(all(class.tb$predicted[[1]]$class %in%
                                  sits_labels(samples_MT_ndvi)$label))

    rfor_model <- sits_train(samples_MT_ndvi, sits_rfor(ntree = 200))
    class.tb <- sits_classify(point_ndvi, samples_MT_ndvi, rfor_model)

    testthat::expect_true(all(class.tb$predicted[[1]]$class %in%
                                  sits_labels(samples_MT_ndvi)$label))

    lda_model <- sits_train(samples_MT_ndvi, sits_lda())
    class.tb <- sits_classify(point_ndvi, samples_MT_ndvi, lda_model)

    testthat::expect_true(all(class.tb$predicted[[1]]$class %in%
                                  sits_labels(samples_MT_ndvi)$label))

    qda_model <- sits_train(samples_MT_ndvi, sits_qda())
    class.tb <- sits_classify(point_ndvi, samples_MT_ndvi, qda_model)

    testthat::expect_true(all(class.tb$predicted[[1]]$class %in%
                                  sits_labels(samples_MT_ndvi)$label))
})


