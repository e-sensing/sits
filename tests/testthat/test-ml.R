context("Machine Learning")
test_that("SVM",{
    #skip_on_cran()
    svm_model <- sits_train(samples_MT_ndvi, sits_svm(kernel = "radial", cost = 10))
    class.tb <- sits_classify(point_ndvi, svm_model)

    expect_true(all(class.tb$predicted[[1]]$class %in%
                                  sits_labels(samples_MT_ndvi)$label))
})

test_that("Random Forest",{
    rfor_model <- sits_train(samples_MT_ndvi, sits_rfor(num.trees = 200))
    class.tb <- sits_classify(point_ndvi, rfor_model)

    expect_true(all(class.tb$predicted[[1]]$class %in%
                                  sits_labels(samples_MT_ndvi)$label))
})

test_that("LDA",{
    lda_model <- sits_train(samples_MT_ndvi, sits_lda())
    class.tb <- sits_classify(point_ndvi, lda_model)

    expect_true(all(class.tb$predicted[[1]]$class %in%
                                  sits_labels(samples_MT_ndvi)$label))
})

test_that("QDA",{
    qda_model <- sits_train(samples_MT_ndvi, sits_qda())
    class.tb <- sits_classify(point_ndvi, qda_model)

    expect_true(all(class.tb$predicted[[1]]$class %in%
                                  sits_labels(samples_MT_ndvi)$label))
})

test_that("DL",{
    dl_model <- sits_train(samples_MT_ndvi, sits_deeplearning(epochs = 20))
    class.tb <- sits_classify(point_ndvi, dl_model)

    expect_true(all(class.tb$predicted[[1]]$class %in%
                        sits_labels(samples_MT_ndvi)$label))
})

test_that("GBM",{
    gbm_model <- sits_train(samples_MT_ndvi, sits_gbm(n.trees = 20))
    class.tb <- sits_classify(point_ndvi, gbm_model)

    expect_true(all(class.tb$predicted[[1]]$class %in%
                        sits_labels(samples_MT_ndvi)$label))
})

test_that("MLR",{
    mlr_model <- sits_train(samples_MT_ndvi, sits_mlr(maxit = 30))
    class.tb <- sits_classify(point_ndvi, mlr_model)

    expect_true(all(class.tb$predicted[[1]]$class %in%
                        sits_labels(samples_MT_ndvi)$label))
})

test_that("Keras diagnostics",{
    data(cerrado_2classes)
    dl_model <- sits_train(cerrado_2classes,
                           sits_deeplearning(units = c(512, 512),
                                             epochs = 20,
                                             dropout_rates = c(0.45, 0.25)))

    expect_true(sits_keras_diagnostics(dl_model))
})
