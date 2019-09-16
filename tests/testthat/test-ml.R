context("Machine Learning")
test_that("SVM",{
    #skip_on_cran()
    samples_mt_ndvi <- sits_select_bands(samples_mt_4bands, ndvi)
    svm_model <- sits_train(samples_mt_ndvi, sits_svm(kernel = "radial", cost = 10))
    class.tb <- sits_classify(point_ndvi, svm_model)

    expect_true(all(class.tb$predicted[[1]]$class %in%
                                  sits_labels(samples_mt_ndvi)$label))
    expect_true(nrow(sits_show_prediction(class.tb)) == 16)
})

test_that("Random Forest",{
    samples_mt_ndvi <- sits_select_bands(samples_mt_4bands, ndvi)
    rfor_model <- sits_train(samples_mt_ndvi, sits_rfor(num_trees = 200))
    class.tb <- sits_classify(point_ndvi, rfor_model)

    expect_true(all(class.tb$predicted[[1]]$class %in%
                                  sits_labels(samples_mt_ndvi)$label))
    expect_true(nrow(sits_show_prediction(class.tb)) == 16)
})

test_that("LDA",{
    samples_mt_ndvi <- sits_select_bands(samples_mt_4bands, ndvi)
    lda_model <- sits_train(samples_mt_ndvi, sits_lda())
    class.tb <- sits_classify(point_ndvi, lda_model)

    expect_true(all(class.tb$predicted[[1]]$class %in%
                                  sits_labels(samples_mt_ndvi)$label))
    expect_true(nrow(sits_show_prediction(class.tb)) == 16)
})

test_that("QDA",{
    samples_mt_ndvi <- sits_select_bands(samples_mt_4bands, ndvi)
    qda_model <- sits_train(samples_mt_ndvi, sits_qda())
    class.tb <- sits_classify(point_ndvi, qda_model)

    expect_true(all(class.tb$predicted[[1]]$class %in%
                                  sits_labels(samples_mt_ndvi)$label))
    expect_true(nrow(sits_show_prediction(class.tb)) == 16)
})
test_that("MLR",{
    #skip_on_cran()
    samples_mt_ndvi <- sits_select_bands(samples_mt_4bands, ndvi)
    model <- sits_train(samples_mt_ndvi, sits_mlr())

    class.tb <- sits_classify(point_ndvi, model)

    expect_true(all(class.tb$predicted[[1]]$class %in%
                        sits_labels(samples_mt_ndvi)$label))
    expect_true(nrow(sits_show_prediction(class.tb)) == 16)
})

test_that("XGBoost",{
    #skip_on_cran()
    samples_mt_ndvi <- sits_select_bands(samples_mt_4bands, ndvi)
    model <- sits_train(samples_mt_ndvi, sits_xgboost())

    class.tb <- sits_classify(point_ndvi, model)

    expect_true(all(class.tb$predicted[[1]]$class %in%
                        sits_labels(samples_mt_ndvi)$label))
    expect_true(nrow(sits_show_prediction(class.tb)) == 16)
})
test_that("DL-MLP",{
    #skip_on_cran()
    samples_mt_ndvi <- sits_select_bands(samples_mt_4bands, ndvi)
    model <- suppressWarnings(sits_train(samples_mt_ndvi, sits_deeplearning(
        layers = c(128,128), dropout_rates = c(0.5, 0.4), epochs = 50, verbose = 0)))

    class.tb <- sits_classify(point_ndvi, model)

    expect_true(all(class.tb$predicted[[1]]$class %in%
                        sits_labels(samples_mt_ndvi)$label))
    expect_true(nrow(sits_show_prediction(class.tb)) == 16)
})
test_that("1D CNN model",{
    #skip_on_cran()
    samples_mt_ndvi <- sits_select_bands(samples_mt_4bands, ndvi)
    model <- suppressWarnings(sits_train(samples_mt_ndvi, sits_FCN(layers = c(32,32), kernels = c(9, 5),
                                                                   epochs = 50, verbose = 0)))

    class.tb <- sits_classify(point_ndvi, model)

    expect_true(all(class.tb$predicted[[1]]$class %in%
                        sits_labels(samples_mt_ndvi)$label))
    expect_true(nrow(sits_show_prediction(class.tb)) == 16)
})
test_that("tempCNN model",{
    #skip_on_cran()
    samples_mt_ndvi <- sits_select_bands(samples_mt_4bands, ndvi)
    model <- suppressWarnings(sits_train(samples_mt_ndvi, sits_TempCNN(
        cnn_layers = c(32, 32), cnn_kernels = c(7, 5),
        cnn_dropout_rates = c(0.5, 0.4),
        mlp_layers = c(128), mlp_dropout_rates = c(0.5),
        epochs = 50, verbose = 0)))

    class.tb <- sits_classify(point_ndvi, model)

    expect_true(all(class.tb$predicted[[1]]$class %in%
                        sits_labels(samples_mt_ndvi)$label))
    expect_true(nrow(sits_show_prediction(class.tb)) == 16)
})
test_that("ResNet",{
    #skip_on_cran()
    samples_mt_ndvi <- sits_select_bands(samples_mt_4bands, ndvi)
    model <- suppressWarnings(sits_train(samples_mt_ndvi, sits_ResNet(
        blocks = c(16, 16, 16), kernels = c(7,5,3), epochs = 50, verbose = 0)))

    class.tb <- sits_classify(point_ndvi, model)

    expect_true(all(class.tb$predicted[[1]]$class %in%
                        sits_labels(samples_mt_ndvi)$label))
    expect_true(nrow(sits_show_prediction(class.tb)) == 16)
})

test_that("LSTM",{
    #skip_on_cran()
    samples_mt_ndvi <- sits_select_bands(samples_mt_4bands, ndvi)
    model <- suppressWarnings(sits_train(samples_mt_ndvi, sits_LSTM_FCN(
        cnn_layers = c(16, 16, 16), epochs = 50, verbose = 0)))

    class.tb <- sits_classify(point_ndvi, model)

    expect_true(all(class.tb$predicted[[1]]$class %in%
                        sits_labels(samples_mt_ndvi)$label))
    expect_true(nrow(sits_show_prediction(class.tb)) == 16)
})
