context("Machine Learning")
test_that("SVM  - Formula logref", {
    # skip_on_cran()
    samples_mt_ndvi <- sits_select(samples_mt_4bands, bands = "NDVI")
    svm_model <- sits_train(
        samples_mt_ndvi,
        sits_svm(
            formula = sits_formula_logref(),
            kernel = "radial",
            cost = 10
        )
    )
    point_class <- sits_classify(point_ndvi, svm_model)

    expect_true(sits_labels(point_class)$label == "NoClass")

    expect_true(all(point_class$predicted[[1]]$class %in%
        sits_labels(samples_mt_ndvi)$label))
    expect_true(nrow(sits_show_prediction(point_class)) == 16)
})

test_that("SVM  - Formula logref - difference", {
    # skip_on_cran()
    samples_mt_2bands <- sits_select(samples_mt_4bands,
                                     bands = c("NDVI", "EVI")
    )
    svm_model <- sits_train(
        samples_mt_2bands,
        sits_svm(
            formula = sits_formula_logref(),
            kernel = "radial",
            cost = 10
        )
    )
    point_class <- sits_classify(cerrado_2classes[1:100, ],
                                 svm_model,
                                 multicores = 2
    )

    expect_true(all(point_class$predicted[[1]]$class %in%
        sits_labels(samples_mt_2bands)$label))
    expect_true(nrow(sits_show_prediction(point_class)) == 100)
})

test_that("SVM - Formula linear", {
    # skip_on_cran()
    samples_mt_ndvi <- sits_select(samples_mt_4bands, bands = "NDVI")
    svm_model <- sits_train(
        samples_mt_ndvi,
        sits_svm(
            formula = sits_formula_linear(),
            kernel = "radial",
            cost = 10
        )
    )
    point_class <- sits_classify(point_ndvi, svm_model)

    expect_true(all(point_class$predicted[[1]]$class %in%
        sits_labels(samples_mt_ndvi)$label))
    expect_true(nrow(sits_show_prediction(point_class)) == 16)
})


test_that("Random Forest", {
    samples_mt_ndvi <- sits_select(samples_mt_4bands, bands = "NDVI")
    rfor_model <- sits_train(samples_mt_ndvi, sits_rfor(num_trees = 200))
    point_class <- sits_classify(point_ndvi, rfor_model)

    expect_true(all(point_class$predicted[[1]]$class %in%
        sits_labels(samples_mt_ndvi)$label))
    expect_true(nrow(sits_show_prediction(point_class)) == 16)
})

test_that("Random Forest - Ranger", {
    samples_mt_ndvi <- sits_select(samples_mt_4bands, bands = "NDVI")
    ranger_model <- sits_train(samples_mt_ndvi, sits_ranger(num_trees = 200))
    point_class <- sits_classify(point_ndvi, ranger_model)

    expect_true(all(point_class$predicted[[1]]$class %in%
                        sits_labels(samples_mt_ndvi)$label))
    expect_true(nrow(sits_show_prediction(point_class)) == 16)
})

test_that("LDA", {
    samples_mt_ndvi <- sits_select(samples_mt_4bands, bands = "NDVI")
    lda_model <- sits_train(samples_mt_ndvi, sits_lda())
    point_class <- sits_classify(point_ndvi, lda_model)

    expect_true(all(point_class$predicted[[1]]$class %in%
        sits_labels(samples_mt_ndvi)$label))
    expect_true(nrow(sits_show_prediction(point_class)) == 16)
})

test_that("QDA", {
    samples_mt_ndvi <- sits_select(samples_mt_4bands, bands = "NDVI")
    qda_model <- sits_train(samples_mt_ndvi, sits_qda())
    point_class <- sits_classify(point_ndvi, qda_model)

    expect_true(all(point_class$predicted[[1]]$class %in%
        sits_labels(samples_mt_ndvi)$label))
    expect_true(nrow(sits_show_prediction(point_class)) == 16)
})
test_that("MLR", {
    # skip_on_cran()
    samples_mt_ndvi <- sits_select(samples_mt_4bands, bands = "NDVI")
    model <- sits_train(samples_mt_ndvi, sits_mlr())

    point_class <- sits_classify(point_ndvi, model)

    expect_true(all(point_class$predicted[[1]]$class %in%
        sits_labels(samples_mt_ndvi)$label))
    expect_true(nrow(sits_show_prediction(point_class)) == 16)
})

test_that("XGBoost", {
    # skip_on_cran()
    samples_mt_ndvi <- sits_select(samples_mt_4bands, bands = "NDVI")
    model <- sits_train(samples_mt_ndvi,
                        sits_xgboost(nrounds = 30,
                                     verbose = FALSE)
    )

    point_class <- sits_classify(point_ndvi, model)

    expect_true(all(point_class$predicted[[1]]$class %in%
        sits_labels(samples_mt_ndvi)$label))
    expect_true(nrow(sits_show_prediction(point_class)) == 16)
})
test_that("DL-MLP", {
    # skip_on_cran()
    Sys.setenv(TF_CPP_MIN_LOG_LEVEL = "1")
    samples_mt_2bands <- sits_select(samples_mt_4bands,
                                     bands = c("NDVI", "EVI")
    )
    model <- suppressMessages(suppressWarnings(
        sits_train(
            samples_mt_2bands,
            sits_deeplearning(
                layers = c(128, 128),
                dropout_rates = c(0.5, 0.4),
                epochs = 50,
                verbose = 0
            )
        )
    ))

    point_2bands <- sits_select(point_mt_6bands, bands = c("NDVI", "EVI"))

    point_class <- sits_classify(point_2bands, model)

    expect_true(all(point_class$predicted[[1]]$class %in%
        sits_labels(samples_mt_2bands)$label))
    expect_true(nrow(sits_show_prediction(point_class)) == 17)
})

test_that("DL-MLP-2classes", {
    # skip_on_cran()
    samples_mt_2bands <- sits_select(samples_mt_4bands,
                                     bands = c("NDVI", "EVI")
    )
    model <- suppressMessages(suppressWarnings(
        sits_train(
            samples_mt_2bands,
            sits_deeplearning(
                layers = c(64, 64, 64),
                dropout_rates = c(0.5, 0.4, 0.3),
                epochs = 50,
                verbose = 0
            )
        )
    ))
    test_eval <- suppressMessages(sits_keras_diagnostics(model))
    expect_true(test_eval["accuracy"] > 0.7)

    point_class <- sits_classify(cerrado_2classes[1:60, ], model)

    expect_true(all(point_class$predicted[[1]]$class %in%
        sits_labels(samples_mt_2bands)$label))
    expect_true(nrow(sits_show_prediction(point_class)) == 60)
})
test_that("1D CNN model", {
    # skip_on_cran()
    samples_mt_ndvi <- sits_select(samples_mt_4bands, bands = "NDVI")
    model <- suppressMessages(suppressWarnings(
        sits_train(
            samples_mt_ndvi,
            sits_FCN(
                layers = c(32, 32),
                kernels = c(9, 5),
                epochs = 50,
                verbose = 0
            )
        )
    ))
    test_eval <- suppressMessages(sits_keras_diagnostics(model))
    expect_true(test_eval["accuracy"] > 0.5)

    point_class <- sits_classify(point_ndvi, model)

    expect_true(all(point_class$predicted[[1]]$class %in%
        sits_labels(samples_mt_ndvi)$label))
    expect_true(nrow(sits_show_prediction(point_class)) == 16)
})
test_that("tempCNN model", {
    # skip_on_cran()
    samples_mt_ndvi <- sits_select(samples_mt_4bands, bands = "NDVI")
    model <- suppressMessages(suppressWarnings(
        sits_train(
            samples_mt_ndvi,
            sits_TempCNN(
                cnn_layers = c(32, 32),
                cnn_kernels = c(7, 5),
                cnn_dropout_rates = c(0.5, 0.4),
                mlp_layers = c(128),
                mlp_dropout_rates = c(0.5),
                epochs = 50,
                verbose = 0
            )
        )
    ))

    test_eval <- suppressMessages(sits_keras_diagnostics(model))
    expect_true(test_eval["accuracy"] > 0.7)

    point_class <- sits_classify(point_ndvi, model)

    expect_true(all(point_class$predicted[[1]]$class %in%
        sits_labels(samples_mt_ndvi)$label))
    expect_true(nrow(sits_show_prediction(point_class)) == 16)
})
test_that("ResNet", {
    # skip_on_cran()
    samples_mt_ndvi <- sits_select(samples_mt_4bands, bands = "NDVI")
    model <- suppressMessages(suppressWarnings(sits_train(
        samples_mt_ndvi,
        sits_ResNet(
            blocks = c(16, 16, 16),
            kernels = c(7, 5, 3),
            epochs = 50,
            verbose = 0
        )
    )))

    point_class <- sits_classify(point_ndvi, model)

    expect_true(all(point_class$predicted[[1]]$class %in%
        sits_labels(samples_mt_ndvi)$label))
    expect_true(nrow(sits_show_prediction(point_class)) == 16)
})

test_that("LSTM", {
    # skip_on_cran()
    samples_mt_ndvi <- sits_select(samples_mt_4bands, bands = "NDVI")
    model <- suppressWarnings(sits_train(samples_mt_ndvi, sits_LSTM_FCN(
        cnn_layers = c(16, 16, 16), epochs = 50, verbose = 0
    )))

    point_class <- sits_classify(point_ndvi, model)

    expect_true(all(point_class$predicted[[1]]$class %in%
        sits_labels(samples_mt_ndvi)$label))
    expect_true(nrow(sits_show_prediction(point_class)) == 16)
})

test_that("normalization", {
    stats <- sits:::.sits_normalization_param(cerrado_2classes)

    norm1 <- sits:::.sits_normalize_data(cerrado_2classes,
        stats,
        multicores = 1
    )

    stats1 <- sits:::.sits_normalization_param(norm1)
    expect_true(stats1[2, NDVI] < 0.1)
    expect_true(stats1[3, NDVI] > 0.99)

    norm2 <- sits:::.sits_normalize_data(cerrado_2classes,
        stats,
        multicores = 2
    )

    stats2 <- sits:::.sits_normalization_param(norm2)

    expect_equal(stats1[1, NDVI], stats2[1, NDVI], tolerance = 0.001)
    expect_equal(stats1[2, NDVI], stats2[2, NDVI], tolerance = 0.001)

    norm3 <- sits:::.sits_normalize_data(cerrado_2classes, stats)

    stats3 <- sits:::.sits_normalization_param(norm3)

    expect_equal(stats1[1, NDVI], stats3[1, NDVI], tolerance = 0.001)
    expect_equal(stats1[2, NDVI], stats3[2, NDVI], tolerance = 0.001)
})
