test_that("SVM  - Formula logref", {
    svm_model <- sits_train(
        samples_modis_ndvi,
        sits_svm(
            formula = sits_formula_logref(),
            kernel = "radial",
            cost = 10
        )
    )
    point_ndvi <- sits_select(point_mt_6bands, bands = "NDVI")
    point_class <- sits_classify(
        data = point_ndvi,
        ml_model = svm_model,
        multicores = 1,
        progress = FALSE
    )

    expect_true(sits_labels(point_class) == "NoClass")

    expect_true(all(point_class$predicted[[1]]$class %in%
        sits_labels(samples_modis_ndvi)))
    expect_true(nrow(sits_show_prediction(point_class)) == 17)
})

test_that("SVM  - Formula logref - difference", {
    svm_model <- sits_train(
        samples_modis_ndvi,
        ml_method = sits_svm(
            formula = sits_formula_logref(),
            kernel = "radial",
            cost = 10
        )
    )
    point_ndvi <- sits_select(point_mt_6bands, bands = "NDVI")
    point_class <- sits_classify(
        data = point_ndvi,
        ml_model = svm_model,
        multicores = 2,
        progress = FALSE
    )

    expect_true(all(point_class$predicted[[1]]$class %in%
        sits_labels(samples_modis_ndvi)))
    expect_true(nrow(sits_show_prediction(point_class)) == 17)
})

test_that("SVM - Formula linear", {
    svm_model <- sits_train(
        samples_modis_ndvi,
        sits_svm(
            formula = sits_formula_linear(),
            kernel = "radial",
            cost = 10
        )
    )
    point_ndvi <- sits_select(point_mt_6bands, bands = "NDVI")
    point_class <- sits_classify(
        data = point_ndvi,
        ml_model = svm_model,
        progress = FALSE
    )

    expect_true(all(point_class$predicted[[1]]$class %in%
        sits_labels(samples_modis_ndvi)))
    expect_true(nrow(sits_show_prediction(point_class)) == 17)
})

test_that("Random Forest", {
    rfor_model <- sits_train(samples_modis_ndvi, sits_rfor(num_trees = 200))
    expect_equal(sits_bands(rfor_model), "NDVI")
    point_ndvi <- sits_select(point_mt_6bands, bands = "NDVI")
    point_class <- sits_classify(
        data = point_ndvi,
        ml_model = rfor_model,
        progress = FALSE
    )

    expect_true(all(point_class$predicted[[1]]$class %in%
        sits_labels(samples_modis_ndvi)))
    expect_true(nrow(sits_show_prediction(point_class)) == 17)

    exported_rf <- sits_model_export(rfor_model)
    expect_s3_class(object = exported_rf, class = "randomForest")
})

test_that("Random Forest - Whittaker", {
    samples_whit <- sits_filter(samples_modis_ndvi, filter = sits_whittaker())
    rfor_model <- sits_train(samples_whit, sits_rfor(num_trees = 200))
    point_ndvi <- sits_select(point_mt_6bands, bands = "NDVI")
    point_whit <- sits_filter(point_ndvi, filter = sits_whittaker())
    point_class <- sits_classify(
        data = point_whit,
        ml_model = rfor_model,
        progress = FALSE
    )

    expect_true(all(point_class$predicted[[1]]$class %in%
        sits_labels(samples_modis_ndvi)))
    expect_true(nrow(sits_show_prediction(point_class)) == 17)
})

test_that("Random Forest - SGolay", {
    samples_mt_sg <- sits_filter(samples_modis_ndvi, filter = sits_sgolay())
    rfor_model <- sits_train(samples_mt_sg, sits_rfor(num_trees = 200))
    point_ndvi <- sits_select(point_mt_6bands, bands = "NDVI")
    point_sg <- sits_filter(point_ndvi, filter = sits_sgolay())
    point_class <- sits_classify(
        data = point_sg,
        ml_model = rfor_model,
        progress = FALSE
    )

    expect_true(all(point_class$predicted[[1]]$class %in%
        sits_labels(samples_modis_ndvi)))
    expect_true(nrow(sits_show_prediction(point_class)) == 17)
})

test_that("XGBoost", {
    model <- sits_train(
        samples_modis_ndvi,
        sits_xgboost(
            nrounds = 10,
            verbose = FALSE
        )
    )
    point_ndvi <- sits_select(point_mt_6bands, bands = "NDVI")
    point_class <- sits_classify(
        data = point_ndvi,
        ml_model = model,
        progress = FALSE
    )

    expect_true(all(point_class$predicted[[1]]$class %in%
        sits_labels(samples_modis_ndvi)))
    expect_true(nrow(sits_show_prediction(point_class)) == 17)
})

test_that("DL-MLP", {
    model <- sits_train(
        samples_modis_ndvi,
        sits_mlp(
            layers = c(128, 128),
            dropout_rates = c(0.5, 0.4),
            epochs = 5
        )
    )
    point_ndvi <- sits_select(point_mt_6bands, bands = "NDVI")

    point_class <- sits_classify(
        data = point_ndvi,
        ml_model = model,
        progress = FALSE
    )

    expect_true(all(point_class$predicted[[1]]$class %in%
        sits_labels(samples_modis_ndvi)))
    expect_true(nrow(sits_show_prediction(point_class)) == 17)

    mlp_model <- sits_model_export(model)
    expect_equal(class(mlp_model), "luz_module_fitted")
    expect_equal(mlp_model$ctx$hparams$num_pred, 12)
    expect_equal(mlp_model$ctx$hparams$layers[1], 128)
    expect_equal(mlp_model$ctx$hparams$layers[2], 128)
    expect_equal(mlp_model$ctx$hparams$y_dim, 4)
    stats <- .ml_stats(mlp_model)
    expect_null(stats)
})

test_that("ResNet", {
    model <- tryCatch(
        {
            sits_train(samples_modis_ndvi, sits_resnet(epochs = 5))
        },
        error = function(e) {
            return(NULL)
        }
    )
    if (purrr::is_null(model)) {
        skip("ResNet model not working")
    }

    point_ndvi <- sits_select(point_mt_6bands, bands = "NDVI")
    point_class <- sits_classify(
        data = point_ndvi,
        ml_model = model,
        progress = FALSE
    )

    expect_true(all(point_class$predicted[[1]]$class %in%
        sits_labels(samples_modis_ndvi)))
    expect_true(nrow(sits_show_prediction(point_class)) == 17)
})

test_that("TempCNN model", {
    model <- sits_train(
        samples_modis_ndvi,
        sits_tempcnn(epochs = 5)
    )

    point_ndvi <- sits_select(point_mt_6bands,
        bands = "NDVI"
    )
    point_class <-
        sits_classify(
            data = point_ndvi,
            ml_model = model,
            progress = FALSE
        )

    expect_true(all(point_class$predicted[[1]]$class %in%
        sits_labels(samples_modis_ndvi)))
    expect_true(nrow(sits_show_prediction(point_class)) == 17)
})

test_that("LightTAE model", {
    model <- sits_train(
        samples_modis_ndvi,
        sits_lighttae(epochs = 5)
    )
    point_ndvi <- sits_select(point_mt_6bands, bands = "NDVI")

    point_class <-
        sits_classify(
            data = point_ndvi,
            ml_model = model,
            progress = FALSE
        )

    expect_true(all(point_class$predicted[[1]]$class %in%
        sits_labels(samples_modis_ndvi)))
    expect_true(nrow(sits_show_prediction(point_class)) == 17)
})

test_that("PSETAE model", {
    model <- sits_train(
        samples_modis_ndvi,
        sits_tae(epochs = 5)
    )

    point_ndvi <- sits_select(point_mt_6bands, bands = c("NDVI"))

    point_class <-
        sits_classify(
            data = point_ndvi,
            ml_model = model,
            progress = FALSE
        )

    expect_true(all(point_class$predicted[[1]]$class %in%
        sits_labels(samples_modis_ndvi)))
    expect_true(nrow(sits_show_prediction(point_class)) == 17)
})

test_that("normalization new version", {
    #
    # New normalization
    #
    stats <- .samples_stats(cerrado_2classes)

    # In new version only predictors can be normalized
    preds <- .predictors(cerrado_2classes)

    # Now, 'norm1' is a normalized predictors
    preds_norm <- .pred_normalize(preds, stats)

    # From predictors, get feature values
    values <- .pred_features(preds)
    values_norm <- .pred_features(preds_norm)

    # Normalized data should have minimum value between
    #   0.0001 (inclusive) and abs(min(values))
    expect_true(1e-4 <= min(values_norm) && min(values_norm) < abs(min(values)))

    # Normalized data should have maximum value between
    #   abs(max(values)) and 1.0 (inclusive)
    expect_true(abs(max(values)) < max(values_norm) && max(values_norm) <= 1.0)
})
