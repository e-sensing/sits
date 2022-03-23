test_that("SVM  - Formula logref", {
  # skip_on_cran()
  samples_mt_ndvi <- sits_select(samples_modis_4bands, bands = "NDVI")
  svm_model <- sits_train(
    samples_mt_ndvi,
    sits_svm(
      formula = sits_formula_logref(),
      kernel = "radial",
      cost = 10
    )
  )
  point_ndvi <- sits_select(point_mt_6bands, bands = "NDVI")
  point_class <- sits_classify(
    data = point_ndvi,
    ml_model = svm_model
  )

  expect_true(sits_labels(point_class) == "NoClass")

  expect_true(all(point_class$predicted[[1]]$class %in%
    sits_labels(samples_mt_ndvi)))
  expect_true(nrow(sits_show_prediction(point_class)) == 17)
})

test_that("SVM  - Formula logref - difference", {
  # skip_on_cran()
  samples_mt_2bands <- sits_select(samples_modis_4bands,
    bands = c("NDVI", "EVI")
  )
  svm_model <- sits_train(
    data = samples_mt_2bands,
    ml_method = sits_svm(
      formula = sits_formula_logref(),
      kernel = "radial",
      cost = 10
    )
  )
  point_class <- sits_classify(
    data = cerrado_2classes[1:100, ],
    ml_model = svm_model,
    multicores = 2
  )

  expect_true(all(point_class$predicted[[1]]$class %in%
    sits_labels(samples_mt_2bands)))
  expect_true(nrow(sits_show_prediction(point_class)) == 100)
})

test_that("SVM - Formula linear", {
  # skip_on_cran()
  samples_mt_ndvi <- sits_select(samples_modis_4bands, bands = "NDVI")
  svm_model <- sits_train(
    samples_mt_ndvi,
    sits_svm(
      formula = sits_formula_linear(),
      kernel = "radial",
      cost = 10
    )
  )
  point_ndvi <- sits_select(point_mt_6bands, bands = "NDVI")
  point_class <- sits_classify(
    data = point_ndvi,
    ml_model = svm_model
  )

  expect_true(all(point_class$predicted[[1]]$class %in%
    sits_labels(samples_mt_ndvi)))
  expect_true(nrow(sits_show_prediction(point_class)) == 17)
})


test_that("Random Forest", {
  samples_mt_ndvi <- sits_select(samples_modis_4bands, bands = "NDVI")
  rfor_model <- sits_train(samples_mt_ndvi, sits_rfor(num_trees = 200))
  point_ndvi <- sits_select(point_mt_6bands, bands = "NDVI")
  point_class <- sits_classify(
    data = point_ndvi,
    ml_model = rfor_model
  )

  expect_true(all(point_class$predicted[[1]]$class %in%
    sits_labels(samples_mt_ndvi)))
  expect_true(nrow(sits_show_prediction(point_class)) == 17)
})
test_that("Random Forest - Whittaker", {
  samples_mt_ndvi <- sits_select(samples_modis_4bands, bands = "NDVI")
  samples_mt_whit <- sits_filter(samples_mt_ndvi, filter = sits_whittaker())
  rfor_model <- sits_train(samples_mt_whit, sits_rfor(num_trees = 200))
  point_ndvi <- sits_select(point_mt_6bands, bands = "NDVI")
  point_whit <- sits_filter(point_ndvi, filter = sits_whittaker())
  point_class <- sits_classify(
    data = point_whit,
    ml_model = rfor_model
  )

  expect_true(all(point_class$predicted[[1]]$class %in%
    sits_labels(samples_mt_ndvi)))
  expect_true(nrow(sits_show_prediction(point_class)) == 17)
})
test_that("Random Forest - SGolay", {
  samples_mt_ndvi <- sits_select(samples_modis_4bands, bands = "NDVI")
  samples_mt_sg <- sits_filter(samples_mt_ndvi, filter = sits_sgolay())
  rfor_model <- sits_train(samples_mt_sg, sits_rfor(num_trees = 200))
  point_ndvi <- sits_select(point_mt_6bands, bands = "NDVI")
  point_sg <- sits_filter(point_ndvi, filter = sits_sgolay())
  point_class <- sits_classify(
    data = point_sg,
    ml_model = rfor_model
  )

  expect_true(all(point_class$predicted[[1]]$class %in%
    sits_labels(samples_mt_ndvi)))
  expect_true(nrow(sits_show_prediction(point_class)) == 17)
})
test_that("Random Forest", {
  samples_mt_ndvi <- sits_select(samples_modis_4bands, bands = "NDVI")
  rfor_model <- sits_train(samples_mt_ndvi, sits_rfor(num_trees = 200))
  point_ndvi <- sits_select(point_mt_6bands, bands = "NDVI")
  point_class <- sits_classify(
    data = point_ndvi,
    ml_model = rfor_model
  )

  expect_true(all(point_class$predicted[[1]]$class %in%
    sits_labels(samples_mt_ndvi)))
  expect_true(nrow(sits_show_prediction(point_class)) == 17)
})
test_that("MLR", {
  # skip_on_cran()
  samples_mt_ndvi <- sits_select(samples_modis_4bands, bands = "NDVI")
  model <- sits_train(samples_mt_ndvi, sits_mlr())
  point_ndvi <- sits_select(point_mt_6bands, bands = "NDVI")
  point_class <- sits_classify(
    data = point_ndvi,
    ml_model = model
  )

  expect_true(all(point_class$predicted[[1]]$class %in%
    sits_labels(samples_mt_ndvi)))
  expect_true(nrow(sits_show_prediction(point_class)) == 17)
})

test_that("XGBoost", {
  # skip_on_cran()
  samples_mt_ndvi <- sits_select(samples_modis_4bands, bands = "NDVI")
  model <- sits_train(
    samples_mt_ndvi,
    sits_xgboost(
      nrounds = 30,
      verbose = FALSE
    )
  )
  point_ndvi <- sits_select(point_mt_6bands, bands = "NDVI")
  point_class <- sits_classify(
    data = point_ndvi,
    ml_model = model
  )

  expect_true(all(point_class$predicted[[1]]$class %in%
    sits_labels(samples_mt_ndvi)))
  expect_true(nrow(sits_show_prediction(point_class)) == 17)
})
test_that("lightGBM", {
  lgbm_model <- sits_train(samples_modis_4bands, sits_lightgbm)
  point_4class <- sits_select(point_mt_6bands,
    bands = sits_bands(samples_modis_4bands)
  )
  point_class <- sits_classify(point_4class, lgbm_model)

  expect_true(all(point_class$predicted[[1]]$class %in%
    sits_labels(samples_modis_4bands)))
  expect_true(nrow(sits_show_prediction(point_class)) == 17)
})
test_that("DL-MLP", {
  # skip_on_cran()
  model <- sits_train(
      samples_modis_4bands,
      sits_mlp(
          layers = c(128, 128),
          dropout_rates = c(0.5, 0.4),
          epochs = 50,
          verbose = 0
      )
  )
  point_4bands <- sits_select(point_mt_6bands,
                              bands = sits_bands(samples_modis_4bands)
  )

  point_class <- sits_classify(
    data = point_4bands,
    ml_model = model
  )

  expect_true(all(point_class$predicted[[1]]$class %in%
    sits_labels(samples_modis_4bands)))
  expect_true(nrow(sits_show_prediction(point_class)) == 17)
})

test_that("ResNet", {
  # skip_on_cran()
  samples_ndvi <- sits_select(samples_modis_4bands, bands = "NDVI")

  model <- tryCatch({
      sits_train(samples_ndvi, sits_ResNet(epochs = 50))
  },
  error = function(e) {
      return(NULL)
  }
  )
  if (purrr::is_null(model))
      skip("ResNet model not working")

  point_ndvi <- sits_select(point_mt_6bands, bands = "NDVI")
  point_class <- sits_classify(
      data = point_ndvi,
      ml_model = model
  )

  expect_true(all(point_class$predicted[[1]]$class %in%
    sits_labels(samples_ndvi)))
  expect_true(nrow(sits_show_prediction(point_class)) == 17)
})
test_that("tempCNN model", {
  # skip_on_cran()
  model <- sits_train(samples_modis_4bands, sits_TempCNN())
  point_4bands <- sits_select(point_mt_6bands,
    bands = c("NDVI", "EVI", "NIR", "MIR")
  )
  point_class <-
    sits_classify(
      data = point_4bands,
      ml_model = model
    )

  expect_true(all(point_class$predicted[[1]]$class %in%
    sits_labels(samples_modis_4bands)))
  expect_true(nrow(sits_show_prediction(point_class)) == 17)
})


test_that("normalization", {
  stats <- .sits_ml_normalization_param(cerrado_2classes)

  norm1 <- .sits_ml_normalize_data(
    cerrado_2classes,
    stats
  )

  stats1 <- .sits_ml_normalization_param(norm1)
  expect_true(stats1[2, NDVI] < 0.1)
  expect_true(stats1[3, NDVI] > 0.99)

  norm2 <- .sits_ml_normalize_data(
    cerrado_2classes,
    stats
  )

  stats2 <- .sits_ml_normalization_param(norm2)

  expect_equal(stats1[1, NDVI], stats2[1, NDVI], tolerance = 0.001)
  expect_equal(stats1[2, NDVI], stats2[2, NDVI], tolerance = 0.001)

  norm3 <- .sits_ml_normalize_data(cerrado_2classes, stats)

  stats3 <- .sits_ml_normalization_param(norm3)

  expect_equal(stats1[1, NDVI], stats3[1, NDVI], tolerance = 0.001)
  expect_equal(stats1[2, NDVI], stats3[2, NDVI], tolerance = 0.001)
})
