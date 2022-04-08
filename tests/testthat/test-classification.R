test_that("Classify with random forest - single core and multicore", {
  samples_mt_ndvi <- sits_select(samples_modis_4bands, bands = "NDVI")
  rfor_model <- sits_train(samples_mt_ndvi, sits_rfor(num_trees = 40))

  expect_type(rfor_model, "closure")
  point_ndvi <- sits_select(point_mt_6bands, bands = "NDVI")
  class_ndvi <- sits_classify(
    data = point_ndvi,
    ml_model = rfor_model
  )

  expect_true(nrow(class_ndvi$predicted[[1]]) == 17)
  expect_true(all(class_ndvi$predicted[[1]]$class %in%
    sits_labels(samples_mt_ndvi)))
  point_ndvi <- sits_select(point_mt_6bands, bands = "NDVI")
  class_ndvi <- sits_classify(
    data = point_ndvi,
    ml_model = rfor_model,
    multicores = 2
  )

  expect_true(nrow(class_ndvi$predicted[[1]]) == 17)
  expect_true(all(class_ndvi$predicted[[1]]$class %in%
    sits_labels(samples_mt_ndvi)))
})

test_that("Classify a set of time series with svm + filter", {

  testthat::skip_on_cran()

  # single core
  samples_filt <-
    sits_select(samples_modis_4bands,
      bands = c("NDVI", "EVI")
    ) %>%
    sits_apply(NDVI = sits_sgolay(NDVI), EVI = sits_sgolay(EVI))

  svm_model <- sits_train(samples_filt, sits_svm())

  class1 <- sits_classify(cerrado_2classes,
    ml_model = svm_model,
    filter_fn = sits_sgolay(),
    multicores = 2
  )

  expect_true(class1$predicted[[1]]$class %in%
    sits_labels(cerrado_2classes))
})

test_that("Classify time series with TWDTW method", {
  testthat::skip_on_cran()
  samples_mt_ndvi <- sits_select(samples_modis_4bands, bands = "NDVI")
  patterns <- sits_patterns(samples_mt_ndvi)
  expect_true(all(sits_labels(patterns) %in% sits_labels(samples_mt_ndvi)))
  point_mt_ndvi <- sits_select(point_mt_6bands, bands = "NDVI")
  matches <- sits_twdtw_classify(point_mt_ndvi,
    patterns,
    bands = "NDVI",
    alpha = -0.1,
    beta = 100,
    theta = 0.5,
    keep = TRUE
  )


  expect_true(all(unique(matches$predicted[[1]]$predicted) %in%
    sits_labels(samples_mt_ndvi)))

  matches <- sits_twdtw_classify(point_mt_ndvi,
    patterns,
    bands = "NDVI",
    alpha = -0.1,
    beta = 100,
    theta = 0.5,
    start_date = "2005-01-01",
    end_date = "2015-12-31",
    keep = TRUE
  )
})

test_that("Classify error bands 1", {
  samples_mt_ndvi <- sits_select(samples_modis_4bands, bands = "NDVI")

  model <- sits_train(samples_mt_ndvi, sits_svm())
  point <- sits_select(point_mt_6bands, "EVI")

  expect_error(
    sits_classify(
      data = point,
      ml_model = model
    )
  )
})
