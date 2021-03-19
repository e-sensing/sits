context("Keras")
test_that("keras read write", {
    samples_mt_ndvi <- sits_select(samples_mt_4bands, "NDVI")
    model <- suppressMessages(suppressWarnings(sits_train(
        samples_mt_ndvi,
        sits_deeplearning(
            layers = c(128, 128),
            dropout_rates = c(0.5, 0.4),
            epochs = 50,
            verbose = 0
        )
    )))

    hdffile <- paste0(tempdir(), "/model_keras.h5")
    rdsfile <- paste0(tempdir(), "/model_keras.rds")
    sits_keras_save(model = model, hdffile = hdffile, rdsfile = rdsfile)

    saved_model <- sits_keras_load(hdffile = hdffile, rdsfile = rdsfile)

    point_class <- sits_classify(point_ndvi, saved_model)

    expect_true(all(point_class$predicted[[1]]$class %in%
        sits_labels(samples_mt_ndvi)))
    expect_true(nrow(sits_show_prediction(point_class)) == 16)

    unlink(hdffile)
    unlink(rdsfile)
})
