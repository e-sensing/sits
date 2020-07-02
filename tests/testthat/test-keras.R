context("Keras")
test_that("keras read write",{
    #skip_on_cran()
    library(keras)
    samples_mt_ndvi <- sits_select_bands(samples_mt_4bands, ndvi)
    model <- suppressMessages(suppressWarnings(sits_train(samples_mt_ndvi,
                                         sits_deeplearning(
                                             layers = c(128,128),
                                             dropout_rates = c(0.5, 0.4),
                                             epochs = 50,
                                             verbose = 0))))

    hdffile  <- "./model_keras.h5"
    rdsfile  <-  "./model_keras.rds"
    sits_keras_save(model = model, hdffile = hdffile, rdsfile = rdsfile)

    saved_model <- sits_keras_load(hdffile = hdffile, rdsfile = rdsfile)

    class.tb <- sits_classify(point_ndvi, saved_model)

    expect_true(all(class.tb$predicted[[1]]$class %in%
                        sits_labels(samples_mt_ndvi)$label))
    expect_true(nrow(sits_show_prediction(class.tb)) == 16)

    unlink(hdffile)
    unlink(rdsfile)
})
