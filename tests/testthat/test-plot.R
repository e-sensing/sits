context("Plot")

test_that("All", {
    data("cerrado_2classes")
    sits_plot(cerrado_2classes)
    sits_plot(cerrado_2classes[1:20,])
    sits_plot(sits_patterns(cerrado_2classes))
    samples_mt_ndvi <- sits_select_bands(samples_mt_6bands, ndvi)
    data(point_ndvi)
    model_svm <- sits_train(samples_mt_ndvi, ml_method = sits_rfor())
    class_ndvi.tb <-  sits_classify(point_ndvi, model_svm)
    sits_plot(class_ndvi.tb)

    samples_mt_ndvi <- sits_select_bands(samples_mt_6bands, ndvi)
    svm_model <- sits_train(samples_mt_ndvi, sits_rfor())
    files  <- c(system.file("extdata/raster/mod13q1/sinop-crop-ndvi.tif", package = "sits"))
    data(timeline_modis_392)
    sinop <- sits_cube(name  = "Sinop-crop", timeline = timeline_modis_392, bands = "ndvi", files = files)
    sinop_probs <- sits_classify(sinop, ml_model = svm_model, memsize = 1, multicores = 1)
    sinop_labels <- sits_label_classification(sinop_probs)
    sits_plot_raster(sinop_labels, time = 1, title = "SINOP class 2000-2001")

    expect_true(all(file.remove(unlist(sinop_probs$files))))
    expect_true(all(file.remove(unlist(sinop_labels$files))))

})


