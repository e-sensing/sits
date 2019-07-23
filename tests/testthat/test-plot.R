context("Plot")

test_that("All", {
    data ("cerrado_2classes")
    sits_plot (cerrado_2classes)
    sits_plot (cerrado_2classes[1:20,])
    sits_plot (sits_patterns(cerrado_2classes))
    data(samples_mt_ndvi)
    data(point_ndvi)
    model_svm <- sits_train(samples_mt_ndvi, ml_method = sits_svm())
    class_ndvi.tb <-  sits_classify(point_ndvi, model_svm)
    sits_plot(class_ndvi.tb)

    data(samples_mt_ndvi)
    svm_model <- sits_train(samples_mt_ndvi, sits_svm())
    files  <- c(system.file("extdata/raster/mod13q1/sinop-crop-ndvi.tif", package = "sits"))
    data(timeline_modis_392)
    raster.tb <- sits_cube(service = "RASTER", name  = "Sinop-crop",
                               timeline = timeline_modis_392, bands = "ndvi", files = files)
    raster_class.tb <- sits_classify(raster.tb, ml_model = svm_model, memsize = 1, multicores = 1,
                                     out_prefix = "raster-class")
    sits_plot_raster(raster_class.tb, time = 1, title = "SINOP class 2000-2001")

    expect_true(TRUE)
})


