context("Plot")

test_that("All", {
    data("cerrado_2classes")
    plot(cerrado_2classes)
    plot(cerrado_2classes[1:20,])
    plot(sits_patterns(cerrado_2classes))
    samples_mt_ndvi <- sits_select_bands(samples_mt_6bands, ndvi)
    data(point_ndvi)
    model_svm <- sits_train(samples_mt_ndvi, ml_method = sits_rfor())
    class_ndvi.tb <-  sits_classify(point_ndvi, model_svm)
    plot(class_ndvi.tb)

    samples_mt_ndvi <- sits_select_bands(samples_mt_6bands, ndvi)
    svm_model <- sits_train(samples_mt_ndvi, sits_rfor())
    files  <- c(system.file("extdata/raster/mod13q1/sinop-crop-ndvi.tif",
                            package = "sits"))
    data(timeline_modis_392)
    sinop <- sits_cube(name  = "Sinop-crop", timeline = timeline_modis_392,
                       bands = "ndvi", files = files)
    sinop_probs <- sits_classify(sinop, ml_model = svm_model,
                                 memsize = 1, multicores = 1)
    sinop_labels <- sits_label_classification(sinop_probs)
    plot(sinop_labels, time = 1, title = "SINOP class 2000-2001")

    expect_true(all(file.remove(unlist(sinop_probs$files))))
    expect_true(all(file.remove(unlist(sinop_labels$files))))

})

test_that("sits_plot",{
# Read a set of samples with 2 classes ("Cerrado" and "Pasture")data ("cerrado_2classes")
# Plot all the samples together
    data("cerrado_2classes")
    sits_plot (cerrado_2classes)
    # Plot the first 20 samples (defaults to "allyears")
    sits_plot(cerrado_2classes[1:20,])
    # Plot the patterns
    sits_plot(sits_patterns(cerrado_2classes))
    # Retrieve the set of samples for the Mato Grosso region (provided by EMBRAPA)
    data(samples_mt_4bands)
    samples_mt_ndvi <- sits_select_bands(samples_mt_4bands, bands = ndvi)
    model_svm <- sits_train(samples_mt_ndvi, ml_method = sits_svm())
    # Retrieve a point
    data(point_ndvi)
    # classify the point
    class_ndvi.tb <-  sits_classify(point_ndvi, model_svm)
    # plot the classification
    sits_plot(class_ndvi.tb)
})
