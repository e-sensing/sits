context("Raster classification")
test_that("Working with raster data cubes", {
    #skip_on_cran()
    files <- c(system.file("extdata/raster/mod13q1/sinop-crop-ndvi.tif", package = "sits"))
    data("timeline_modis_392")
    sinop <- sits_cube(name = "Sinop-crop", timeline = timeline_modis_392, bands = "ndvi", files = files)

    r_obj <- sits:::.sits_cube_robj(sinop)
    expect_true(raster::nrow(r_obj) == sinop$nrows)
    expect_true(raster::xmin(r_obj) == sinop$xmin)

    point.tb <- sits_get_data(sinop, longitude = -55.55502, latitude = -11.52774)

    expect_true(length(sits_time_series_dates(point.tb)) == length(timeline_modis_392))

    svm_model <- sits_train(samples_mt_ndvi, sits_svm())

    sinop_probs <- sits_classify(sinop, svm_model, memsize = 4, multicores = 2)

    expect_true(all(file.exists(unlist(sinop_probs$files))))
    rc_obj <- sits:::.sits_cube_robj(sinop_probs)
    expect_true(raster::nrow(rc_obj) == sinop_probs$nrows)

    sinop_class <- sits::sits_label_classification(sinop_probs)
    expect_true(all(file.exists(unlist(sinop_class$files))))

    sinop_bayes <- sits::sits_label_classification(sinop_probs, smoothing = "bayesian")
    expect_true(all(file.exists(unlist(sinop_bayes$files))))

    rc_obj2 <- sits:::.sits_cube_robj(sinop_bayes)
    expect_true(raster::nrow(rc_obj2) == sinop_bayes$nrows)
    expect_true(raster::nrow(rc_obj2) == raster::nrow(rc_obj))

    sinop_majority <- sits_label_classification(sinop_probs, smoothing = "majority")
    expect_true(all(file.exists(unlist(sinop_majority$files))))
    rc_obj3 <- sits:::.sits_cube_robj(sinop_majority)
    expect_true(raster::nrow(rc_obj2) == sinop_majority$nrows)
    expect_true(raster::nrow(rc_obj3) == raster::nrow(rc_obj))


    sinop_majority_bayes <- sits_label_classification(sinop_probs, smoothing = "bayesian+majority")
    expect_true(all(file.exists(unlist(sinop_majority_bayes$files))))
    rc_obj4 <- sits:::.sits_cube_robj(sinop_majority_bayes)
    expect_true(raster::nrow(rc_obj4) == sinop_majority$nrows)
    expect_true(raster::nrow(rc_obj4) == raster::nrow(rc_obj))

    expect_true(length(sits_timeline(sinop_majority_bayes)) == length(sits_timeline(sinop_probs)))
    expect_true(length(sits_timeline(sinop_bayes)) == length(sits_timeline(sinop_probs)))
    expect_true(length(sits_timeline(sinop_class)) == length(sits_timeline(sinop_probs)))

    expect_true(sits_timeline(sinop_class)[1] == sits_timeline(sinop_probs)[1])
    expect_true(sits_timeline(sinop_bayes)[1] == sits_timeline(sinop_probs)[1])

    expect_true(all(file.remove(unlist(sinop_class$files))))
    expect_true(all(file.remove(unlist(sinop_probs$files))))
    expect_true(all(file.remove(unlist(sinop_bayes$files))))
    expect_true(all(file.remove(unlist(sinop_majority$files))))
    expect_true(all(file.remove(unlist(sinop_majority_bayes$files))))
})
