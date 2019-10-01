context("Raster classification")
test_that("Working with raster data cubes", {
    #skip_on_cran()
    files <- c(system.file("extdata/raster/mod13q1/sinop-crop-ndvi.tif",
                           package = "sits"))
    data("timeline_modis_392")
    sinop <- sits_cube(name = "Sinop-crop", timeline = timeline_modis_392,
                       satellite = "TERRA", sensor = "MODIS",
                       bands = "ndvi", files = files)

    r_obj <- sits:::.sits_cube_robj(sinop)
    expect_true(raster::nrow(r_obj) == sinop$nrows)
    expect_true(raster::xmin(r_obj) == sinop$xmin)

    samples_mt_ndvi <- sits_select_bands(samples_mt_4bands, ndvi)
    rfor_model <- sits_train(samples_mt_ndvi, sits_svm())

    # classify using multicores
    sinop_probs <- sits_classify(sinop, rfor_model, memsize = 4, multicores = 1)

    expect_true(all(file.exists(unlist(sinop_probs$files))))
    rc_obj <- sits:::.sits_cube_robj(sinop_probs)
    expect_true(raster::nrow(rc_obj) == sinop_probs$nrows)

    expect_error(sits:::.sits_raster)

    sinop_class <- sits::sits_label_classification(sinop_probs)
    expect_true(all(file.exists(unlist(sinop_class$files))))

    sinop_bayes <- sits::sits_label_classification(sinop_probs,
                                                   smoothing = "bayesian")
    expect_true(all(file.exists(unlist(sinop_bayes$files))))

    rc_obj2 <- sits:::.sits_cube_robj(sinop_bayes)
    expect_true(raster::nrow(rc_obj2) == sinop_bayes$nrows)
    expect_true(raster::nrow(rc_obj2) == raster::nrow(rc_obj))

    sinop_majority <- sits_label_classification(sinop_probs,
                                                smoothing = "majority")
    expect_true(all(file.exists(unlist(sinop_majority$files))))
    rc_obj3 <- sits:::.sits_cube_robj(sinop_majority)
    expect_true(raster::nrow(rc_obj2) == sinop_majority$nrows)
    expect_true(raster::nrow(rc_obj3) == raster::nrow(rc_obj))


    sinop_majority_bayes <- sits_label_classification(sinop_probs,
                                            smoothing = "bayesian+majority")
    expect_true(all(file.exists(unlist(sinop_majority_bayes$files))))
    rc_obj4 <- sits:::.sits_cube_robj(sinop_majority_bayes)
    expect_true(raster::nrow(rc_obj4) == sinop_majority$nrows)
    expect_true(raster::nrow(rc_obj4) == raster::nrow(rc_obj))

    expect_true(length(sits_timeline(sinop_majority_bayes)) ==
                    length(sits_timeline(sinop_probs)))
    expect_true(length(sits_timeline(sinop_bayes)) ==
                    length(sits_timeline(sinop_probs)))
    expect_true(length(sits_timeline(sinop_class)) ==
                    length(sits_timeline(sinop_probs)))

    expect_true(sits_timeline(sinop_class)[1] == sits_timeline(sinop_probs)[1])
    expect_true(sits_timeline(sinop_bayes)[1] == sits_timeline(sinop_probs)[1])

    expect_true(all(file.remove(unlist(sinop_class$files))))
    expect_true(all(file.remove(unlist(sinop_probs$files))))
    expect_true(all(file.remove(unlist(sinop_bayes$files))))
    expect_true(all(file.remove(unlist(sinop_majority$files))))
    expect_true(all(file.remove(unlist(sinop_majority_bayes$files))))
})

test_that("Single core classification", {
    #
    svm_model <- sits_train(samples_mt_ndvi, sits_svm())
    # classify using one core
    sinop_probs <- sits_classify(sinop, rfor_model, memsize = 2, multicores = 1)

    # retrieve the output raster layers
    bricks_probs <- .sits_cube_all_robjs(sinop_probs)

    expect_true(all(file.exists(unlist(sinop_probs$files))))
    rc_obj <- sits:::.sits_cube_robj(sinop_probs)
    expect_true(raster::nrow(rc_obj) == sinop_probs$nrows)

    expect_true(all(file.remove(unlist(sinop_probs$files))))

})

test_that("Check webfile access", {
    files <- c("https://landsat-modis.s3-sa-east-1.amazonaws.com/Sinop_evi.tif")
    files2 <- sits:::.sits_raster_check_webfiles(files)
    expect_true(as.logical(grep("vsicurl", files2)))

})

test_that("Multicore classification of one year", {
    samples_mt_2bands <- sits_select_bands(samples_mt_4bands, ndvi, evi)
    svm_model <- sits_train(samples_mt_2bands, sits_svm())

    ndvi_file <- c(system.file("extdata/raster/mod13q1/Sinop_evi_2014.tif",
                              package = "sits"))

    evi_file <- c(system.file("extdata/raster/mod13q1/Sinop_evi_2014.tif",
                           package = "sits"))

    data("timeline_2013_2014")

    sinop_2014 <- sits_cube(name = "Sinop-2014", timeline = timeline_2013_2014,
                       satellite = "TERRA", sensor = "MODIS",
                       bands = c("ndvi", "evi"), files = c(ndvi_file, evi_file))

    sinop_2014_probs <- sits_classify(sinop_2014, svm_model, memsize = 4, multicores = 2)

    expect_true(all(file.exists(unlist(sinop_2014_probs$files))))
    rc_obj <- sits:::.sits_cube_robj(sinop_2014_probs)
    expect_true(raster::nrow(rc_obj) == sinop_2014_probs$nrows)

    expect_true(all(file.remove(unlist(sinop_2014_probs$files))))

})

test_that("Check GDAL access and Brick files", {
    files <- c(system.file("extdata/raster/mod13q1/sinop-crop-ndvi.tif",
                           package = "sits"))
    expect_true(sits:::.sits_raster_check_gdal_access(files))

    expect_true(sits:::.sits_raster_check_bricks(files))

})

test_that("Raster Cube", {

    cube <- sits:::.sits_raster_cube(service = "BRICK",
                URL = "http://127.0.0.1",
                satellite = "TERRA",
                sensor = "MODIS",
                name = "Sinop",
                timeline = timeline_modis_392,
                bands = "ndvi",
                files = c(system.file("extdata/raster/mod13q1/sinop-crop-ndvi.tif",
                          package = "sits"))
                )
    expect_true(cube$nrows == 11 && cube$ncols == 14)
})

test_that("Raster filename", {

    file <- sits:::.sits_raster_filename(output_dir = "./",
                                         version = "v1",
                                         name = "Sinop",
                                         type = "probs",
                                         start_date = "2018-08-01",
                                         end_date = "2019-07-31")

    expect_true(as.logical(grep("Sinop_probs_2018_8_2019_7", file)))
})

test_that("Guess satellite", {
    file <- c(system.file("extdata/raster/mod13q1/sinop-crop-ndvi.tif",
                           package = "sits"))
    satellite <- sits:::.sits_raster_guess_satellite(raster::brick(file))

    expect_true(satellite == "TERRA")
})
