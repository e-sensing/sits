context("Raster classification")


test_that("Multi-year, single core classification", {
    #
    samples_mt_ndvi <- sits_select(samples_mt_4bands, bands = "NDVI")
    rfor_model <- sits_train(samples_mt_ndvi, sits_rfor(num_trees = 200))
    files <- c(system.file("extdata/raster/mod13q1/sinop-crop-ndvi.tif",
        package = "sits"
    ))
    data("timeline_modis_392")
    sinop <- sits_cube(
        type = "BRICK",
        name = "sinop-crop",
        timeline = timeline_modis_392,
        satellite = "TERRA",
        sensor = "MODIS",
        bands = "NDVI",
        files = files
    )
    # classify using one core
    sinop_probs <- suppressMessages(
        sits_classify(sinop,
                      rfor_model,
                      output_dir = tempdir(),
                      memsize = 2,
                      multicores = 1
        )
    )

    # Retrieve values and test them
    probs1 <- sinop_probs$file_info[[1]]$path[1]
    r_obj <- suppressWarnings(terra::rast(probs1))
    max_lyr1 <- max(terra::values(r_obj)[, 1])
    expect_true(max_lyr1 > 8000)

    max_lyr2 <- max(terra::values(r_obj)[, 2])
    expect_true(max_lyr2 < 1000)

    # retrieve the output raster layers
    expect_true(all(file.exists(unlist(sinop_probs$file_info[[1]]$path))))
    expect_true(all(file.remove(unlist(sinop_probs$file_info[[1]]$path))))
})

test_that("Multi-year, multi-core classification", {
    # skip_on_cran()
    files <- c(system.file("extdata/raster/mod13q1/sinop-crop-ndvi.tif",
        package = "sits"
    ))
    data("timeline_modis_392")
    sinop <- sits_cube(
        type = "BRICK",
        name = "sinop-crop",
        timeline = timeline_modis_392,
        satellite = "TERRA",
        sensor = "MODIS",
        bands = "ndvi",
        files = files
    )

    r_obj <- suppressWarnings(terra::rast(sinop$file_info[[1]]$path[1]))
    expect_true(terra::nrow(r_obj) == sinop$nrows)
    expect_true(terra::xmin(r_obj) == sinop$xmin)

    samples_mt_ndvi <- sits_select(samples_mt_4bands, bands = "NDVI")
    svm_model <- sits_train(samples_mt_ndvi, sits_svm())

    # classify using multicores
    sinop_probs <- suppressMessages(
        sits_classify(sinop,
                      svm_model,
                      output_dir = tempdir(),
                      memsize = 4,
                      multicores = 2
        )
    )

    expect_true(all(file.exists(unlist(sinop_probs$file_info[[1]]$path))))
    rc_obj <- suppressWarnings(terra::rast(sinop_probs$file_info[[1]]$path[1]))
    expect_true(terra::nrow(rc_obj) == sinop_probs$nrows)

    max_lyr1 <- max(terra::values(rc_obj)[, 1])
    expect_true(max_lyr1 > 9000)

    max_lyr2 <- max(terra::values(rc_obj)[, 2])
    expect_true(max_lyr2 < 1000)

    expect_true(all(file.remove(unlist(sinop_probs$file_info[[1]]$path))))
})

test_that("One-year, single core classification", {
    samples_2bands <- sits_select(samples_mt_4bands, bands = c("NDVI", "EVI"))
    dl_model <- sits_train(samples_2bands, sits_deeplearning(
        layers = c(256, 256, 256),
        dropout_rates = c(0.5, 0.4, 0.3),
        epochs = 80,
        batch_size = 64,
        verbose = 0
    ))

    ndvi_file <- c(system.file("extdata/raster/mod13q1/sinop-ndvi-2014.tif",
        package = "sits"
    ))

    evi_file <- c(system.file("extdata/raster/mod13q1/sinop-evi-2014.tif",
        package = "sits"
    ))

    data("timeline_2013_2014")

    sinop_cube <- sits_cube(
        type = "BRICK",
        name = "sinop-2014",
        timeline = timeline_2013_2014,
        satellite = "TERRA",
        sensor = "MODIS",
        bands = c("ndvi", "evi"),
        files = c(ndvi_file, evi_file)
    )

    sinop_probs <- suppressMessages(
        sits_classify(sinop_cube,
                      dl_model,
                      output_dir = tempdir(),
                      memsize = 4,
                      multicores = 1
        )
    )

    expect_true(all(file.exists(unlist(sinop_probs$file_info[[1]]$path))))
    rc_obj <- suppressWarnings(terra::rast(sinop_probs$file_info[[1]]$path[1]))
    expect_true(terra::nrow(rc_obj) == sinop_probs$nrows)

    max_lyr1 <- max(terra::values(rc_obj)[, 1])
    expect_true(max_lyr1 < 4500)

    max_lyr3 <- max(terra::values(rc_obj)[, 3])
    expect_true(max_lyr3 > 7000)

    expect_true(all(file.remove(unlist(sinop_probs$file_info[[1]]$path))))
})

test_that("One-year, multicore classification", {

    samples_2bands <- sits_select(samples_mt_4bands, bands = c("NDVI", "EVI"))

    svm_model <- sits_train(samples_2bands, sits_svm())

    ndvi_file <- c(system.file("extdata/raster/mod13q1/sinop-ndvi-2014.tif",
        package = "sits"
    ))

    evi_file <- c(system.file("extdata/raster/mod13q1/sinop-evi-2014.tif",
        package = "sits"
    ))

    data("timeline_2013_2014")

    sinop_cube <- sits_cube(
        type = "BRICK",
        name = "sinop-2014",
        timeline = timeline_2013_2014,
        satellite = "TERRA",
        sensor = "MODIS",
        bands = c("ndvi", "evi"),
        files = c(ndvi_file, evi_file)
    )

    sinop_probs <- suppressMessages(
        sits_classify(sinop_cube,
                      svm_model,
                      output_dir = tempdir(),
                      memsize = 4,
                      multicores = 2
        )
    )

    expect_true(all(file.exists(unlist(sinop_probs$file_info[[1]]$path))))
    rc_obj <- suppressWarnings(terra::rast(sinop_probs$file_info[[1]]$path[1]))
    expect_true(terra::nrow(rc_obj) == sinop_probs$nrows)

    max_lyr2 <- max(terra::values(rc_obj)[, 2])
    expect_true(max_lyr2 < 1000)

    max_lyr3 <- max(terra::values(rc_obj)[, 3])
    expect_true(max_lyr3 > 8000)

    expect_true(all(file.remove(unlist(sinop_probs$file_info[[1]]$path))))
})

test_that("One-year, single core classification with filter", {
    samples_2bands <- sits_select(samples_mt_4bands, bands = c("NDVI", "EVI"))
    samples_filt <- sits_whittaker(samples_2bands, bands_suffix = "")
    svm_model <- sits_train(samples_filt, sits_svm())

    ndvi_file <- c(system.file("extdata/raster/mod13q1/sinop-ndvi-2014.tif",
        package = "sits"
    ))

    evi_file <- c(system.file("extdata/raster/mod13q1/sinop-evi-2014.tif",
        package = "sits"
    ))

    data("timeline_2013_2014")

    sinop_cube <- sits_cube(
        type = "BRICK",
        name = "sinop-2014",
        timeline = timeline_2013_2014,
        satellite = "TERRA",
        sensor = "MODIS",
        bands = c("ndvi", "evi"),
        files = c(ndvi_file, evi_file)
    )

    sinop_probs <- suppressMessages(
        sits_classify(
            data = sinop_cube,
            ml_model = svm_model,
            filter = sits_whittaker(lambda = 3.0),
            output_dir = tempdir(),
            memsize = 4,
            multicores = 1
        )
    )

    expect_true(all(file.exists(unlist(sinop_probs$file_info[[1]]$path))))
    expect_true(all(file.remove(unlist(sinop_probs$file_info[[1]]$path))))
})

test_that("One-year, multicore classification with filter", {
    samples_2bands <- sits_select(samples_mt_4bands, bands = c("NDVI", "EVI"))
    samples_filt <- sits_sgolay(samples_2bands, bands_suffix = "")
    svm_model <- sits_train(samples_filt, sits_svm())

    ndvi_file <- c(system.file("extdata/raster/mod13q1/sinop-ndvi-2014.tif",
        package = "sits"
    ))

    evi_file <- c(system.file("extdata/raster/mod13q1/sinop-evi-2014.tif",
        package = "sits"
    ))

    data("timeline_2013_2014")

    sinop_2014 <- sits_cube(
        type = "BRICK",
        name = "sinop-2014",
        timeline = timeline_2013_2014,
        satellite = "TERRA",
        sensor = "MODIS",
        bands = c("ndvi", "evi"),
        files = c(ndvi_file, evi_file)
    )

    sinop_2014_probs <- suppressMessages(
        sits_classify(
            data = sinop_2014,
            ml_model = svm_model,
            filter = sits_whittaker(lambda = 3.0),
            output_dir = tempdir(),
            memsize = 4,
            multicores = 2
        )
    )
    expect_true(all(file.exists(unlist(sinop_2014_probs$file_info[[1]]$path))))

    rc_obj <- suppressWarnings(
        terra::rast(sinop_2014_probs$file_info[[1]]$path[1])
    )
    expect_true(terra::nrow(rc_obj) == sinop_2014_probs$nrows)

    max_lyr2 <- max(terra::values(rc_obj)[, 2])
    expect_true(max_lyr2 < 1000)

    max_lyr3 <- max(terra::values(rc_obj)[, 3])
    expect_true(max_lyr3 > 8000)

    expect_true(all(file.remove(unlist(sinop_2014_probs$file_info[[1]]$path))))
})

test_that("One-year, multicore classification with post-processing", {
    samples_2bands <- sits_select(samples_mt_4bands, bands = c("NDVI", "EVI"))

    svm_model <- sits_train(samples_2bands, sits_svm())

    ndvi_file <- c(system.file("extdata/raster/mod13q1/sinop-ndvi-2014.tif",
        package = "sits"
    ))

    evi_file <- c(system.file("extdata/raster/mod13q1/sinop-evi-2014.tif",
        package = "sits"
    ))

    data("timeline_2013_2014")

    sinop_cube <- sits_cube(
        type = "BRICK",
        name = "sinop-2014",
        timeline = timeline_2013_2014,
        satellite = "TERRA",
        sensor = "MODIS",
        bands = c("ndvi", "evi"),
        files = c(ndvi_file, evi_file)
    )

    sinop_probs <- suppressMessages(
        sits_classify(
            sinop_cube,
            svm_model,
            output_dir = tempdir(),
            memsize = 4,
            multicores = 2
        )
    )

    expect_true(all(file.exists(unlist(sinop_probs$file_info[[1]]$path))))

    sinop_class <- sits::sits_label_classification(sinop_probs,
                                                   output_dir = tempdir()
    )
    expect_true(all(file.exists(unlist(sinop_class$file_info[[1]]$path))))

    expect_true(length(sits_timeline(sinop_class)) ==
                    length(sits_timeline(sinop_probs))
    )

    r_obj <- terra::rast(sinop_class$file_info[[1]]$path[1])
    max_lab <- max(terra::values(r_obj))
    min_lab <- min(terra::values(r_obj))
    expect_equal(max_lab, 6)
    expect_equal(min_lab, 1)

    sinop_majority <- sits_label_majority(sinop_class,
                                          output_dir = tempdir()
    )
    expect_true(all(file.exists(unlist(sinop_majority$file_info[[1]]$path))))
    r_maj <- suppressWarnings(
        terra::rast(sinop_majority$file_info[[1]]$path[1])
    )
    max_maj <- max(terra::values(r_maj))
    min_maj <- min(terra::values(r_maj))
    expect_equal(max_maj, 6)
    expect_equal(min_maj, 1)


    sinop_bayes <- sits::sits_smooth(sinop_probs,
                                     output_dir = tempdir()
    )
    expect_true(all(file.exists(unlist(sinop_bayes$file_info[[1]]$path))))

    expect_true(length(sits_timeline(sinop_bayes)) ==
                    length(sits_timeline(sinop_probs))
    )

    r_bay <- suppressWarnings(terra::rast(sinop_bayes$file_info[[1]]$path[1]))
    expect_true(terra::nrow(r_bay) == sinop_probs$nrows)

    max_bay2 <- max(terra::values(r_bay)[, 2])
    expect_true(max_bay2 < 1000)

    max_bay3 <- max(terra::values(r_bay)[, 3])
    expect_true(max_bay3 > 8000)

    sinop_gauss <- sits::sits_smooth(sinop_probs, type = "gaussian",
                                     output_dir = tempdir(),
                                     memsize = 4,
                                     multicores = 2
    )
    expect_true(all(file.exists(unlist(sinop_gauss$file_info[[1]]$path))))

    r_gau <- suppressWarnings(terra::rast(sinop_gauss$file_info[[1]]$path[1]))
    expect_true(terra::nrow(r_gau) == sinop_probs$nrows)

    max_gau2 <- max(terra::values(r_gau)[, 2])
    expect_true(max_gau2 < 1000)

    max_gau3 <- max(terra::values(r_gau)[, 3])
    expect_true(max_gau3 > 8000)

    sinop_bil <- sits::sits_smooth(sinop_probs, type = "bilinear",
                                     output_dir = tempdir()
    )
    expect_true(all(file.exists(unlist(sinop_bil$file_info[[1]]$path))))

    r_bil <- suppressWarnings(terra::rast(sinop_bil$file_info[[1]]$path[1]))
    expect_true(terra::nrow(r_bil) == sinop_probs$nrows)

    max_bil2 <- max(terra::values(r_bil)[, 2])
    expect_true(max_bil2 < 1000)

    max_bil3 <- max(terra::values(r_bil)[, 3])
    expect_true(max_bil3 > 8000)


    expect_true(all(file.remove(unlist(sinop_class$file_info[[1]]$path))))
    expect_true(all(file.remove(unlist(sinop_bayes$file_info[[1]]$path))))
    expect_true(all(file.remove(unlist(sinop_gauss$file_info[[1]]$path))))
    expect_true(all(file.remove(unlist(sinop_bil$file_info[[1]]$path))))
    expect_true(all(file.remove(unlist(sinop_majority$file_info[[1]]$path))))

    expect_true(all(file.remove(unlist(sinop_probs$file_info[[1]]$path))))
})


test_that("Check GDAL access", {
    files <- c(system.file("extdata/raster/mod13q1/sinop-crop-ndvi.tif",
        package = "sits"
    ))
    expect_true(sits:::.sits_raster_api_check_access(files[1]))
})

test_that("Raster filename", {
    file <- sits:::.sits_raster_api_filename(
        output_dir = "./",
        version = "v1",
        name = "sinop",
        type = "probs",
        start_date = "2018-08-01",
        end_date = "2019-07-31"
    )

    expect_true(as.logical(grep("sinop_probs_2018_8_2019_7", file)))
})
