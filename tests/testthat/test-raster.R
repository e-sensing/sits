context("Raster classification")

test_that("One-year, single core classification", {
    samples_2bands <- sits_select(samples_mt_4bands, bands = c("NDVI", "EVI"))
    dl_model <- sits_train(samples_2bands, sits_deeplearning(
        layers = c(256, 256, 256),
        dropout_rates = c(0.5, 0.4, 0.3),
        epochs = 80,
        batch_size = 64,
        verbose = 0
    ))

    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    sinop <- sits_cube(
        source = "LOCAL",
        name = "sinop-2014",
        satellite = "TERRA",
        sensor = "MODIS",
        data_dir = data_dir,
        delim = "_",
        parse_info = c("X1", "X2", "band", "date")
    )
    sinop_probs <- suppressMessages(
        sits_classify(sinop,
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
    expect_true(max_lyr1 <= 10000)

    max_lyr3 <- max(terra::values(rc_obj)[, 3])
    expect_true(max_lyr3 <= 10000)

    expect_true(all(file.remove(unlist(sinop_probs$file_info[[1]]$path))))
})

test_that("One-year, multicore classification", {

    samples_2bands <- sits_select(samples_mt_4bands, bands = c("NDVI", "EVI"))

    svm_model <- sits_train(samples_2bands, sits_svm())

    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    sinop <- sits_cube(
        source = "LOCAL",
        name = "sinop-2014",
        satellite = "TERRA",
        sensor = "MODIS",
        data_dir = data_dir,
        delim = "_",
        parse_info = c("X1", "X2", "band", "date")
    )

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

    max_lyr2 <- max(terra::values(rc_obj)[, 2])
    expect_true(max_lyr2 <= 10000)

    max_lyr3 <- max(terra::values(rc_obj)[, 3])
    expect_true(max_lyr3 <= 10000)

    expect_true(all(file.remove(unlist(sinop_probs$file_info[[1]]$path))))
})

test_that("One-year, single core classification with filter", {
    samples_2bands <- sits_select(samples_mt_4bands, bands = c("NDVI", "EVI"))
    samples_filt <- sits_whittaker(samples_2bands, bands_suffix = "")
    svm_model <- sits_train(samples_filt, sits_svm())

    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    sinop <- sits_cube(
        source = "LOCAL",
        name = "sinop-2014",
        satellite = "TERRA",
        sensor = "MODIS",
        data_dir = data_dir,
        delim = "_",
        parse_info = c("X1", "X2", "band", "date")
    )

    sinop_probs <- suppressMessages(
        sits_classify(
            data = sinop,
            ml_model = svm_model,
            filter_fn = sits_whittaker(lambda = 3.0),
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

    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    sinop <- sits_cube(
        source = "LOCAL",
        name = "sinop-2014",
        satellite = "TERRA",
        sensor = "MODIS",
        data_dir = data_dir,
        delim = "_",
        parse_info = c("X1", "X2", "band", "date")
    )

    sinop_2014_probs <- suppressMessages(
        sits_classify(
            data = sinop,
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
    expect_true(max_lyr2 <= 10000)

    max_lyr3 <- max(terra::values(rc_obj)[, 3])
    expect_true(max_lyr3 <= 10000)

    expect_true(all(file.remove(unlist(sinop_2014_probs$file_info[[1]]$path))))
})

test_that("One-year, multicore classification with post-processing", {
    samples_2bands <- sits_select(samples_mt_4bands, bands = c("NDVI", "EVI"))

    svm_model <- sits_train(samples_2bands, sits_svm())

    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    sinop <- sits_cube(
        source = "LOCAL",
        name = "sinop-2014",
        satellite = "TERRA",
        sensor = "MODIS",
        data_dir = data_dir,
        delim = "_",
        parse_info = c("X1", "X2", "band", "date")
    )

    sinop_probs <- suppressMessages(
        sits_classify(
            sinop,
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
    expect_true(max_lab <= 9)
    expect_true(min_lab >= 1)

    sinop_majority <- sits_label_majority(sinop_class,
                                          output_dir = tempdir()
    )
    expect_true(all(file.exists(unlist(sinop_majority$file_info[[1]]$path))))
    r_maj <- suppressWarnings(
        terra::rast(sinop_majority$file_info[[1]]$path[1])
    )
    max_maj <- max(terra::values(r_maj))
    min_maj <- min(terra::values(r_maj))
    expect_true(max_maj <= 9)
    expect_true(min_maj >= 1)


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
    expect_true(max_bay2 <= 10000)

    max_bay3 <- max(terra::values(r_bay)[, 3])
    expect_true(max_bay3 <= 10000)

    sinop_gauss <- sits::sits_smooth(sinop_probs, type = "gaussian",
                                     output_dir = tempdir(),
                                     memsize = 4,
                                     multicores = 2
    )
    expect_true(all(file.exists(unlist(sinop_gauss$file_info[[1]]$path))))

    r_gau <- suppressWarnings(terra::rast(sinop_gauss$file_info[[1]]$path[1]))
    expect_true(terra::nrow(r_gau) == sinop_probs$nrows)

    max_gau2 <- max(terra::values(r_gau)[, 2])
    expect_true(max_gau2 <= 10000)

    max_gau3 <- max(terra::values(r_gau)[, 3])
    expect_true(max_gau3 <= 10000)

    sinop_bil <- sits::sits_smooth(sinop_probs, type = "bilinear",
                                     output_dir = tempdir()
    )
    expect_true(all(file.exists(unlist(sinop_bil$file_info[[1]]$path))))

    r_bil <- suppressWarnings(terra::rast(sinop_bil$file_info[[1]]$path[1]))
    expect_true(terra::nrow(r_bil) == sinop_probs$nrows)

    max_bil2 <- max(terra::values(r_bil)[, 2])
    expect_true(max_bil2 <= 10000)

    max_bil3 <- max(terra::values(r_bil)[, 3])
    expect_true(max_bil3 <= 10000)


    expect_true(all(file.remove(unlist(sinop_class$file_info[[1]]$path))))
    expect_true(all(file.remove(unlist(sinop_bayes$file_info[[1]]$path))))
    expect_true(all(file.remove(unlist(sinop_gauss$file_info[[1]]$path))))
    expect_true(all(file.remove(unlist(sinop_bil$file_info[[1]]$path))))
    expect_true(all(file.remove(unlist(sinop_majority$file_info[[1]]$path))))

    expect_true(all(file.remove(unlist(sinop_probs$file_info[[1]]$path))))
})


test_that("Check GDAL access", {
    file <- c(system.file("extdata/raster/mod13q1/TERRA_MODIS_EVI_2013-09-14.jp2",
        package = "sits"
    ))
    expect_true(sits:::.sits_raster_api_check_access(file))
})
