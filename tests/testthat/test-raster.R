test_that("One-year, single core classification", {
    rfor_model <- sits_train(
        samples_modis_ndvi,
        sits_rfor(num_trees = 30)
    )

    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    sinop <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6",
        data_dir = data_dir,
        delim = "_",
        bands = "NDVI",
        parse_info = c("X1", "tile", "band", "date")
    )
    sinop_probs <- sits_classify(
        data = sinop,
        ml_model = rfor_model,
        output_dir = tempdir(),
        memsize = 4,
        multicores = 1,
        version = "test1"
    )

    # testing resume feature
    expect_message(
        object = { sits_classify(
            data = sinop,
            ml_model = rfor_model,
            output_dir = tempdir(),
            memsize = 4,
            multicores = 1,
            version = "test1"
        ) },
        regexp = "Recovery: "
    )

    sits_labels(sinop_probs) <- c(
        "Cerrado", "Floresta",
        "Pastagem", "Soja_Milho"
    )
    expect_true(all(sits_labels(sinop_probs) %in%
                        c("Cerrado", "Floresta", "Pastagem", "Soja_Milho")))
    expect_true(all(file.exists(unlist(sinop_probs$file_info[[1]]$path))))
    r_obj <- .raster_open_rast(sinop_probs$file_info[[1]]$path[[1]])

    expect_true(.raster_nrows(r_obj) == .tile_nrows(sinop_probs))

    max_lyr1 <- max(.raster_get_values(r_obj)[, 1], na.rm = TRUE)
    expect_true(max_lyr1 <= 10000)

    max_lyr3 <- max(.raster_get_values(r_obj)[, 3], na.rm = TRUE)
    expect_true(max_lyr3 <= 10000)

    expect_true(all(file.remove(unlist(sinop_probs$file_info[[1]]$path))))
})

test_that("One-year, multicore classification", {

    timeline_samples <- sits_timeline(samples_modis_ndvi)
    start_date <- timeline_samples[1]
    end_date <- timeline_samples[length(timeline_samples)]

    svm_model <- sits_train(samples_modis_ndvi, sits_svm())

    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    sinop <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6",
        data_dir = data_dir,
        delim = "_",
        bands = "NDVI",
        parse_info = c("X1", "tile", "band", "date")
    )

    sinop_probs <- tryCatch(
        {
            suppressMessages(
                sits_classify(
                    data = sinop,
                    ml_model = svm_model,
                    start_date = start_date,
                    end_date = end_date,
                    output_dir = tempdir(),
                    memsize = 4,
                    multicores = 2,
                    verbose = TRUE,
                    version = "test2"
                )
            )
        },
        error = function(e) {
            return(NULL)
        }
    )
    if (purrr::is_null(sinop_probs)) {
        skip("Unable to allocate multicores")
    }
    sinop_probs <- tryCatch(
        {
            suppressMessages(
                sits_classify(
                    data = sinop,
                    ml_model = svm_model,
                    start_date = start_date,
                    end_date = end_date,
                    output_dir = tempdir(),
                    memsize = 4,
                    multicores = 2,
                    verbose = TRUE,
                    version = "test2"
                )
            )
        },
        error = function(e) {
            return(NULL)
        }
    )

    expect_true(all(file.exists(unlist(sinop_probs$file_info[[1]]$path))))
    r_obj <- .raster_open_rast(sinop_probs$file_info[[1]]$path[[1]])
    expect_true(.raster_nrows(r_obj) == .tile_nrows(sinop_probs))

    max_lyr2 <- max(.raster_get_values(r_obj)[, 2], na.rm = TRUE)
    expect_true(max_lyr2 <= 10000)

    max_lyr3 <- max(.raster_get_values(r_obj)[, 3], na.rm = TRUE)
    expect_true(max_lyr3 <= 10000)

    expect_true(all(file.remove(unlist(sinop_probs$file_info[[1]]$path))))
})

test_that("One-year, single core classification with filter", {

    samples_filt <- sits_filter(samples_modis_ndvi, filter = sits_whittaker())

    svm_model <- sits_train(samples_filt, sits_svm())

    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    sinop <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6",
        data_dir = data_dir,
        delim = "_",
        bands = "NDVI",
        parse_info = c("X1", "tile", "band", "date")
    )

    sinop_probs <- suppressMessages(
        sits_classify(
            data = sinop,
            ml_model = svm_model,
            filter_fn = sits_whittaker(),
            output_dir = tempdir(),
            memsize = 4,
            multicores = 1,
            version = "test3"
        )
    )
    r_obj <- .raster_open_rast(sinop_probs$file_info[[1]]$path[[1]])

    expect_true(.raster_nrows(r_obj) == .tile_nrows(sinop_probs))

    max_lyr2 <- max(.raster_get_values(r_obj)[, 2], na.rm = TRUE)
    expect_true(max_lyr2 <= 10000)

    max_lyr3 <- max(.raster_get_values(r_obj)[, 3], na.rm = TRUE)
    expect_true(max_lyr3 <= 10000)
    expect_true(all(file.remove(unlist(sinop_probs$file_info[[1]]$path))))
})

test_that("One-year, multicore classification with Savitzky-Golay filter", {

    samples_filt <- sits_apply(samples_modis_ndvi, NDVI = sits_sgolay(NDVI))

    rfor_model <- sits_train(samples_filt, sits_rfor())

    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    sinop <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6",
        data_dir = data_dir,
        delim = "_",
        parse_info = c("X1", "tile", "band", "date")
    )

    sinop_2014_probs <- tryCatch(
        {
            suppressMessages(
                sits_classify(
                    data = sinop,
                    ml_model = rfor_model,
                    filter = sits_sgolay(),
                    output_dir = tempdir(),
                    memsize = 4,
                    multicores = 2,
                    version = "test4"
                )
            )
        },
        error = function(e) {
            return(NULL)
        }
    )

    if (purrr::is_null(sinop_2014_probs)) {
        skip("Unable to allocate multicores")
    }
    expect_true(all(file.exists(unlist(sinop_2014_probs$file_info[[1]]$path))))

    r_obj <- .raster_open_rast(sinop_2014_probs$file_info[[1]]$path[[1]])

    expect_true(.raster_nrows(r_obj) == .tile_nrows(sinop_2014_probs))

    max_lyr2 <- max(.raster_get_values(r_obj)[, 2], na.rm = TRUE)
    expect_true(max_lyr2 <= 10000)

    max_lyr3 <- max(.raster_get_values(r_obj)[, 3], na.rm = TRUE)
    expect_true(max_lyr3 <= 10000)

    expect_true(all(file.remove(unlist(sinop_2014_probs$file_info[[1]]$path))))
})

test_that("One-year, multicore classification with Whittaker filter", {

    samples_filt <- sits_apply(samples_modis_ndvi,
                               NDVI = sits_whittaker(NDVI, lambda = 0.5))

    xgb_model <- sits_train(samples_filt, sits_xgboost())

    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    sinop <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6",
        data_dir = data_dir,
        delim = "_",
        parse_info = c("X1", "tile", "band", "date")
    )

    sinop_2014_probs <- tryCatch(
        {
            suppressMessages(
                sits_classify(
                    data = sinop,
                    ml_model = xgb_model,
                    filter = sits_whittaker(lambda = 3.0),
                    output_dir = tempdir(),
                    memsize = 4,
                    multicores = 2,
                    version = "test5"
                )
            )
        },
        error = function(e) {
            return(NULL)
        }
    )

    if (purrr::is_null(sinop_2014_probs)) {
        skip("Unable to allocated multicores")
    }
    expect_true(all(file.exists(unlist(sinop_2014_probs$file_info[[1]]$path))))

    r_obj <- .raster_open_rast(sinop_2014_probs$file_info[[1]]$path[[1]])

    expect_true(.raster_nrows(r_obj) == .tile_nrows(sinop_2014_probs))

    max_lyr2 <- max(.raster_get_values(r_obj)[, 2])
    expect_true(max_lyr2 <= 10000)

    max_lyr3 <- max(.raster_get_values(r_obj)[, 3])
    expect_true(max_lyr3 <= 10000)

    expect_true(all(file.remove(unlist(sinop_2014_probs$file_info[[1]]$path))))
})

test_that("One-year, multicore classification with MLP", {

    torch_model <- sits_train(samples_modis_ndvi, sits_mlp(epochs = 20))

    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    sinop <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6",
        data_dir = data_dir,
        delim = "_",
        parse_info = c("X1", "tile", "band", "date")
    )

    sinop_2014_probs <- tryCatch(
        {
            suppressMessages(
                sits_classify(
                    data = sinop,
                    ml_model = torch_model,
                    output_dir = tempdir(),
                    memsize = 8,
                    multicores = 2,
                    version = "test6"
                )
            )
        },
        error = function(e) {
            return(NULL)
        }
    )

    if (purrr::is_null(sinop_2014_probs)) {
        skip("Unable to allocate multicores")
    }
    expect_true(all(file.exists(unlist(sinop_2014_probs$file_info[[1]]$path))))

    r_obj <- .raster_open_rast(sinop_2014_probs$file_info[[1]]$path[[1]])

    expect_true(.raster_nrows(r_obj) == .tile_nrows(sinop_2014_probs))

    max_lyr2 <- max(.raster_get_values(r_obj)[, 2], na.rm = TRUE)
    expect_true(max_lyr2 <= 10000)

    max_lyr3 <- max(.raster_get_values(r_obj)[, 3], na.rm = TRUE)
    expect_true(max_lyr3 <= 10000)

    expect_true(all(file.remove(unlist(sinop_2014_probs$file_info[[1]]$path))))
})

test_that("One-year, multicore classification with TempCNN", {

    torch_model <- sits_train(samples_modis_ndvi, sits_tempcnn(epochs = 20))

    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    sinop <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6",
        data_dir = data_dir,
        delim = "_",
        parse_info = c("X1", "tile", "band", "date")
    )

    sinop_2014_probs <- tryCatch(
        {
            suppressMessages(
                sits_classify(
                    data = sinop,
                    ml_model = torch_model,
                    output_dir = tempdir(),
                    memsize = 8,
                    multicores = 2,
                    version = "test7"
                )
            )
        },
        error = function(e) {
            return(NULL)
        }
    )

    if (purrr::is_null(sinop_2014_probs)) {
        skip("Unable to allocate multicores")
    }
    expect_true(all(file.exists(unlist(sinop_2014_probs$file_info[[1]]$path))))

    r_obj <- .raster_open_rast(sinop_2014_probs$file_info[[1]]$path[[1]])

    expect_true(.raster_nrows(r_obj) == .tile_nrows(sinop_2014_probs))

    max_lyr2 <- max(.raster_get_values(r_obj)[, 2], na.rm = TRUE)
    expect_true(max_lyr2 <= 10000)

    max_lyr3 <- max(.raster_get_values(r_obj)[, 3], na.rm = TRUE)
    expect_true(max_lyr3 <= 10000)

    expect_true(all(file.remove(unlist(sinop_2014_probs$file_info[[1]]$path))))
})

test_that("One-year, multicore classification with ResNet", {

    torch_model <- sits_train(samples_modis_ndvi, sits_resnet(epochs = 20))

    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    sinop <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6",
        data_dir = data_dir,
        delim = "_",
        parse_info = c("X1", "tile", "band", "date")
    )

    sinop_2014_probs <- tryCatch(
        {
            suppressMessages(
                sits_classify(
                    data = sinop,
                    ml_model = torch_model,
                    output_dir = tempdir(),
                    memsize = 8,
                    multicores = 2,
                    version = "test8"
                )
            )
        },
        error = function(e) {
            return(NULL)
        }
    )

    if (purrr::is_null(sinop_2014_probs)) {
        skip("Unable to allocate multicores")
    }
    expect_true(all(file.exists(unlist(sinop_2014_probs$file_info[[1]]$path))))

    r_obj <- .raster_open_rast(sinop_2014_probs$file_info[[1]]$path[[1]])

    expect_true(.raster_nrows(r_obj) == .tile_nrows(sinop_2014_probs))

    max_lyr2 <- max(.raster_get_values(r_obj)[, 2], na.rm = TRUE)
    expect_true(max_lyr2 <= 10000)

    max_lyr3 <- max(.raster_get_values(r_obj)[, 3], na.rm = TRUE)
    expect_true(max_lyr3 <= 10000)

    expect_true(all(file.remove(unlist(sinop_2014_probs$file_info[[1]]$path))))
})

test_that("One-year, multicore classification with TAE", {

    torch_model <- sits_train(samples_modis_ndvi, sits_tae(epochs = 20))

    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    sinop <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6",
        data_dir = data_dir,
        delim = "_",
        parse_info = c("X1", "tile", "band", "date")
    )

    sinop_2014_probs <- tryCatch(
        {
            suppressMessages(
                sits_classify(
                    data = sinop,
                    ml_model = torch_model,
                    output_dir = tempdir(),
                    memsize = 8,
                    multicores = 2,
                    version = "test9"
                )
            )
        },
        error = function(e) {
            return(NULL)
        }
    )

    if (purrr::is_null(sinop_2014_probs)) {
        skip("Unable to allocate multicores")
    }
    expect_true(all(file.exists(unlist(sinop_2014_probs$file_info[[1]]$path))))

    r_obj <- .raster_open_rast(sinop_2014_probs$file_info[[1]]$path[[1]])

    expect_true(.raster_nrows(r_obj) == .tile_nrows(sinop_2014_probs))

    max_lyr2 <- max(.raster_get_values(r_obj)[, 2])
    expect_true(max_lyr2 <= 10000)

    max_lyr3 <- max(.raster_get_values(r_obj)[, 3])
    expect_true(max_lyr3 <= 10000)

    expect_true(all(file.remove(unlist(sinop_2014_probs$file_info[[1]]$path))))
})

test_that("One-year, multicore classification with LightTAE", {

    torch_model <- sits_train(samples_modis_ndvi, sits_lighttae(epochs = 20))

    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    sinop <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6",
        data_dir = data_dir,
        delim = "_",
        parse_info = c("X1", "tile", "band", "date")
    )

    sinop_2014_probs <- tryCatch(
        {
            suppressMessages(
                sits_classify(
                    data = sinop,
                    ml_model = torch_model,
                    output_dir = tempdir(),
                    memsize = 8,
                    multicores = 2,
                    version = "test10"
                )
            )
        },
        error = function(e) {
            return(NULL)
        }
    )

    if (purrr::is_null(sinop_2014_probs)) {
        skip("Unable to allocated multicores")
    }
    expect_true(all(file.exists(unlist(sinop_2014_probs$file_info[[1]]$path))))

    r_obj <- .raster_open_rast(sinop_2014_probs$file_info[[1]]$path[[1]])

    expect_true(.raster_nrows(r_obj) == .tile_nrows(sinop_2014_probs))

    max_lyr2 <- max(.raster_get_values(r_obj)[, 2])
    expect_true(max_lyr2 <= 10000)

    max_lyr3 <- max(.raster_get_values(r_obj)[, 3])
    expect_true(max_lyr3 <= 10000)

    expect_true(all(file.remove(unlist(sinop_2014_probs$file_info[[1]]$path))))
})

test_that("One-year, multicores classification with cloud band", {

    csv_file <- system.file("extdata/samples/samples_sinop_crop.csv",
                            package = "sits")
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6",
        data_dir = data_dir,
        delim = "_",
        parse_info = c("X1", "tile", "band", "date")
    )

    cloud_cube <- sits_apply(
        data = cube,
        output_dir = tempdir(),
        CLOUD = ifelse(NDVI <= 0.2, 0.0002, 0.0001),
        memsize = 4,
        multicores = 2
    )

    kern_cube <- sits_apply(
        data = cube,
        output_dir = tempdir(),
        NDVI_TEXTURE = w_sd(NDVI),
        window_size = 3,
        memsize = 4,
        multicores = 2
    )

    cube_merged <- sits_merge(data1 = cloud_cube, data2 = kern_cube)

    samples_ndvi <- sits_get_data(
        cube = cube_merged,
        samples = csv_file,
        multicores = 1
    )

    rf_model <- sits_train(samples_ndvi, ml_method = sits_rfor)

    probs_cube <- tryCatch(
        {
            suppressMessages(
                sits_classify(
                    cube_merged,
                    rf_model,
                    output_dir = tempdir(),
                    memsize = 4,
                    multicores = 2,
                    version = "test11"
                )
            )
        },
        error = function(e) {
            return(NULL)
        }
    )

    if (purrr::is_null(probs_cube)) {
        skip("Unable to allocate multicores")
    }

    expect_true(all(file.exists(unlist(probs_cube$file_info[[1]]$path))))
})

test_that("One-year, multicore classification with post-processing", {

    rfor_model <- sits_train(samples_modis_ndvi, sits_rfor())

    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    sinop <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6",
        data_dir = data_dir
    )

    temp_dir <- tempdir()

    unlink(
        list.files(temp_dir,
                   pattern = ".*probs.*.tif$",
                   full.names = TRUE
        )
    )

    sinop_probs <- tryCatch(
        {
            suppressMessages(
                sits_classify(
                    sinop,
                    rfor_model,
                    output_dir = tempdir(),
                    memsize = 4,
                    multicores = 2,
                    version = "test12"
                )
            )
        },
        error = function(e) {
            return(NULL)
        }
    )

    if (purrr::is_null(sinop_probs)) {
        skip("Unable to allocate multicores")
    }
    expect_true(all(file.exists(unlist(sinop_probs$file_info[[1]]$path))))

    sinop_class <- sits_label_classification(
        sinop_probs,
        output_dir = tempdir(),
        version = "test12"
    )

    # testing resume feature
    expect_message(
        object = {sits_label_classification(
            sinop_probs,
            output_dir = tempdir(),
            version = "test12"
        ) },
        regexp = "Recovery"
    )
    expect_true(all(file.exists(unlist(sinop_class$file_info[[1]]$path))))

    expect_true(length(sits_timeline(sinop_class)) ==
                    length(sits_timeline(sinop_probs)))

    r_obj <- .raster_open_rast(sinop_class$file_info[[1]]$path[[1]])
    max_lab <- max(.raster_get_values(r_obj))
    min_lab <- min(.raster_get_values(r_obj))
    expect_true(max_lab <= 9)
    expect_true(min_lab >= 1)


    sinop_bayes <- sits_smooth(
        sinop_probs,
        output_dir = tempdir(),
        multicores = 2,
        memsize = 4,
        version = "test12"
    )

    # testing the recovery feature
    expect_message(
        object = {sits_smooth(
            sinop_probs,
            output_dir = tempdir(),
            multicores = 2,
            memsize = 4,
            version = "test12"
        ) },
        regexp = "Recovery"
    )
    expect_true(all(file.exists(unlist(sinop_bayes$file_info[[1]]$path))))

    expect_true(length(sits_timeline(sinop_bayes)) ==
                    length(sits_timeline(sinop_probs)))

    r_bay <- .raster_open_rast(sinop_bayes$file_info[[1]]$path[[1]])
    expect_true(.raster_nrows(r_bay) == .tile_nrows(sinop_probs))

    max_bay2 <- max(.raster_get_values(r_bay)[, 2])
    expect_true(max_bay2 <= 10000)

    max_bay3 <- max(.raster_get_values(r_bay)[, 3])
    expect_true(max_bay3 <= 10000)

    sinop_bayes_2 <- sits_smooth(
        sinop_probs,
        output_dir = tempdir(),
        window_size = 9,
        neigh_fraction = 1.0,
        multicores = 2,
        memsize = 4,
        version = "test_v2"
    )
    r_bay_2 <- .raster_open_rast(sinop_bayes_2$file_info[[1]]$path[[1]])
    expect_true(.raster_nrows(r_bay_2) == .tile_nrows(sinop_probs))

    max_bay2 <- max(.raster_get_values(r_bay_2)[, 2])
    expect_true(max_bay2 <= 10000)

    max_bay3 <- max(.raster_get_values(r_bay_2)[, 3])
    expect_true(max_bay3 <= 10000)

    sinop_uncert <- sits_uncertainty(
        cube = sinop_bayes,
        type = "margin",
        output_dir = tempdir(),
        multicores = 1,
        memsize = 4,
        version = "test12"
    )

    expect_true(all(file.exists(unlist(sinop_uncert$file_info[[1]]$path))))
    r_unc <- .raster_open_rast(sinop_uncert$file_info[[1]]$path[[1]])
    expect_true(.raster_nrows(r_unc) == .tile_nrows(sinop_probs))

    max_unc <- max(.raster_get_values(r_unc))
    expect_true(max_unc <= 10000)

    sinop_uncert_2 <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6",
        bands = "margin",
        version = "test12",
        labels = sits_labels(sinop_class),
        data_dir = temp_dir,
        parse_info = c("X1", "X2", "tile",
                       "start_date", "end_date",
                       "band", "version")
    )

    timeline_orig <- sits_timeline(sinop)
    timeline_probs <- sits_timeline(sinop_probs)
    timeline_unc <- sits_timeline(sinop_uncert)
    timeline_class <- sits_timeline(sinop_class)
    timeline_model <- sits_timeline(rfor_model)
    timeline_ts <- sits_timeline(samples_modis_ndvi)

    expect_equal(timeline_ts, timeline_model)
    expect_equal(timeline_ts, timeline_orig)
    expect_equal(timeline_probs, timeline_unc)
    expect_equal(timeline_probs, timeline_class)
    expect_equal(timeline_orig[1], timeline_class[1])
    expect_equal(timeline_orig[length(timeline_orig)], timeline_class[2])

    expect_true(all(file.remove(unlist(sinop_class$file_info[[1]]$path))))
    expect_true(all(file.remove(unlist(sinop_bayes$file_info[[1]]$path))))
    expect_true(all(file.remove(unlist(sinop_bayes_2$file_info[[1]]$path))))

    expect_true(all(file.remove(unlist(sinop_probs$file_info[[1]]$path))))
    expect_true(all(file.remove(unlist(sinop_uncert$file_info[[1]]$path))))
})

test_that("One-year, multicores processing reclassify", {
    # Open mask map
    data_dir <- system.file("extdata/raster/prodes", package = "sits")
    prodes2021 <- sits_cube(
        source = "USGS",
        collection = "LANDSAT-C2L2-SR",
        data_dir = data_dir,
        parse_info = c("X1", "X2", "tile", "start_date", "end_date",
                       "band", "version"),
        bands = "class",
        version = "v20220606",
        labels = c("Forest", "Water", "NonForest",
                   "NonForest2", "NoClass", "d2007", "d2008",
                   "d2009", "d2010", "d2011", "d2012",
                   "d2013", "d2014", "d2015", "d2016",
                   "d2017", "d2018", "r2010", "r2011",
                   "r2012", "r2013", "r2014", "r2015",
                   "r2016", "r2017", "r2018", "d2019",
                   "r2019", "d2020", "NoClass", "r2020",
                   "Clouds2021", "d2021", "r2021")
    )
    # Open classification map
    data_dir <- system.file("extdata/raster/classif", package = "sits")
    ro_class <- sits_cube(
        source = "MPC",
        collection = "SENTINEL-2-L2A",
        data_dir = data_dir,
        parse_info = c("X1", "X2", "tile", "start_date", "end_date",
                       "band", "version"),
        bands = "class",
        labels = c("ClearCut_Fire", "ClearCut_BareSoil",
                   "ClearCut_Veg", "Forest")
    )
    # Reclassify cube
    ro_mask <- sits_reclassify(
        cube = ro_class,
        mask = prodes2021,
        rules = list(
            "Old_Deforestation" = mask %in% c(
                "d2007", "d2008", "d2009",
                "d2010", "d2011", "d2012",
                "d2013", "d2014", "d2015",
                "d2016", "d2017", "d2018",
                "r2010", "r2011", "r2012",
                "r2013", "r2014", "r2015",
                "r2016", "r2017", "r2018",
                "d2019", "r2019", "d2020",
                "r2020", "r2021"
            ),
            "Water_Mask" = mask == "Water",
            "NonForest_Mask" = mask %in% c("NonForest", "NonForest2")
        ),
        memsize = 4,
        multicores = 2,
        output_dir = tempdir()
    )

    expect_equal(
        sits_labels(ro_mask),
        c("ClearCut_Fire", "ClearCut_BareSoil",
          "ClearCut_Veg", "Forest", "Old_Deforestation",
          "Water_Mask", "NonForest_Mask")
    )

    unlink(ro_mask$file_info[[1]]$path)
})

test_that("One-year, multicores mosaic", {
    # create a random forest model
    rfor_model <- sits_train(samples_modis_ndvi, sits_rfor())
    # create a data cube from local files
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6",
        data_dir = data_dir,
        delim = "_",
        parse_info = c("X1", "tile", "band", "date")
    )
    output_dir <- tempdir()
    # classify a data cube
    probs_cube <- sits_classify(
        data = cube,
        ml_model = rfor_model,
        output_dir = output_dir
    )
    # smooth the probability cube using Bayesian statistics
    bayes_cube <- sits_smooth(probs_cube, output_dir = output_dir)
    # label the probability cube
    label_cube <- sits_label_classification(bayes_cube, output_dir = output_dir)
    # create roi
    roi <- sf::st_sfc(
        sf::st_polygon(
            list(rbind(
                c(-55.64768, -11.68649),
                c(-55.69654, -11.66455),
                c(-55.62973, -11.61519),
                c(-55.64768, -11.68649)))), crs = 4326
    )
    # crop and reproject original cube
    suppressWarnings(mosaic_cube <- sits_mosaic(
        cube = cube,
        roi = roi,
        output_dir = output_dir,
        version = "v1",
        multicores = 1
    ))
    expect_equal(mosaic_cube[["tile"]], "MOSAIC")
    expect_equal(nrow(mosaic_cube), 1)
    bbox_mos <- sits_bbox(mosaic_cube, as_crs = 4326)
    bbox_roi <- sf::st_bbox(roi)
    expect_true(
        bbox_mos[["xmin"]] < bbox_roi[["xmin"]] &&
            bbox_mos[["xmax"]] > bbox_roi[["xmax"]] &&
            bbox_mos[["ymin"]] < bbox_roi[["ymin"]] &&
            bbox_mos[["ymax"]] > bbox_roi[["ymax"]]
    )

    # crop and reproject classified image
    suppressWarnings(mosaic_class <- sits_mosaic(
        cube = label_cube,
        roi = roi,
        crs = 4326,
        output_dir = output_dir,
        version = "v1",
        multicores = 1
    ))

    expect_equal(mosaic_class[["tile"]], "MOSAIC")
    expect_equal(nrow(mosaic_class), 1)
    bbox_cube <- sits_bbox(mosaic_class)
    bbox_roi <- sf::st_bbox(roi)
    expect_true(
        bbox_cube[["xmin"]] < bbox_roi[["xmin"]] &&
        bbox_cube[["xmax"]] > bbox_roi[["xmax"]] &&
        bbox_cube[["ymin"]] < bbox_roi[["ymin"]] &&
        bbox_cube[["ymax"]] > bbox_roi[["ymax"]]
    )

    # resume feature
    mosaic_class <- sits_mosaic(
        cube = label_cube,
        roi = roi,
        crs = 4326,
        output_dir = output_dir,
        version = "v1"
    )
    expect_equal(mosaic_class[["tile"]], "MOSAIC")

    # create new roi
    roi2 <- sf::st_sfc(
        sf::st_polygon(
            list(rbind(
                c(-55.91563676, -11.92443997),
                c(-55.02414662, -11.92443997),
                c(-55.02414662, -11.38658587),
                c(-55.91563676, -11.38658587),
                c(-55.91563676, -11.92443997)))), crs = 4326
    )

    # reproject classified image
    mosaic_class2 <- sits_mosaic(
        cube = label_cube,
        roi = roi2,
        crs = 4326,
        output_dir = output_dir,
        version = "v2"
    )

    expect_equal(mosaic_class2[["tile"]], "MOSAIC")
    expect_equal(nrow(mosaic_class2), 1)
    bbox_cube <- sits_bbox(mosaic_class2)
    bbox_roi <- sf::st_bbox(roi2)
    expect_true(
        bbox_cube[["xmin"]] > bbox_roi[["xmin"]] &&
            bbox_cube[["xmax"]] < bbox_roi[["xmax"]] &&
            bbox_cube[["ymin"]] > bbox_roi[["ymin"]] &&
            bbox_cube[["ymax"]] < bbox_roi[["ymax"]]
    )

    uncert_cube <- sits_uncertainty(probs_cube, output_dir = output_dir)
    mosaic_uncert <- sits_mosaic(
        cube = uncert_cube,
        roi = roi,
        crs = 4326,
        output_dir = output_dir,
        version = "v3"
    )

    expect_equal(mosaic_uncert[["tile"]], "MOSAIC")
    expect_equal(nrow(mosaic_uncert), 1)
    bbox_cube <- sits_bbox(mosaic_uncert)
    bbox_roi <- sf::st_bbox(roi)
    expect_true(
        bbox_cube[["xmin"]] < bbox_roi[["xmin"]] &&
            bbox_cube[["xmax"]] > bbox_roi[["xmax"]] &&
            bbox_cube[["ymin"]] < bbox_roi[["ymin"]] &&
            bbox_cube[["ymax"]] > bbox_roi[["ymax"]]
    )

    unlink(probs_cube$file_info[[1]]$path)
    unlink(bayes_cube$file_info[[1]]$path)
    unlink(label_cube$file_info[[1]]$path)
    unlink(mosaic_cube$file_info[[1]]$path)
    unlink(mosaic_class$file_info[[1]]$path)
    unlink(mosaic_class2$file_info[[1]]$path)
    unlink(mosaic_uncert$file_info[[1]]$path)
    unlink(uncert_cube$file_info[[1]]$path)
})
test_that("Kernel functions", {
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6",
        data_dir = data_dir,
        delim = "_",
        parse_info = c("X1", "tile", "band", "date")
    )

    cube_median <- sits_apply(
        data = cube,
        output_dir = tempdir(),
        NDVI_MEDIAN = w_median(NDVI),
        window_size = 3,
        memsize = 4,
        multicores = 1
    )
    r_obj <- .raster_open_rast(cube$file_info[[1]]$path[[1]])
    v_obj <- matrix(.raster_get_values(r_obj), ncol = 254)
    r_obj_md <- .raster_open_rast(cube_median$file_info[[1]]$path[[2]])
    v_obj_md <- matrix(.raster_get_values(r_obj_md), ncol = 254)

    median_1 <- median(as.vector(v_obj[4:6,4:6]))
    median_2 <- v_obj_md[5,5]

    expect_false(median_1 == median_2)

    cube_mean <- sits_apply(
        data = cube,
        output_dir = tempdir(),
        NDVI_MEAN = w_mean(NDVI),
        window_size = 3,
        memsize = 4,
        multicores = 2
    )
    r_obj <- .raster_open_rast(cube[1,]$file_info[[1]]$path[[1]])
    v_obj <- matrix(.raster_get_values(r_obj), nrow = 144)
    r_obj_m <- .raster_open_rast(cube_mean$file_info[[1]]$path[[2]])
    v_obj_m <- matrix(.raster_get_values(r_obj_m), nrow = 144)

    mean_1 <- as.integer(mean(as.vector(v_obj[4:6,4:6])))
    mean_2 <- v_obj_m[5,5]
    expect_false(mean_1 == mean_2)
})

test_that("Raster GDAL datatypes", {
    gdal_type <- .raster_gdal_datatype("INT2U")
    expect_equal(gdal_type, "UInt16")
})
