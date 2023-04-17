test_that("Single core classification with rfor", {
    rfor_model <- sits_train(
        samples_modis_ndvi,
        sits_rfor(num_trees = 30)
    )
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    sinop <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6",
        data_dir = data_dir
    )
    output_dir <- paste0(tempdir(), "/single_rfor")
    if (!dir.exists(output_dir)) {
        dir.create(output_dir)
    }
    sinop_probs <- sits_classify(
        data = sinop,
        ml_model = rfor_model,
        output_dir = output_dir,
        memsize = 4,
        multicores = 1
    )

    # testing resume feature
    out <- capture_messages({
        expect_message(
        object = { sits_classify(
            data = sinop,
            ml_model = rfor_model,
            output_dir = output_dir,
            memsize = 4,
            multicores = 2
        ) },
        regexp = "Recovery: "
        )
    })
    expect_true(grepl("output_dir", out[1]))

    sits_labels(sinop_probs) <- c(
        "Cerrado", "Floresta",
        "Pastagem", "Soja_Milho"
    )
    expect_true(all(sits_labels(sinop_probs) %in%
                        c("Cerrado", "Floresta", "Pastagem", "Soja_Milho")))
    expect_true(all(file.exists(unlist(sinop_probs$file_info[[1]]$path))))
    r_obj <- .raster_open_rast(sinop_probs$file_info[[1]]$path[[1]])

    expect_true(.raster_nrows(r_obj) == .tile_nrows(sinop_probs))

    max_lyr1 <- max(.raster_get_values(r_obj)[, 1])
    expect_true(max_lyr1 <= 10000)

    max_lyr3 <- max(.raster_get_values(r_obj)[, 3])
    expect_true(max_lyr3 <= 10000)

    expect_true(all(file.remove(unlist(sinop_probs$file_info[[1]]$path))))
})

test_that("Classification with SVM", {

    svm_model <- sits_train(samples_modis_ndvi, sits_svm())

    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    sinop <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6",
        data_dir = data_dir
    )
    output_dir <- paste0(tempdir(), "/svm")
    if (!dir.exists(output_dir)) {
        dir.create(output_dir)
    }

    sinop_probs <- sits_classify(
        data = sinop,
        ml_model = svm_model,
        output_dir = output_dir,
        memsize = 4,
        multicores = 2
    )
    expect_true(all(file.exists(unlist(sinop_probs$file_info[[1]]$path))))
    r_obj <- .raster_open_rast(sinop_probs$file_info[[1]]$path[[1]])
    expect_true(.raster_nrows(r_obj) == .tile_nrows(sinop_probs))

    max_lyr2 <- max(.raster_get_values(r_obj)[, 2])
    expect_true(max_lyr2 <= 10000)

    max_lyr3 <- max(.raster_get_values(r_obj)[, 3])
    expect_true(max_lyr3 <= 10000)

    expect_true(all(file.remove(unlist(sinop_probs$file_info[[1]]$path))))
})
test_that("Classification with XGBoost", {

    xgb_model <- sits_train(samples_modis_ndvi, sits_xgboost())

    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    sinop <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6",
        data_dir = data_dir
    )
    output_dir <- paste0(tempdir(), "/xgb")
    if (!dir.exists(output_dir)) {
        dir.create(output_dir)
    }

    sinop_probs <- sits_classify(
        data = sinop,
        ml_model = xgb_model,
        output_dir = output_dir,
        memsize = 4,
        multicores = 2
    )
    expect_true(all(file.exists(unlist(sinop_probs$file_info[[1]]$path))))
    r_obj <- .raster_open_rast(sinop_probs$file_info[[1]]$path[[1]])
    expect_true(.raster_nrows(r_obj) == .tile_nrows(sinop_probs))

    max_lyr2 <- max(.raster_get_values(r_obj)[, 2])
    expect_true(max_lyr2 <= 10000)

    max_lyr3 <- max(.raster_get_values(r_obj)[, 3])
    expect_true(max_lyr3 <= 10000)

    expect_true(all(file.remove(unlist(sinop_probs$file_info[[1]]$path))))
})

test_that("Classification with SVM and Whittaker filter", {

    samples_filt <- sits_filter(samples_modis_ndvi, filter = sits_whittaker())

    svm_model <- sits_train(samples_filt, sits_svm())

    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    sinop <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6",
        data_dir = data_dir
    )

    output_dir <- paste0(tempdir(), "/svm_whit")
    if (!dir.exists(output_dir)) {
        dir.create(output_dir)
    }
    sinop_probs <- sits_classify(
        data = sinop,
        ml_model = svm_model,
        filter_fn = sits_whittaker(),
        output_dir = output_dir,
        memsize = 4,
        multicores = 2
    )
    r_obj <- .raster_open_rast(sinop_probs$file_info[[1]]$path[[1]])

    expect_true(.raster_nrows(r_obj) == .tile_nrows(sinop_probs))

    max_lyr2 <- max(.raster_get_values(r_obj)[, 2])
    expect_true(max_lyr2 <= 10000)

    max_lyr3 <- max(.raster_get_values(r_obj)[, 3])
    expect_true(max_lyr3 <= 10000)
    expect_true(all(file.remove(unlist(sinop_probs$file_info[[1]]$path))))
})

test_that("Classification with RFOR and Savitzky-Golay filter", {

    samples_filt <- sits_apply(samples_modis_ndvi, NDVI = sits_sgolay(NDVI))

    rfor_model <- sits_train(samples_filt, sits_rfor())

    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    sinop <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6",
        data_dir = data_dir
    )
    output_dir <- paste0(tempdir(), "/rfor_sg")
    if (!dir.exists(output_dir)) {
        dir.create(output_dir)
    }
    sinop_2014_probs <- sits_classify(
        data = sinop,
        ml_model = rfor_model,
        filter_fn = sits_sgolay(),
        output_dir = output_dir,
        memsize = 4,
        multicores = 2
    )

    expect_true(all(file.exists(unlist(sinop_2014_probs$file_info[[1]]$path))))

    r_obj <- .raster_open_rast(sinop_2014_probs$file_info[[1]]$path[[1]])

    expect_true(.raster_nrows(r_obj) == .tile_nrows(sinop_2014_probs))

    max_lyr2 <- max(.raster_get_values(r_obj)[, 2])
    expect_true(max_lyr2 <= 10000)

    max_lyr3 <- max(.raster_get_values(r_obj)[, 3])
    expect_true(max_lyr3 <= 10000)

    expect_true(all(file.remove(unlist(sinop_2014_probs$file_info[[1]]$path))))
})

test_that("Classification with MLP", {

    torch_model <- sits_train(samples_modis_ndvi, sits_mlp(epochs = 20))

    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    sinop <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6",
        data_dir = data_dir
    )
    output_dir <- paste0(tempdir(), "/mlp")
    if (!dir.exists(output_dir)) {
        dir.create(output_dir)
    }

    sinop_2014_probs <- sits_classify(
        data = sinop,
        ml_model = torch_model,
        output_dir = output_dir,
        memsize = 8,
        multicores = 2
    )
    expect_true(all(file.exists(unlist(sinop_2014_probs$file_info[[1]]$path))))

    r_obj <- .raster_open_rast(sinop_2014_probs$file_info[[1]]$path[[1]])

    expect_true(.raster_nrows(r_obj) == .tile_nrows(sinop_2014_probs))

    max_lyr2 <- max(.raster_get_values(r_obj)[, 2])
    expect_true(max_lyr2 <= 10000)

    max_lyr3 <- max(.raster_get_values(r_obj)[, 3])
    expect_true(max_lyr3 <= 10000)

    expect_true(all(file.remove(unlist(sinop_2014_probs$file_info[[1]]$path))))
})

test_that("Classification with TempCNN", {

    torch_model <- sits_train(samples_modis_ndvi, sits_tempcnn(epochs = 20))

    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    sinop <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6",
        data_dir = data_dir
    )
    output_dir <- paste0(tempdir(), "/tcnn")
    if (!dir.exists(output_dir)) {
        dir.create(output_dir)
    }

    sinop_2014_probs <- sits_classify(
        data = sinop,
        ml_model = torch_model,
        output_dir = output_dir,
        memsize = 8,
        multicores = 2
    )
    expect_true(all(file.exists(unlist(sinop_2014_probs$file_info[[1]]$path))))

    r_obj <- .raster_open_rast(sinop_2014_probs$file_info[[1]]$path[[1]])

    expect_true(.raster_nrows(r_obj) == .tile_nrows(sinop_2014_probs))

    max_lyr2 <- max(.raster_get_values(r_obj)[, 2])
    expect_true(max_lyr2 <= 10000)

    max_lyr3 <- max(.raster_get_values(r_obj)[, 3])
    expect_true(max_lyr3 <= 10000)

    expect_true(all(file.remove(unlist(sinop_2014_probs$file_info[[1]]$path))))
})

test_that("Classification with ResNet", {

    torch_model <- sits_train(samples_modis_ndvi, sits_resnet(epochs = 20))

    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    sinop <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6",
        data_dir = data_dir
    )
    output_dir <- paste0(tempdir(), "/rnet")
    if (!dir.exists(output_dir)) {
        dir.create(output_dir)
    }
    sinop_2014_probs <- sits_classify(
        data = sinop,
        ml_model = torch_model,
        output_dir = output_dir,
        memsize = 8,
        multicores = 2
    )
    expect_true(all(file.exists(unlist(sinop_2014_probs$file_info[[1]]$path))))

    r_obj <- .raster_open_rast(sinop_2014_probs$file_info[[1]]$path[[1]])

    expect_true(.raster_nrows(r_obj) == .tile_nrows(sinop_2014_probs))

    max_lyr2 <- max(.raster_get_values(r_obj)[, 2])
    expect_true(max_lyr2 <= 10000)

    max_lyr3 <- max(.raster_get_values(r_obj)[, 3])
    expect_true(max_lyr3 <= 10000)

    expect_true(all(file.remove(unlist(sinop_2014_probs$file_info[[1]]$path))))
})

test_that("Classification with TAE", {

    torch_model <- sits_train(samples_modis_ndvi, sits_tae(epochs = 20))

    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    sinop <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6",
        data_dir = data_dir
    )
    output_dir <- paste0(tempdir(), "/tae")
    if (!dir.exists(output_dir)) {
        dir.create(output_dir)
    }
    sinop_2014_probs <-  sits_classify(
        data = sinop,
        ml_model = torch_model,
        output_dir = output_dir,
        memsize = 8,
        multicores = 2
    )
    expect_true(all(file.exists(unlist(sinop_2014_probs$file_info[[1]]$path))))

    r_obj <- .raster_open_rast(sinop_2014_probs$file_info[[1]]$path[[1]])

    expect_true(.raster_nrows(r_obj) == .tile_nrows(sinop_2014_probs))

    max_lyr2 <- max(.raster_get_values(r_obj)[, 2])
    expect_true(max_lyr2 <= 10000)

    max_lyr3 <- max(.raster_get_values(r_obj)[, 3])
    expect_true(max_lyr3 <= 10000)

    expect_true(all(file.remove(unlist(sinop_2014_probs$file_info[[1]]$path))))
})

test_that("Classification with LightTAE", {

    torch_model <- sits_train(samples_modis_ndvi, sits_lighttae(epochs = 20))

    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    sinop <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6",
        data_dir = data_dir
    )
    output_dir <- paste0(tempdir(), "/ltae")
    if (!dir.exists(output_dir)) {
        dir.create(output_dir)
    }

    sinop_2014_probs <- sits_classify(
        data = sinop,
        ml_model = torch_model,
        output_dir = output_dir,
        memsize = 8,
        multicores = 2
    )
    expect_true(all(file.exists(unlist(sinop_2014_probs$file_info[[1]]$path))))

    r_obj <- .raster_open_rast(sinop_2014_probs$file_info[[1]]$path[[1]])

    expect_true(.raster_nrows(r_obj) == .tile_nrows(sinop_2014_probs))

    max_lyr2 <- max(.raster_get_values(r_obj)[, 2])
    expect_true(max_lyr2 <= 10000)

    max_lyr3 <- max(.raster_get_values(r_obj)[, 3])
    expect_true(max_lyr3 <= 10000)

    expect_true(all(file.remove(unlist(sinop_2014_probs$file_info[[1]]$path))))
})

test_that("Classification with cloud band", {

    csv_file <- system.file("extdata/samples/samples_sinop_crop.csv",
                            package = "sits")
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6",
        data_dir = data_dir
    )
    output_dir <- paste0(tempdir(), "/cloud")
    if (!dir.exists(output_dir)) {
        dir.create(output_dir)
    }
    cloud_cube <- sits_apply(
        data = cube,
        output_dir = output_dir,
        CLOUD = ifelse(NDVI <= 0.2, 0.0002, 0.0001),
        memsize = 4,
        multicores = 2
    )

    kern_cube <- sits_apply(
        data = cube,
        output_dir = output_dir,
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

    sinop_2014_probs <- sits_classify(
        data = cube_merged,
        ml_model = rf_model,
        output_dir = output_dir,
        memsize = 4,
        multicores = 2
    )
    expect_true(all(file.exists(unlist(sinop_2014_probs$file_info[[1]]$path))))

    r_obj <- .raster_open_rast(sinop_2014_probs$file_info[[1]]$path[[1]])

    expect_true(.raster_nrows(r_obj) == .tile_nrows(sinop_2014_probs))

    max_lyr2 <- max(.raster_get_values(r_obj)[, 2])
    expect_true(max_lyr2 <= 10000)

    max_lyr3 <- max(.raster_get_values(r_obj)[, 3])
    expect_true(max_lyr3 <= 10000)

    expect_true(all(file.remove(unlist(sinop_2014_probs$file_info[[1]]$path))))
})

test_that("Classification with post-processing", {

    rfor_model <- sits_train(samples_modis_ndvi, sits_rfor())

    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")

    sinop <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6",
        data_dir = data_dir
    )
    output_dir <- paste0(tempdir(), "/bayes")
    if (!dir.exists(output_dir)) {
        dir.create(output_dir)
    }

    sinop_probs <- sits_classify(
        data = sinop,
        ml_model = rfor_model,
        output_dir = output_dir,
        memsize = 4,
        multicores = 1
    )
    expect_true(all(file.exists(unlist(sinop_probs$file_info[[1]]$path))))
    sinop_class <- sits_label_classification(
        sinop_probs,
        output_dir = output_dir,
    )

    # testing resume feature
    out <- capture_messages({
        expect_message(
        object = {sits_label_classification(
            sinop_probs,
            output_dir = output_dir
        ) },
        regexp = "Recovery"
        )
    })
    expect_true(grepl("output_dir", out[1]))

    expect_true(all(file.exists(unlist(sinop_class$file_info[[1]]$path))))
    expect_true(length(sits_timeline(sinop_class)) ==
                    length(sits_timeline(sinop_probs)))

    r_obj <- .raster_open_rast(sinop_class$file_info[[1]]$path[[1]])
    max_lab <- max(.raster_get_values(r_obj))
    min_lab <- min(.raster_get_values(r_obj))
    expect_true(max_lab == 4)
    expect_true(min_lab == 1)


    sinop_bayes <- sits_smooth(
        sinop_probs,
        output_dir = output_dir,
        memsize = 4,
        multicores = 2
    )

    # testing the recovery feature
    out <- capture_messages({
        expect_message(
        object = {sits_smooth(
            sinop_probs,
            output_dir = output_dir,
            multicores = 2,
            memsize = 4
        ) },
        regexp = "Recovery"
        )
    })
    expect_true(grepl("output_dir", out[1]))

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
        output_dir = output_dir,
        window_size = 7,
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
        output_dir = output_dir,
        memsize = 4,
        multicores = 2
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
        version = "v1",
        labels = sits_labels(sinop_class),
        data_dir = output_dir,
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
test_that("Raster GDAL datatypes", {
    gdal_type <- .raster_gdal_datatype("INT2U")
    expect_equal(gdal_type, "UInt16")
})
