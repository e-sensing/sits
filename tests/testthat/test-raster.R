test_that("Single core classification with rfor", {
    rfor_model <- sits_train(
        samples_modis_ndvi,
        sits_rfor(num_trees = 30)
    )
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    sinop <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6",
        data_dir = data_dir,
        progress = TRUE,
        verbose = TRUE
    )
    expect_error(.check_bbox(sinop))

    output_dir <- paste0(tempdir(), "/single_rfor")
    if (!dir.exists(output_dir)) {
        dir.create(output_dir)
    }
    sinop_probs <- sits_classify(
        data = sinop,
        ml_model = rfor_model,
        output_dir = output_dir,
        memsize = 4,
        multicores = 1,
        progress = FALSE
    )
    bands_p <- sits_bands(sinop_probs)
    labels_p <- sits_labels(sinop_probs)
    expect_true(.check_cube_is_results_cube(bands_p, labels_p))

    # testing resume feature
    out <- capture_messages({
        expect_message(
            object = {
                sits_classify(
                    data = sinop,
                    ml_model = rfor_model,
                    output_dir = output_dir,
                    memsize = 4,
                    multicores = 2,
                    progress = TRUE
                )
            },
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

    # defaults and errors
    expect_error(sits_classify(probs_cube, rf_model))
    sinop_df <- sinop
    class(sinop_df) <- "data.frame"
    expect_error(sits_classify(sinop_df, rfor_model, output_dir = tempdir()))
    expect_true(all(file.remove(unlist(sinop_probs$file_info[[1]]$path))))
})

test_that("Classification with SVM", {
    svm_model <- sits_train(samples_modis_ndvi, sits_svm())

    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    sinop <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6",
        data_dir = data_dir,
        progress = FALSE
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
        multicores = 2,
        progress = FALSE
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
        data_dir = data_dir,
        progress = FALSE
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
        multicores = 2,
        progress = FALSE
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
        data_dir = data_dir,
        progress = FALSE
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
        multicores = 2,
        progress = FALSE
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
        data_dir = data_dir,
        progress = FALSE
    )
    output_dir <- paste0(tempdir(), "/rfor_sg")
    if (!dir.exists(output_dir)) {
        dir.create(output_dir)
    }
    start_date <- sits_timeline(sinop)[1]
    end_date <- sits_timeline(sinop)[length(sits_timeline(sinop))]
    sinop_2014_probs <- sits_classify(
        data = sinop,
        ml_model = rfor_model,
        filter_fn = sits_sgolay(),
        start_date = start_date,
        end_date = end_date,
        output_dir = output_dir,
        memsize = 4,
        multicores = 2,
        progress = TRUE,
        verbose = TRUE
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
        data_dir = data_dir,
        progress = FALSE
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
        multicores = 2,
        progress = FALSE
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
        data_dir = data_dir,
        progress = FALSE
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
        multicores = 2,
        progress = FALSE
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
        data_dir = data_dir,
        progress = FALSE
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
        multicores = 2,
        progress = FALSE
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
        data_dir = data_dir,
        progress = FALSE
    )
    output_dir <- paste0(tempdir(), "/tae")
    if (!dir.exists(output_dir)) {
        dir.create(output_dir)
    }
    sinop_2014_probs <- sits_classify(
        data = sinop,
        ml_model = torch_model,
        output_dir = output_dir,
        memsize = 8,
        multicores = 2,
        progress = FALSE
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
        data_dir = data_dir,
        progress = FALSE
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
        multicores = 2,
        progress = FALSE
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
        package = "sits"
    )
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6",
        data_dir = data_dir,
        progress = FALSE
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
        multicores = 1,
        progress = FALSE
    )

    rf_model <- sits_train(samples_ndvi, ml_method = sits_rfor)

    sinop_2014_probs <- sits_classify(
        data = cube_merged,
        ml_model = rf_model,
        output_dir = output_dir,
        memsize = 4,
        multicores = 2,
        progress = FALSE
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
        data_dir = data_dir,
        progress = FALSE
    )
    output_dir <- paste0(tempdir(), "/bayes")
    if (!dir.exists(output_dir)) {
        dir.create(output_dir)
    }

    sinop2c <- .cube_find_class(sinop)
    expect_true("raster_cube" %in% class(sinop2c))
    expect_true("eo_cube" %in% class(sinop2c))

    sinop2 <- sinop
    class(sinop2) <- "data.frame"
    new_cube <- .cube_find_class(sinop2)
    expect_true("raster_cube" %in% class(new_cube))
    expect_true("eo_cube" %in% class(new_cube))

    bands <- .cube_bands(sinop2)
    expect_equal(bands, "NDVI")

    path1 <- .tile_path(sinop2, date = "2013-09-14",
                        band = "NDVI")
    expect_true(grepl("jp2", path1))

    expect_equal(.tile_source(sinop2), "BDC")
    expect_equal(.tile_collection(sinop2), "MOD13Q1-6")
    expect_equal(.tile_satellite(sinop2), "TERRA")
    expect_equal(.tile_sensor(sinop2), "MODIS")
    expect_equal(.tile_bands(sinop2), "NDVI")
    expect_equal(.tile_ncols(sinop2), 255)
    expect_equal(.tile_nrows(sinop2), 147)
    expect_equal(.tile_size(sinop2)$ncols, 255)
    expect_equal(.tile_size(sinop2)$nrows, 147)
    expect_gt(.tile_xres(sinop2), 231)
    expect_gt(.tile_yres(sinop2), 231)
    expect_equal(as.Date(.tile_start_date(sinop2)), as.Date("2013-09-14"))
    expect_equal(as.Date(.tile_end_date(sinop2)), as.Date("2014-08-29"))
    expect_equal(.tile_fid(sinop), .tile_fid(sinop2))
    expect_equal(.tile_crs(sinop), .tile_crs(sinop2))
    expect_error(.tile_area_freq(sinop))
    expect_equal(.tile_timeline(sinop), .tile_timeline(sinop2))
    expect_true(.tile_is_complete(sinop2))
    band_conf <- .tile_band_conf(sinop2, band = "NDVI")
    expect_equal(band_conf$band_name, "NDVI")

    expect_error(.cube_find_class(samples_modis_ndvi))

    is_complete <- .cube_is_complete(sinop2)
    expect_true(is_complete)

    time_tb <- .cube_timeline_acquisition(sinop2, period = "P2M", origin = NULL)
    expect_equal(nrow(time_tb), 6)
    expect_equal(time_tb[[1,1]], as.Date("2013-09-14"))

    bbox <- .cube_bbox(sinop2)
    expect_equal(bbox[["xmin"]], -6073798)
    bbox2 <- .tile_bbox(sinop2)
    expect_equal(bbox2[["xmin"]], -6073798)

    sf_obj <- .cube_as_sf(sinop2)
    bbox3 <- sf::st_bbox(sf_obj)
    expect_equal(bbox[["xmin"]], bbox3[["xmin"]])

    sf_obj2 <- .tile_as_sf(sinop2)
    bbox4 <- sf::st_bbox(sf_obj2)
    expect_equal(bbox[["xmin"]], bbox4[["xmin"]])

    expect_true(.cube_during(sinop2, "2014-01-01", "2014-04-01"))
    expect_true(.tile_during(sinop2, "2014-01-01", "2014-04-01"))

    t <- .cube_filter_interval(sinop2, "2014-01-01", "2014-04-01")
    expect_equal(length(sits_timeline(t)), 3)

    t1 <- .tile_filter_interval(sinop2, "2014-01-01", "2014-04-01")
    expect_equal(length(sits_timeline(t1)), 3)

    timeline <- sits_timeline(sinop2)
    dates <- as.Date(c(timeline[1], timeline[3], timeline[5]))
    t2 <- .cube_filter_dates(sinop2, dates)
    expect_equal(.tile_timeline(t2), dates)

    paths <- .cube_paths(sinop2)[[1]]
    expect_equal(length(paths), 12)
    expect_true(grepl("jp2", paths[12]))

    expect_true(.cube_is_local(sinop2))

    cube <- .cube_split_features(sinop2)
    expect_equal(nrow(cube), 12)

    cube <- .cube_split_assets(sinop2)
    expect_equal(nrow(cube), 12)

    expect_false(.cube_contains_cloud(sinop2))

    sinop_probs <- sits_classify(
        data = sinop,
        ml_model = rfor_model,
        output_dir = output_dir,
        memsize = 4,
        multicores = 1,
        progress = FALSE
    )
    expect_true(all(file.exists(unlist(sinop_probs$file_info[[1]]$path))))

    sinop_class <- sits_label_classification(
        sinop_probs,
        output_dir = output_dir,
        progress = FALSE
    )
    # testing resume feature
    out <- capture_messages({
        expect_message(
            object = {
                sits_label_classification(
                    sinop_probs,
                    output_dir = output_dir,
                    progress = FALSE
                )
            },
            regexp = "Recovery"
        )
    })
    expect_error(sits_label_classification(
        sinop, output_dir = tempdir()))
    expect_error(sits_label_classification(
        sinop2, output_dir = tempdir()))

    expect_true(grepl("output_dir", out[1]))

    expect_true(all(file.exists(unlist(sinop_class$file_info[[1]]$path))))
    expect_true(length(sits_timeline(sinop_class)) ==
        length(sits_timeline(sinop_probs)))

    r_obj <- .raster_open_rast(sinop_class$file_info[[1]]$path[[1]])
    max_lab <- max(.raster_get_values(r_obj))
    min_lab <- min(.raster_get_values(r_obj))
    expect_true(max_lab == 4)
    expect_true(min_lab == 1)

    # test access for data.frame objects
    #
    sinop4 <- sinop_class
    class(sinop4) <- "data.frame"
    new_cube4 <- .cube_find_class(sinop4)
    expect_true("raster_cube" %in% class(new_cube4))
    expect_true("derived_cube" %in% class(new_cube4))
    expect_true("class_cube" %in% class(new_cube4))

    labels <- .cube_labels(sinop4)
    expect_true(all(c("Cerrado", "Forest", "Pasture","Soy_Corn") %in% labels))
    labels <- .tile_labels(sinop4)
    expect_true(all(c("Cerrado", "Forest", "Pasture","Soy_Corn") %in% labels))

    labels <- sits_labels(sinop4)
    expect_true(all(c("Cerrado", "Forest", "Pasture","Soy_Corn") %in% labels))

    sits_labels(sinop4) <- c("Cerrado", "Floresta", "Pastagem","Soja_Milho")
    labels <- sits_labels(sinop4)
    expect_true("Cerrado" %in% labels)

    expect_equal(.tile_area_freq(sinop_class)[1,3],.tile_area_freq(sinop4)[1,3])

    expect_error(.tile_update_label(
        sinop_probs,
        c("Cerrado", "Floresta", "Pastagem","Soja_Milho")
    ))

    class(sinop4) <- "data.frame"
    col <- .cube_collection(sinop4)
    expect_equal(col, "MOD13Q1-6")

    col <- .tile_collection(sinop4)
    expect_equal(col, "MOD13Q1-6")

    crs <- .cube_crs(sinop4)
    expect_true(grepl("Sinusoidal", crs))
    expect_true(grepl("Sinusoidal", .tile_crs(sinop4)))

    class <- .cube_s3class(sinop4)
    expect_true("raster_cube" %in% class)
    expect_true("derived_cube" %in% class)
    expect_true("class_cube" %in% class)

    expect_equal(.cube_ncols(sinop4), 255)
    expect_equal(.tile_ncols(sinop4), 255)
    expect_equal(.cube_nrows(sinop4), 147)
    expect_equal(.tile_nrows(sinop4), 147)
    expect_equal(.cube_source(sinop4), "BDC")
    expect_equal(.tile_source(sinop4), "BDC")
    expect_equal(.cube_collection(sinop4), "MOD13Q1-6")
    expect_equal(.tile_collection(sinop4), "MOD13Q1-6")

    sd <- .cube_start_date(sinop4)
    expect_equal(sd, as.Date("2013-09-14"))

    ed <- .cube_end_date(sinop4)
    expect_equal(ed, as.Date("2014-08-29"))

    timeline <- .cube_timeline(sinop4)[[1]]
    expect_equal(timeline[1], sd)
    expect_equal(timeline[2], ed)

    size <- .tile_size(sinop4)
    expect_equal(size$nrows, 147)
    expect_true(.tile_is_complete(sinop4))

    # Save QML file
    qml_file <- paste0(tempdir(),"/myfile.qml")
    sits_colors_qgis(sinop_class, qml_file)
    expect_true(file.size(qml_file) > 2000)

    sinop_bayes <- sits_smooth(
        sinop_probs,
        output_dir = output_dir,
        memsize = 4,
        multicores = 2
    )
    # testing the recovery feature
    out <- capture_messages({
        expect_message(
            object = {
                sits_smooth(
                    sinop_probs,
                    output_dir = output_dir,
                    multicores = 2,
                    memsize = 4
                )
            },
            regexp = "Recovery"
        )
    })
    expect_true(grepl("output_dir", out[1]))

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
    expect_error(sits_label_classification(
        sinop_uncert, output_dir = tempdir()
    ))

    expect_true(all(file.exists(unlist(sinop_uncert$file_info[[1]]$path))))
    r_unc <- .raster_open_rast(sinop_uncert$file_info[[1]]$path[[1]])
    expect_true(.raster_nrows(r_unc) == .tile_nrows(sinop_probs))

    max_unc <- max(.raster_get_values(r_unc))
    expect_true(max_unc <= 10000)

    sinop5 <- sinop_uncert
    class(sinop5) <- "data.frame"
    new_cube5 <- .cube_find_class(sinop5)
    expect_true("raster_cube" %in% class(new_cube5))
    expect_true("derived_cube" %in% class(new_cube5))
    expect_true("uncert_cube" %in% class(new_cube5))


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


    sinop6 <- sinop_probs
    class(sinop6) <- "data.frame"

    sinop_bayes_3 <- sits_smooth(sinop6, output_dir = tempdir())
    expect_equal(sits_bands(sinop_bayes_3), "bayes")

    expect_error(sits_smooth(sinop, output_dir = tempdir()))
    expect_error(sits_smooth(sinop_class, output_dir = tempdir()))
    expect_error(sits_smooth(sinop_uncert, output_dir = tempdir()))



    expect_true(all(file.remove(unlist(sinop_class$file_info[[1]]$path))))
    expect_true(all(file.remove(unlist(sinop_bayes$file_info[[1]]$path))))
    expect_true(all(file.remove(unlist(sinop_bayes_2$file_info[[1]]$path))))
    expect_true(all(file.remove(unlist(sinop_bayes_3$file_info[[1]]$path))))

    expect_true(all(file.remove(unlist(sinop_probs$file_info[[1]]$path))))
    expect_true(all(file.remove(unlist(sinop_uncert$file_info[[1]]$path))))
})

test_that("Clean classification",{

    rfor_model <- sits_train(samples_modis_ndvi, sits_rfor())

    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")

    sinop <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6",
        data_dir = data_dir,
        progress = FALSE
    )
    output_dir <- paste0(tempdir(), "/clean")
    if (!dir.exists(output_dir)) {
        dir.create(output_dir)
    }

    sinop_probs <- sits_classify(
        data = sinop,
        ml_model = rfor_model,
        output_dir = output_dir,
        memsize = 4,
        multicores = 1,
        progress = FALSE
    )
    sinop_class <- sits_label_classification(
        sinop_probs,
        output_dir = output_dir,
        progress = FALSE
    )
    sum_orig <- summary(sinop_class)

    # testing sits clean
    clean_cube <- sits_clean(
        cube = sinop_class,
        output_dir = output_dir,
        progress = FALSE
    )
    # testing the recovery feature
    out <- capture_messages({
        expect_message(
            object = {
                sits_clean(
                    cube = sinop_class,
                    output_dir = output_dir,
                    progress = FALSE
                )
            },
            regexp = "Recovery"
        )
    })
    sum_clean <- summary(clean_cube)

    expect_equal(nrow(sum_orig), nrow(sum_clean))
    expect_equal(sum(sum_orig$count), sum(sum_clean$count))
    expect_lt(sum_orig[2,4], sum_clean[2,4])

    # test errors in sits_clean
    expect_error(
        sits_clean(cube = sinop,
                   output_dir = output_dir)
    )
    expect_error(
        sits_clean(cube = sinop_probs,
                   output_dir = output_dir)
    )
    sp <- sinop_class
    class(sp) <- "data.frame"

    clean_cube2 <- sits_clean(
        cube = sp,
        output_dir = output_dir,
        version = "v2",
        progress = FALSE
    )
    sum_clean2 <- summary(clean_cube2)

    expect_equal(nrow(sum_orig), nrow(sum_clean2))
    expect_equal(sum(sum_orig$count), sum(sum_clean2$count))
    expect_lt(sum_orig[2,4], sum_clean2[2,4])

})
test_that("Raster GDAL datatypes", {
    gdal_type <- .raster_gdal_datatype("INT2U")
    expect_equal(gdal_type, "UInt16")
})

test_that("Raster terra interface", {
    r_obj <- .raster_new_rast.terra(
        nrows = 766,
        ncols = 1307,
        xmin = 534780,
        ymin = 9025580,
        xmax = 560920,
        ymax = 9040900,
        nlayers = 1,
        crs = 3270
    )
    expect_equal(nrow(r_obj), 766)
    expect_equal(ncol(r_obj), 1307)
    expect_equal(terra::xmin(r_obj), 534780)

    r_obj_1 <- .raster_new_rast.terra(
        nrows = 766,
        ncols = 1307,
        xmin = 534780,
        ymin = 9025580,
        xmax = 560920,
        ymax = 9040900,
        nlayers = 1,
        crs = 3270,
        xres = 20,
        yres = 20
    )
    expect_equal(nrow(r_obj_1), 766)
    expect_equal(ncol(r_obj_1), 1307)
    expect_equal(terra::xmin(r_obj_1), 534780)

    block <- c("col" = 1, "row" = 1, "ncols" = 100, "nrows" = 100)
    bbox <- .raster_bbox(r_obj, block = block)
    expect_equal(bbox[["xmin"]], 534780)
    expect_equal(bbox[["ymin"]], 9038900)
    expect_equal(bbox[["xmax"]], 536780)
    expect_equal(bbox[["ymax"]], 9040900)

    prodes_dir <- system.file("extdata/raster/prodes", package = "sits")
    prodes_file <- list.files(prodes_dir)
    r_clone <- .raster_clone(paste0(prodes_dir, "/" ,prodes_file), nlayers = 1)
    r_prodes <- .raster_open_rast(paste0(prodes_dir, "/", prodes_file))
    expect_equal(nrow(r_clone), nrow(r_prodes))
    expect_equal(ncol(r_clone), ncol(r_prodes))
})
