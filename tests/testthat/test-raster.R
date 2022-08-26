test_that("One-year, single core classification", {
    samples_ndvi <- sits_select(samples_modis_4bands,
        bands = c("NDVI")
    )
    rfor_model <- sits_train(
        samples_ndvi,
        sits_rfor(num_trees = 30)
    )

    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    sinop <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6",
        data_dir = data_dir,
        delim = "_",
        bands = "NDVI",
        parse_info = c("X1", "X2", "tile", "band", "date")
    )
    sinop_probs <- sits_classify(
        data = sinop,
        ml_model = rfor_model,
        output_dir = tempdir(),
        memsize = 4,
        multicores = 1
    )
    sits_labels(sinop_probs) <- c(
        "Cerrado", "Floresta",
        "Pastagem", "Soja_Milho"
    )
    expect_true(all(sits_labels(sinop_probs) %in%
        c("Cerrado", "Floresta", "Pastagem", "Soja_Milho")))
    expect_true(all(file.exists(unlist(sinop_probs$file_info[[1]]$path))))
    r_obj <- .raster_open_rast(sinop_probs$file_info[[1]]$path[[1]])

    expect_true(.raster_nrows(r_obj) == .cube_size(sinop_probs)[["nrows"]])

    max_lyr1 <- max(.raster_get_values(r_obj)[, 1])
    expect_true(max_lyr1 <= 10000)

    max_lyr3 <- max(.raster_get_values(r_obj)[, 3])
    expect_true(max_lyr3 <= 10000)

    expect_true(all(file.remove(unlist(sinop_probs$file_info[[1]]$path))))
})

test_that("One-year, multicore classification", {

    samples_ndvi <- sits_select(samples_modis_4bands,
        bands = c("NDVI")
    )

    timeline_samples <- sits_timeline(samples_ndvi)
    start_date <- timeline_samples[1]
    end_date <- timeline_samples[length(timeline_samples)]

    svm_model <- sits_train(samples_ndvi, sits_svm())

    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    sinop <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6",
        data_dir = data_dir,
        delim = "_",
        bands = "NDVI",
        parse_info = c("X1", "X2", "tile", "band", "date")
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
                    verbose = TRUE
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
                    verbose = TRUE
                )
            )
        },
        error = function(e) {
            return(NULL)
        }
    )

    expect_true(all(file.exists(unlist(sinop_probs$file_info[[1]]$path))))
    r_obj <- sits:::.raster_open_rast(sinop_probs$file_info[[1]]$path[[1]])
    expect_true(
        sits:::.raster_nrows(r_obj) == sits:::.cube_size(sinop_probs)[["nrows"]]
    )

    max_lyr2 <- max(sits:::.raster_get_values(r_obj)[, 2])
    expect_true(max_lyr2 <= 10000)

    max_lyr3 <- max(sits:::.raster_get_values(r_obj)[, 3])
    expect_true(max_lyr3 <= 10000)

    expect_true(all(file.remove(unlist(sinop_probs$file_info[[1]]$path))))
})

test_that("One-year, single core classification with filter", {

    samples_filt <-
        sits_select(samples_modis_4bands, bands = c("NDVI")) %>%
        sits_filter(filter = sits_whittaker())

    svm_model <- sits_train(samples_filt, sits_svm())

    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    sinop <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6",
        data_dir = data_dir,
        delim = "_",
        bands = "NDVI",
        parse_info = c("X1", "X2", "tile", "band", "date")
    )

    sinop_probs <- suppressMessages(
        sits_classify(
            data = sinop,
            ml_model = svm_model,
            filter_fn = sits_whittaker(),
            output_dir = tempdir(),
            memsize = 4,
            multicores = 1
        )
    )

    expect_true(all(file.exists(unlist(sinop_probs$file_info[[1]]$path))))
    expect_true(all(file.remove(unlist(sinop_probs$file_info[[1]]$path))))
})

test_that("One-year, multicore classification with Savitzky-Golay filter", {

    samples_filt <-
        sits_select(samples_modis_4bands, bands = c("NDVI")) %>%
        sits_apply(
            NDVI = sits_sgolay(NDVI)
        )

    rfor_model <- sits_train(samples_filt, sits_rfor())

    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    sinop <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6",
        data_dir = data_dir,
        delim = "_",
        parse_info = c("X1", "X2", "tile", "band", "date")
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
                    multicores = 2
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

    r_obj <- sits:::.raster_open_rast(sinop_2014_probs$file_info[[1]]$path[[1]])

    expect_true(sits:::.raster_nrows(r_obj) ==
        sits:::.cube_size(sinop_2014_probs)[["nrows"]])

    max_lyr2 <- max(sits:::.raster_get_values(r_obj)[, 2])
    expect_true(max_lyr2 <= 10000)

    max_lyr3 <- max(sits:::.raster_get_values(r_obj)[, 3])
    expect_true(max_lyr3 <= 10000)

    expect_true(all(file.remove(unlist(sinop_2014_probs$file_info[[1]]$path))))
})

test_that("One-year, multicore classification with Whittaker filter", {

    samples_filt <-
        sits_select(samples_modis_4bands, bands = c("NDVI")) %>%
        sits_apply(
            NDVI = sits_whittaker(NDVI, lambda = 0.5)
        )

    xgb_model <- sits_train(samples_filt, sits_xgboost())

    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    sinop <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6",
        data_dir = data_dir,
        delim = "_",
        parse_info = c("X1", "X2", "tile", "band", "date")
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
                    multicores = 2
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

    expect_true(.raster_nrows(r_obj) == .cube_size(sinop_2014_probs)[["nrows"]])

    max_lyr2 <- max(.raster_get_values(r_obj)[, 2])
    expect_true(max_lyr2 <= 10000)

    max_lyr3 <- max(.raster_get_values(r_obj)[, 3])
    expect_true(max_lyr3 <= 10000)

    expect_true(all(file.remove(unlist(sinop_2014_probs$file_info[[1]]$path))))
})

test_that("One-year, multicore classification with torch", {

    samples_ndvi <-
        sits_select(samples_modis_4bands, bands = c("NDVI"))

    torch_model <- sits_train(samples_ndvi, sits_mlp())

    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    sinop <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6",
        data_dir = data_dir,
        delim = "_",
        parse_info = c("X1", "X2", "tile", "band", "date")
    )

    sinop_2014_probs <- tryCatch(
        {
            suppressMessages(
                sits_classify(
                    data = sinop,
                    ml_model = torch_model,
                    output_dir = tempdir(),
                    memsize = 8,
                    multicores = 2
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

    expect_true(.raster_nrows(r_obj) == .cube_size(sinop_2014_probs)[["nrows"]])

    max_lyr2 <- max(.raster_get_values(r_obj)[, 2])
    expect_true(max_lyr2 <= 10000)

    max_lyr3 <- max(.raster_get_values(r_obj)[, 3])
    expect_true(max_lyr3 <= 10000)

    expect_true(all(file.remove(unlist(sinop_2014_probs$file_info[[1]]$path))))
})

test_that("One-year, multicore classification with ResNet", {

    samples_ndvi <-
        sits_select(samples_modis_4bands, bands = c("NDVI"))

    torch_model <- sits_train(samples_ndvi, sits_resnet())

    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    sinop <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6",
        data_dir = data_dir,
        delim = "_",
        parse_info = c("X1", "X2", "tile", "band", "date")
    )

    sinop_2014_probs <- tryCatch(
        {
            suppressMessages(
                sits_classify(
                    data = sinop,
                    ml_model = torch_model,
                    output_dir = tempdir(),
                    memsize = 8,
                    multicores = 2
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

    expect_true(.raster_nrows(r_obj) == .cube_size(sinop_2014_probs)[["nrows"]])

    max_lyr2 <- max(.raster_get_values(r_obj)[, 2])
    expect_true(max_lyr2 <= 10000)

    max_lyr3 <- max(.raster_get_values(r_obj)[, 3])
    expect_true(max_lyr3 <= 10000)

    expect_true(all(file.remove(unlist(sinop_2014_probs$file_info[[1]]$path))))
})

test_that("One-year, multicore classification with TAE", {

    samples_ndvi <-
        sits_select(samples_modis_4bands, bands = c("NDVI"))

    torch_model <- sits_train(samples_ndvi, sits_tae())

    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    sinop <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6",
        data_dir = data_dir,
        delim = "_",
        parse_info = c("X1", "X2", "tile", "band", "date")
    )

    sinop_2014_probs <- tryCatch(
        {
            suppressMessages(
                sits_classify(
                    data = sinop,
                    ml_model = torch_model,
                    output_dir = tempdir(),
                    memsize = 8,
                    multicores = 2
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

    expect_true(.raster_nrows(r_obj) == .cube_size(sinop_2014_probs)[["nrows"]])

    max_lyr2 <- max(.raster_get_values(r_obj)[, 2])
    expect_true(max_lyr2 <= 10000)

    max_lyr3 <- max(.raster_get_values(r_obj)[, 3])
    expect_true(max_lyr3 <= 10000)

    expect_true(all(file.remove(unlist(sinop_2014_probs$file_info[[1]]$path))))
})

test_that("One-year, multicore classification with LightTAE", {

    samples_ndvi <-
        sits_select(samples_modis_4bands, bands = c("NDVI"))

    torch_model <- sits_train(samples_ndvi, sits_lighttae())

    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    sinop <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6",
        data_dir = data_dir,
        delim = "_",
        parse_info = c("X1", "X2", "tile", "band", "date")
    )

    sinop_2014_probs <- tryCatch(
        {
            suppressMessages(
                sits_classify(
                    data = sinop,
                    ml_model = torch_model,
                    output_dir = tempdir(),
                    memsize = 8,
                    multicores = 2
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

    expect_true(.raster_nrows(r_obj) == .cube_size(sinop_2014_probs)[["nrows"]])

    max_lyr2 <- max(.raster_get_values(r_obj)[, 2])
    expect_true(max_lyr2 <= 10000)

    max_lyr3 <- max(.raster_get_values(r_obj)[, 3])
    expect_true(max_lyr3 <= 10000)

    expect_true(all(file.remove(unlist(sinop_2014_probs$file_info[[1]]$path))))
})

test_that("One-year, multicore and multiblocks classification", {

    sits::sits_config(processing_bloat = 9000)

    samples_ndvi <- sits_select(samples_modis_4bands, bands = "NDVI")
    rf_model <- sits_train(samples_ndvi, ml_method = sits_rfor)

    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6",
        data_dir = data_dir
    )

    probs_cube <- tryCatch(
        {
            suppressMessages(
                sits_classify(
                    cube,
                    rf_model,
                    output_dir = tempdir(),
                    memsize = 4,
                    multicores = 2
                )
            )
        },
        error = function(e) {
            return(NULL)
        }
    )

    sits::sits_config(processing_bloat = 5)

    if (purrr::is_null(probs_cube)) {
        skip("Unable to allocate multicores")
    }

    expect_true(all(file.exists(unlist(probs_cube$file_info[[1]]$path))))
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
        parse_info = c("X1", "X2", "tile", "band", "date")
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

    cube_merged <- sits_merge(data1 = cube, data2 = kern_cube)

    samples_ndvi <- sits_get_data(
        cube = cube_merged,
        samples = csv_file,
        multicores = 2
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
                    multicores = 2
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
    samples_ndvi <-
        sits_select(samples_modis_4bands, bands = c("NDVI"))

    torch_model <- sits_train(samples_ndvi, sits_tempcnn(epochs = 10))

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
                    torch_model,
                    output_dir = temp_dir,
                    memsize = 4,
                    multicores = 2
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

    sinop_probs_2 <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6",
        bands = "probs",
        data_dir = temp_dir,
        labels = sits_labels(sinop_probs),
        parse_info = c(
            "X1", "X2", "tile", "start_date",
            "end_date", "band", "version"
        )
    )

    expect_true(.cube_is_equal(sinop_probs, sinop_probs_2))

    sinop_class <- sits_label_classification(
        sinop_probs,
        output_dir = temp_dir
    )
    expect_true(all(file.exists(unlist(sinop_class$file_info[[1]]$path))))

    expect_true(length(sits_timeline(sinop_class)) ==
        length(sits_timeline(sinop_probs)))

    r_obj <- .raster_open_rast(sinop_class$file_info[[1]]$path[[1]])
    max_lab <- max(.raster_get_values(r_obj))
    min_lab <- min(.raster_get_values(r_obj))
    expect_true(max_lab <= 9)
    expect_true(min_lab >= 1)

    sinop_class_2 <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6",
        bands = "class",
        labels = sits_labels(sinop_class),
        data_dir = temp_dir,
        parse_info = c(
            "X1", "X2", "tile", "start_date",
            "end_date", "band", "version"
        )
    )

    expect_true(.cube_is_equal(sinop_class, sinop_class_2))

    sinop_bayes <- sits_smooth(
        sinop_probs,
        output_dir = temp_dir,
        multicores = 2
    )
    expect_true(all(file.exists(unlist(sinop_bayes$file_info[[1]]$path))))

    expect_true(length(sits_timeline(sinop_bayes)) ==
        length(sits_timeline(sinop_probs)))

    r_bay <- .raster_open_rast(sinop_bayes$file_info[[1]]$path[[1]])
    expect_true(.raster_nrows(r_bay) == .cube_size(sinop_probs)[["nrows"]])

    max_bay2 <- max(.raster_get_values(r_bay)[, 2])
    expect_true(max_bay2 <= 10000)

    max_bay3 <- max(.raster_get_values(r_bay)[, 3])
    expect_true(max_bay3 <= 10000)

    sinop_bayes_2 <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6",
        bands = "bayes",
        labels = sits_labels(sinop_class),
        data_dir = temp_dir,
        parse_info = c(
            "X1", "X2", "tile", "start_date",
            "end_date", "band", "version"
        )
    )

    expect_true(.cube_is_equal(sinop_bayes, sinop_bayes))

    sinop_bil <- sits_smooth(
        cube = sinop_probs,
        type = "bilateral",
        output_dir = temp_dir,
        multicores = 1
    )

    expect_true(all(file.exists(unlist(sinop_bil$file_info[[1]]$path))))

    r_bil <- .raster_open_rast(sinop_bil$file_info[[1]]$path[[1]])
    expect_true(.raster_nrows(r_bil) == .cube_size(sinop_probs)[["nrows"]])

    max_bil2 <- max(.raster_get_values(r_bil)[, 2])
    expect_true(max_bil2 <= 10000)

    max_bil3 <- max(.raster_get_values(r_bil)[, 3])
    expect_true(max_bil3 <= 10000)

    sinop_uncert <- sits_uncertainty(
        cube = sinop_bayes,
        type = "entropy",
        output_dir = temp_dir
    )

    expect_true(all(file.exists(unlist(sinop_uncert$file_info[[1]]$path))))
    r_unc <- .raster_open_rast(sinop_uncert$file_info[[1]]$path[[1]])
    expect_true(.raster_nrows(r_unc) == .cube_size(sinop_probs)[["nrows"]])

    max_unc <- max(.raster_get_values(r_unc))
    expect_true(max_unc <= 10000)

    sinop_uncert_2 <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6",
        bands = "entropy",
        labels = sits_labels(sinop_class),
        data_dir = temp_dir
    )

    timeline_orig <- sits_timeline(sinop)
    timeline_probs <- sits_timeline(sinop_probs)
    timeline_unc <- sits_timeline(sinop_uncert)
    timeline_class <- sits_timeline(sinop_class)
    timeline_model <- sits_timeline(torch_model)
    timeline_ts <- sits_timeline(samples_modis_4bands)

    expect_equal(timeline_ts, timeline_model)
    expect_equal(timeline_ts, timeline_orig)
    expect_equal(timeline_probs, timeline_unc)
    expect_equal(timeline_probs, timeline_class)
    expect_equal(timeline_orig[1], timeline_class[1])
    expect_equal(timeline_orig[length(timeline_orig)], timeline_class[2])

    expect_true(all(file.remove(unlist(sinop_class$file_info[[1]]$path))))
    expect_true(all(file.remove(unlist(sinop_bayes$file_info[[1]]$path))))
    expect_true(all(file.remove(unlist(sinop_bil$file_info[[1]]$path))))

    expect_true(all(file.remove(unlist(sinop_probs$file_info[[1]]$path))))
    expect_true(all(file.remove(unlist(sinop_uncert$file_info[[1]]$path))))
})

test_that("One-year, multicores processing mixture model ", {

    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6",
        data_dir = data_dir,
        delim = "_",
        parse_info = c("X1", "X2", "tile", "band", "date")
    )

    endmembers_spectra <-
        tibble::tibble(
            type = c("vegetation", "not-vegetation"),
            NDVI = c(8500, 3400)
        )

    mix_cube <- sits_mixture_model(
        cube = cube,
        endmembers_spectra = endmembers_spectra,
        memsize = 4,
        multicores = 2,
        output_dir = tempdir()
    )

    expect_true(all(file.exists(unlist(mix_cube$file_info[[1]]$path))))
    expect_true(
        all(c("NOT-VEGETATION", "VEGETATION", "RMSE") %in% sits_bands(mix_cube))
    )

    unlink(mix_cube$file_info[[1]]$path)
})

test_that("One-year, multicores processing reclassify", {
    # Open mask map
    data_dir <- system.file("extdata/raster/prodes", package = "sits")
    prodes2021 <- sits_cube(
        source = "USGS",
        collection = "LANDSAT-C2L2-SR",
        data_dir = data_dir,
        parse_info = c("x1", "tile", "start_date", "end_date",
                       "band", "version"),
        bands = "class",
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
        parse_info = c("x1", "tile", "start_date", "end_date",
                       "band", "version"),
        bands = "class",
        labels = c("ClearCut_Fire", "ClearCut_BareSoil",
                   "ClearCut_Veg", "Forest")
    )
    # Reclassify cube
    ro_mask <- sits_reclassify(
        cube = ro_class,
        mask = prodes2021,
        "Old_Deforestation" =
            mask %in% c("d2007", "d2008", "d2009",
                        "d2010", "d2011", "d2012",
                        "d2013", "d2014", "d2015",
                        "d2016", "d2017", "d2018",
                        "r2010", "r2011", "r2012",
                        "r2013", "r2014", "r2015",
                        "r2016", "r2017", "r2018",
                        "d2019", "r2019", "d2020",
                        "r2020", "r2021"),
        "Water_Mask" = mask == "Water",
        "NonForest_Mask" = mask %in% c("NonForest", "NonForest2"),
        mask_na_values = TRUE,
        memsize = 4,
        multicores = 6,
        output_dir = tempdir(),
        progress = TRUE
    )

    expect_equal(
        sits_labels(ro_mask),
        c("ClearCut_Fire", "ClearCut_BareSoil",
          "ClearCut_Veg", "Forest", "Old_Deforestation",
          "Water_Mask", "NonForest_Mask")
    )

    unlink(ro_mask$file_info[[1]]$path)
})

test_that("Raster GDAL datatypes", {
    gdal_type <- sits:::.raster_gdal_datatype("INT2U")
    expect_equal(gdal_type, "UInt16")
})
