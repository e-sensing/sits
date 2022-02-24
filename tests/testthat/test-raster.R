source("./test-utils.R")

test_that("One-year, single core classification", {
    samples_2bands <- sits_select(samples_modis_4bands,
                                  bands = c("NDVI", "EVI"))
    dl_model <- suppress_keras(
        sits_train(samples_2bands,
                   sits_mlp(
                       layers = c(256, 256, 256),
                       dropout_rates = c(0.5, 0.4, 0.3),
                       epochs = 80,
                       batch_size = 64,
                       verbose = 0
                   )
        )
    )

    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    sinop <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6",
        data_dir = data_dir,
        delim = "_",
        parse_info = c("X1", "X2", "tile", "band", "date")
    )
    sinop_probs <- suppress_keras(
        sits_classify(
            data = sinop,
            ml_model = dl_model,
            output_dir = tempdir(),
            memsize = 4,
            multicores = 1
        )
    )
    sits_labels(sinop_probs) <- c("Cerrado", "Floresta", "Pastagem", "Soja_Milho")
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

    samples_2bands <- sits_select(samples_modis_4bands,
                                  bands = c("EVI", "NDVI"))

    timeline_samples <- sits_timeline(samples_2bands)
    start_date <- timeline_samples[1]
    end_date <- timeline_samples[length(timeline_samples)]

    svm_model <- sits_train(samples_2bands, sits_svm())

    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    sinop <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6",
        data_dir = data_dir,
        delim = "_",
        parse_info = c("X1", "X2", "tile", "band", "date")
    )

    sinop_probs <- tryCatch({
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
    })
    if (purrr::is_null(sinop_probs)) {
        skip("Unable to allocate multicores")
    }
    sinop_probs <- tryCatch({
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
    })

    expect_true(all(file.exists(unlist(sinop_probs$file_info[[1]]$path))))
    r_obj <- sits:::.raster_open_rast(sinop_probs$file_info[[1]]$path[[1]])
    expect_true(sits:::.raster_nrows(r_obj) == sits:::.cube_size(sinop_probs)[["nrows"]])

    max_lyr2 <- max(sits:::.raster_get_values(r_obj)[, 2])
    expect_true(max_lyr2 <= 10000)

    max_lyr3 <- max(sits:::.raster_get_values(r_obj)[, 3])
    expect_true(max_lyr3 <= 10000)

    expect_true(all(file.remove(unlist(sinop_probs$file_info[[1]]$path))))
})

test_that("One-year, single core classification with filter", {

    sits:::.sits_debug(flag = TRUE)
    samples_filt <-
        sits_select(samples_modis_4bands, bands = c("EVI", "NDVI")) %>%
        sits_filter(filter = sits_whittaker())

    svm_model <- sits_train(samples_filt, sits_svm())

    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    sinop <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6",
        data_dir = data_dir,
        delim = "_",
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
    sits:::.sits_debug(flag = FALSE)
})

test_that("One-year, multicore classification with Savitsky-Golay filter", {

    samples_filt <-
        sits_select(samples_modis_4bands, bands = c("NDVI", "EVI")) %>%
        sits_apply(NDVI = sits_sgolay(NDVI),
                   EVI = sits_sgolay(EVI))

    rfor_model <- sits_train(samples_filt, sits_rfor())

    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    sinop <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6",
        data_dir = data_dir,
        delim = "_",
        parse_info = c("X1", "X2", "tile", "band", "date")
    )

    sinop_2014_probs <- tryCatch({
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
    })

    if (purrr::is_null(sinop_2014_probs)) {
        skip("Unable to allocated multicores")
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
        sits_select(samples_modis_4bands, bands = c("NDVI", "EVI")) %>%
        sits_apply(NDVI = sits_whittaker(NDVI, lambda = 0.5),
                   EVI = sits_whittaker(EVI, lambda = 0.5))

    lgbm_model <- sits_train(samples_filt, sits_lightgbm())

    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    sinop <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6",
        data_dir = data_dir,
        delim = "_",
        parse_info = c("X1", "X2", "tile", "band", "date")
    )

    sinop_2014_probs <- tryCatch({
        suppressMessages(
            sits_classify(
                data = sinop,
                ml_model = lgbm_model,
                filter = sits_whittaker(lambda = 3.0),
                output_dir = tempdir(),
                memsize = 4,
                multicores = 2
            )
        )
    },
    error = function(e) {
        return(NULL)
    })

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

test_that("One-year, multicore classification with post-processing", {
    samples_2bands <- sits_select(samples_modis_4bands,
                                  bands = c("NDVI", "EVI"))

    rfor_model <- sits_train(samples_2bands, sits_rfor())

    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    sinop <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6",
        data_dir = data_dir
    )

    sinop_probs <- tryCatch({
        suppressMessages(
            sits_classify(
                sinop,
                rfor_model,
                output_dir = tempdir(),
                memsize = 4,
                multicores = 2
            )
        )
    },
    error = function(e){
        return(NULL)
    })
    if (purrr::is_null(sinop_probs)) {
        skip("Unable to allocate multicores")
    }
    expect_true(all(file.exists(unlist(sinop_probs$file_info[[1]]$path))))

    sinop_probs_2 <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6",
        bands = "probs",
        data_dir = tempdir(),
        labels = sits_labels(sinop_probs)
    )

    expect_true(.cube_is_equal(sinop_probs, sinop_probs_2))

    sinop_class <- sits_label_classification(
        sinop_probs,
        output_dir = tempdir()
    )
    expect_true(all(file.exists(unlist(sinop_class$file_info[[1]]$path))))

    expect_true(length(sits_timeline(sinop_class)) ==
                    length(sits_timeline(sinop_probs))
    )

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
        data_dir = tempdir()
    )

    expect_true(.cube_is_equal(sinop_class, sinop_class_2))

    sinop_bayes <- sits_smooth(
        sinop_probs,
        output_dir = tempdir()
    )
    expect_true(all(file.exists(unlist(sinop_bayes$file_info[[1]]$path))))

    expect_true(length(sits_timeline(sinop_bayes)) ==
                    length(sits_timeline(sinop_probs))
    )

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
        data_dir = tempdir()
    )

    expect_true(.cube_is_equal(sinop_bayes, sinop_bayes))

    sinop_gauss <- sits_smooth(
        cube = sinop_probs,
        type = "gaussian",
        output_dir = tempdir(),
        memsize = 4,
        multicores = 2
    )
    expect_true(all(file.exists(unlist(sinop_gauss$file_info[[1]]$path))))

    r_gau <- .raster_open_rast(sinop_gauss$file_info[[1]]$path[[1]])
    expect_true(.raster_nrows(r_gau) == .cube_size(sinop_probs)[["nrows"]])

    max_gau2 <- max(.raster_get_values(r_gau)[, 2])
    expect_true(max_gau2 <= 10000)

    max_gau3 <- max(.raster_get_values(r_gau)[, 3])
    expect_true(max_gau3 <= 10000)

    sinop_bil <- sits_smooth(
        cube = sinop_probs,
        type = "bilateral",
        output_dir = tempdir()
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
        output_dir = tempdir())

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
        data_dir = tempdir()
    )

    timeline_orig <- sits_timeline(sinop)
    timeline_probs <- sits_timeline(sinop_probs)
    timeline_unc  <- sits_timeline(sinop_uncert)
    timeline_class <- sits_timeline(sinop_class)
    timeline_model <- sits_timeline(rfor_model)
    timeline_ts <- sits_timeline(samples_modis_4bands)

    expect_equal(timeline_ts, timeline_model)
    expect_equal(timeline_ts, timeline_orig)
    expect_equal(timeline_probs, timeline_unc)
    expect_equal(timeline_probs, timeline_class)
    expect_equal(timeline_orig[1], timeline_class[1])
    expect_equal(timeline_orig[length(timeline_orig)], timeline_class[2])

    expect_true(all(file.remove(unlist(sinop_class$file_info[[1]]$path))))
    expect_true(all(file.remove(unlist(sinop_bayes$file_info[[1]]$path))))
    expect_true(all(file.remove(unlist(sinop_gauss$file_info[[1]]$path))))
    expect_true(all(file.remove(unlist(sinop_bil$file_info[[1]]$path))))

    expect_true(all(file.remove(unlist(sinop_probs$file_info[[1]]$path))))
    expect_true(all(file.remove(unlist(sinop_uncert$file_info[[1]]$path))))
})

test_that("Raster GDAL datatypes", {

    gdal_type <- sits:::.raster_gdal_datatype("INT2U")
    expect_equal(gdal_type, "UInt16")
})
