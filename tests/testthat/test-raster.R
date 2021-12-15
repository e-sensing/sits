source("./test-utils.R")

test_that("One-year, single core classification", {
    samples_2bands <- sits_select(samples_modis_4bands,
                                  bands = c("EVI", "NDVI"))
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
                output_dir = tempdir(),
                memsize = 4,
                multicores = 2
            )
        )
    },
    error = function(e) {
        return(NULL)
    })

    if (purrr::is_null(sinop_probs)) {
        skip("Unable to allocate multicores")
    }

    expect_true(all(file.exists(unlist(sinop_probs$file_info[[1]]$path))))
    r_obj <- .raster_open_rast(sinop_probs$file_info[[1]]$path[[1]])
    expect_true(.raster_nrows(r_obj) == .cube_size(sinop_probs)[["nrows"]])

    max_lyr2 <- max(.raster_get_values(r_obj)[, 2])
    expect_true(max_lyr2 <= 10000)

    max_lyr3 <- max(.raster_get_values(r_obj)[, 3])
    expect_true(max_lyr3 <= 10000)

    expect_true(all(file.remove(unlist(sinop_probs$file_info[[1]]$path))))
})

test_that("One-year, single core classification with filter", {

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
})

test_that("One-year, multicore classification with filter", {

    samples_filt <-
        sits_select(samples_modis_4bands, bands = c("NDVI", "EVI")) %>%
        sits_apply(NDVI = sits_whittaker(NDVI, lambda = 3.0),
                   EVI = sits_whittaker(EVI, lambda = 3.0))

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
        data_dir = data_dir,
        delim = "_",
        parse_info = c("X1", "X2", "tile", "band", "date")
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


    expect_true(all(file.remove(unlist(sinop_class$file_info[[1]]$path))))
    expect_true(all(file.remove(unlist(sinop_bayes$file_info[[1]]$path))))
    expect_true(all(file.remove(unlist(sinop_gauss$file_info[[1]]$path))))
    expect_true(all(file.remove(unlist(sinop_bil$file_info[[1]]$path))))

    expect_true(all(file.remove(unlist(sinop_probs$file_info[[1]]$path))))
    expect_true(all(file.remove(unlist(sinop_uncert$file_info[[1]]$path))))
})
