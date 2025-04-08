test_that("Classify with random forest - single core and multicore", {
    rfor_model <- sits_train(samples_modis_ndvi, sits_rfor(num_trees = 40))

    expect_type(rfor_model, "closure")
    point_ndvi <- sits_select(point_mt_6bands, bands = "NDVI")
    class_ndvi <- sits_classify(
        data = point_ndvi,
        ml_model = rfor_model,
        progress = FALSE
    )

    expect_true(nrow(class_ndvi$predicted[[1]]) == 17)
    expect_true(all(class_ndvi$predicted[[1]]$class %in%
        sits_labels(samples_modis_ndvi)))
    point_ndvi <- sits_select(point_mt_6bands, bands = "NDVI")
    class_ndvi <- sits_classify(
        data = point_ndvi,
        ml_model = rfor_model,
        multicores = 2,
        progress = FALSE
    )

    expect_true(nrow(class_ndvi$predicted[[1]]) == 17)
    expect_true(all(class_ndvi$predicted[[1]]$class %in%
        sits_labels(samples_modis_ndvi)))
})

test_that("Classify a set of time series with svm + filter", {
    # single core
    samples_filt <- sits_apply(cerrado_2classes,
        NDVI = sits_sgolay(NDVI),
        EVI = sits_sgolay(EVI),
    )

    svm_model <- sits_train(samples_filt, sits_svm())

    class1 <- sits_classify(cerrado_2classes,
        ml_model = svm_model,
        filter_fn = sits_sgolay(),
        multicores = 2,
        progress = FALSE,
    )

    expect_true(class1$predicted[[1]]$class %in%
        sits_labels(cerrado_2classes))
})

test_that("Classify error bands 1", {
    model <- sits_train(samples_modis_ndvi, sits_svm())
    point <- sits_select(point_mt_6bands, bands = "EVI")

    expect_error(
        sits_classify(
            data = point,
            ml_model = model
        )
    )
})

test_that("Classify with NA values", {
    # load cube
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    raster_cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir,
        tiles = "012010",
        bands = "NDVI",
        start_date = "2013-09-14",
        end_date = "2014-08-29",
        multicores = 2,
        progress = FALSE
    )
    # preparation - create directory to save NA
    data_dir <- paste0(tempdir(), "/na-cube")
    dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
    # preparation - insert NA in cube
    raster_cube <- sits_apply(
        data = raster_cube,
        NDVI_NA = ifelse(NDVI > 0.5, NA, NDVI),
        output_dir = data_dir
    )
    raster_cube <- sits_select(raster_cube, bands = "NDVI_NA")
    .fi(raster_cube) <- .fi(raster_cube) |>
                            dplyr::mutate(band = "NDVI")
    # preparation - create a random forest model
    rfor_model <- sits_train(samples_modis_ndvi, sits_rfor(num_trees = 40))
    # test classification with NA
    class_map <- sits_classify(
        data = raster_cube,
        ml_model = rfor_model,
        output_dir = data_dir,
        progress = FALSE
    )
    class_map_rst <- .raster_open_rast(class_map[["file_info"]][[1]][["path"]])
    expect_true(anyNA(class_map_rst[]))
    # remove test files
    unlink(data_dir)
})

test_that("Classify with exclusion mask", {
    # load cube
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    raster_cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir,
        tiles = "012010",
        bands = "NDVI",
        start_date = "2013-09-14",
        end_date = "2014-08-29",
        multicores = 2,
        progress = FALSE
    )
    # preparation - create directory to save NA
    data_dir <- paste0(tempdir(), "/exclusion-mask-na")
    dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
    # preparation - create exclusion mask
    exclusion_mask <- sf::st_as_sfc(
        x = sf::st_bbox(c(
            xmin = -55.63478,
            ymin = -11.63328,
            xmax = -55.54080,
            ymax = -11.56978
        ),
            crs = "EPSG:4326"
        )
    )
    # transform object to cube crs
    exclusion_mask <- sf::st_transform(exclusion_mask, .cube_crs(raster_cube))
    # preparation - calculate centroid of the exclusion mask
    exclusion_mask_centroid <- sf::st_centroid(exclusion_mask)
    # preparation - create a random forest model
    rfor_model <- sits_train(samples_modis_ndvi, sits_rfor(num_trees = 40))
    # test classification with NA
    probs_map <- suppressWarnings(
        sits_classify(
            data = raster_cube,
            ml_model = rfor_model,
            output_dir = data_dir,
            exclusion_mask = exclusion_mask,
            progress = FALSE
        )
    )
    # testing original data
    probs_map_rst <- .raster_open_rast(probs_map[["file_info"]][[1]][["path"]])
    expect_true(anyNA(probs_map_rst[]))
    # extract values
    probs_map_value <- .raster_extract(
        probs_map_rst,
        .raster_open_vect(exclusion_mask_centroid)
    )

    expect_true(any(is.na(probs_map_value)))
    # remove test files
    unlink(data_dir)
})
