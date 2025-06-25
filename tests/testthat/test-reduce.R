test_that("Reduce cube with NDVI median", {
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    modis_cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir
    )

    dir_images <- paste0(tempdir(), "/reduce/")
    if (!dir.exists(dir_images)) {
        suppressWarnings(dir.create(dir_images))
    }

    reduce_median <- sits_reduce(
        data = modis_cube,
        NDVI_MEDIAN = t_median(NDVI),
        multicores = 2,
        memsize = 4,
        output_dir = dir_images
    )

    expect_true(all(sits_bands(reduce_median) %in% c("NDVI-MEDIAN")))
    timeline <- sits_timeline(reduce_median)
    expect_length(timeline, 1)
    expect_true(timeline == "2013-09-14")

    values_cube <- .raster_read_rast(.cube_paths(modis_cube)[[1]])
    values_median <- .raster_read_rast(.cube_paths(reduce_median)[[1]])

    expect_equal(
        as.integer(C_temp_median(values_cube)[4, ]), values_median[4, ][[1]],
        tolerance = 0.001
    )

    expect_true(all(sits_bands(reduce_median) %in% c("NDVI-MEDIAN")))
    timeline <- sits_timeline(reduce_median)
    expect_length(timeline, 1)
    expect_true(timeline == "2013-09-14")
    Sys.setenv("SITS_DOCUMENTATION_MODE" = "FALSE")
    expect_message({
        object <- sits_reduce(
            data = modis_cube,
            NDVI_MEDIAN = t_median(NDVI),
            multicores = 1,
            memsize = 1,
            output_dir = dir_images
        )
    })
    reduce_min <- sits_reduce(
        data = modis_cube,
        NDVI_MIN = t_min(NDVI),
        multicores = 2,
        memsize = 4,
        output_dir = dir_images
    )
    values_min <- .raster_read_rast(.cube_paths(reduce_min)[[1]])

    expect_equal(
        as.integer(C_temp_min(values_cube)[4, ]), values_min[4, ][[1]],
        tolerance = 0.001
    )

    reduce_mean <- sits_reduce(
        data = modis_cube,
        NDVI_MEAN = t_mean(NDVI),
        multicores = 2,
        memsize = 4,
        output_dir = dir_images
    )
    values_mean <- .raster_read_rast(.cube_paths(reduce_mean)[[1]])

    expect_equal(
        as.integer(C_temp_mean(values_cube)[4, ]), values_mean[4, ][[1]],
        tolerance = 0.001
    )

    reduce_std <- sits_reduce(
        data = modis_cube,
        NDVI_STD = t_std(NDVI),
        multicores = 2,
        memsize = 4,
        output_dir = dir_images
    )
    values_std <- .raster_read_rast(.cube_paths(reduce_std)[[1]])

    expect_equal(
        as.integer(C_temp_std(values_cube)[4, ]), values_std[4, ][[1]] * 10000,
        tolerance = 0.001
    )

    reduce_skew <- sits_reduce(
        data = modis_cube,
        NDVI_SKEW = t_skewness(NDVI),
        multicores = 2,
        memsize = 4,
        output_dir = dir_images
    )
    values_skew <- .raster_read_rast(.cube_paths(reduce_skew)[[1]])

    expect_equal(
        C_temp_skew(values_cube)[4, ], values_skew[4, ][[1]],
        tolerance = 0.001
    )

    reduce_kurt <- sits_reduce(
        data = modis_cube,
        NDVI_KURT = t_kurtosis(NDVI),
        multicores = 2,
        memsize = 4,
        output_dir = dir_images
    )
    values_kurt <- .raster_read_rast(.cube_paths(reduce_kurt)[[1]])

    expect_equal(
        C_temp_kurt(values_cube)[4, ], values_kurt[4, ][[1]],
        tolerance = 0.001
    )

    reduce_amp <- sits_reduce(
        data = modis_cube,
        NDVI_AMP = t_amplitude(NDVI),
        multicores = 2,
        memsize = 4,
        output_dir = dir_images
    )
    values_amp <- .raster_read_rast(.cube_paths(reduce_amp)[[1]])

    expect_equal(
        as.integer(C_temp_amplitude(values_cube)[4, ]), values_amp[4, ][[1]],
        tolerance = 0.001
    )

    reduce_slp <- sits_reduce(
        data = modis_cube,
        NDVI_SLP = t_fslope(NDVI),
        multicores = 2,
        memsize = 4,
        output_dir = dir_images
    )
    values_slp <- .raster_read_rast(.cube_paths(reduce_slp)[[1]])

    expect_equal(
        as.integer(C_temp_fslope(values_cube)[4, ]), values_slp[4, ][[1]],
        tolerance = 0.001
    )

    reduce_fqr <- sits_reduce(
        data = modis_cube,
        NDVI_FQR = t_fqr(NDVI),
        multicores = 2,
        memsize = 4,
        output_dir = dir_images
    )
    values_fqr <- .raster_read_rast(.cube_paths(reduce_fqr)[[1]])

    expect_equal(
        as.integer(C_temp_fqr(values_cube)[4, ]), values_fqr[4, ][[1]],
        tolerance = 0.001
    )
    reduce_tqr <- sits_reduce(
        data = modis_cube,
        NDVI_TQR = t_tqr(NDVI),
        multicores = 2,
        memsize = 4,
        output_dir = dir_images
    )
    values_tqr <- .raster_read_rast(.cube_paths(reduce_tqr)[[1]])

    expect_equal(
        as.integer(C_temp_tqr(values_cube)[4, ]), values_tqr[4, ][[1]],
        tolerance = 0.001
    )
    reduce_iqr <- sits_reduce(
        data = modis_cube,
        NDVI_IQR = t_iqr(NDVI),
        multicores = 2,
        memsize = 4,
        output_dir = dir_images
    )
    values_iqr <- .raster_read_rast(.cube_paths(reduce_iqr)[[1]])

    expect_equal(
        as.integer(C_temp_iqr(values_cube)[4, ]), values_iqr[4, ][[1]],
        tolerance = 0.001
    )
    unlink(list.files(dir_images,
        pattern = "\\.tif$",
        full.names = TRUE
    ))
})

test_that("Reduce samples with NDVI max", {
    reduced_samples <- sits_reduce(
        data = samples_modis_ndvi,
        NDVI_MAX = t_max(NDVI)
    )

    expect_true(all(sits_bands(reduced_samples) %in% c("NDVI-MAX")))
    timeline <- sits_timeline(reduced_samples)
    expect_length(timeline, 1)
    expect_true(timeline == "2013-09-14")
    Sys.setenv("SITS_DOCUMENTATION_MODE" = "FALSE")
    expect_warning({
        object <- sits_reduce(
            data = reduced_samples,
            NDVI_MAX = t_max(NDVI)
        )
    })

    values_sample <- as.matrix(samples_modis_ndvi$time_series[[65]][, "NDVI"])
    value_max <- reduced_samples$time_series[[65]][["NDVI-MAX"]][[1]]
    expect_equal(
        C_temp_max(t(values_sample))[[1]], value_max,
        tolerance = 0.001
    )
})
