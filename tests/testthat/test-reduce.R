test_that("Reduce cube with NDVI median", {
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    modis_cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6",
        data_dir = data_dir
    )

    dir_images <- paste0(tempdir(), "/reduce/")
    if (!dir.exists(dir_images)) {
        suppressWarnings(dir.create(dir_images))
    }

    reduced_cube <- sits_reduce(
        data = modis_cube,
        NDVI_MEDIAN = t_median(NDVI),
        multicores = 1,
        memsize = 1,
        output_dir = dir_images
    )

    expect_true(all(sits_bands(reduced_cube) %in% c("NDVI-MEDIAN")))
    timeline <- sits_timeline(reduced_cube)
    expect_length(timeline, 1)
    expect_true(timeline == "2013-09-14")

    values_cube <- .raster_read_rast(.cube_paths(modis_cube)[[1]])
    values_median <- .raster_read_rast(.cube_paths(reduced_cube)[[1]])

    expect_equal(
        as.integer(C_temp_median(values_cube)[4,]), values_median[4,][[1]],
        tolerance = 0.001
    )

    out <- capture_warning({
        expect_message(
            object = {
                sits_reduce(
                    data = reduced_cube,
                    NDVI_MEDIAN = t_median(NDVI),
                    multicores = 1,
                    memsize = 1,
                    output_dir = dir_images
                )
            },
            regexp = "already exists"
        )
    })

    unlink(list.files(dir_images,
                      pattern = "\\.tif$",
                      full.names = TRUE
    ))
})

test_that("Reduce samples with NDVI max",{
    reduced_samples <- sits_reduce(
        data = samples_modis_ndvi,
        NDVI_MAX = t_max(NDVI)
    )

    expect_true(all(sits_bands(reduced_samples) %in% c("NDVI-MAX")))
    timeline <- sits_timeline(reduced_samples)
    expect_length(timeline, 1)
    expect_true(timeline == "2013-09-14")

    out <- capture_warning({
        expect_message(
            object = {
                sits_reduce(
                    data = reduced_samples,
                    NDVI_MAX = t_max(NDVI)
                )
            },
            regexp = "already exists"
        )
    })

    values_sample <- as.matrix(samples_modis_ndvi$time_series[[65]][, "NDVI"])
    value_max <- reduced_samples$time_series[[65]][["NDVI-MAX"]][[1]]
    expect_equal(
        C_temp_max(t(values_sample))[[1]], value_max, tolerance = 0.001
    )
})
