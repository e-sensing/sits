test_that("Testing index generation", {
    # Create a cube with two bands
    s2_cube <- tryCatch(
        {
            sits_cube(
                source = "AWS",
                collection = "SENTINEL-2-L2A",
                tiles = "20LKP",
                bands = c("B05", "B8A", "CLOUD"),
                start_date = "2019-07-18",
                end_date = "2019-08-30",
                progress = FALSE
            )
        },
        error = function(e) {
            return(NULL)
        }
    )

    testthat::skip_if(
        purrr::is_null(s2_cube),
        "AWS is not accessible"
    )

    dir_images <- paste0(tempdir(), "/images/")
    if (!dir.exists(dir_images)) {
        suppressWarnings(dir.create(dir_images))
    }

    unlink(list.files(dir_images,
                      pattern = "\\.tif$",
                      full.names = TRUE
    ))
    # Regularize cube
    gc_cube <- suppressWarnings(
        sits_regularize(
            cube = s2_cube,
            output_dir = dir_images,
            res = 160,
            period = "P1M",
            multicores = 2,
            progress = FALSE
        )
    )
    # Calculate EVI
    gc_cube_new <- sits_apply(gc_cube,
                              EVI = 2.5 * (B8A - B05) / (B8A + 2.4 * B05 + 1),
                              multicores = 1,
                              output_dir = dir_images
    )

    # Test EVI
    expect_true(all(sits_bands(gc_cube_new) %in% c("EVI", "B05", "B8A")))

    timeline <- sits_timeline(gc_cube_new)
    start_date <- timeline[1]
    end_date <- timeline[length(timeline)]

    expect_true(start_date == "2019-07-01")
    expect_true(end_date == "2019-08-01")

    file_info_b05 <- .fi(gc_cube_new) |> .fi_filter_bands(bands = "B05")
    b05_band_1 <- .raster_open_rast(file_info_b05$path[[1]])

    file_info_b8a <- .fi(gc_cube_new) |> .fi_filter_bands(bands = "B8A")
    b8a_band_1 <- .raster_open_rast(file_info_b8a$path[[1]])

    file_info_evi2 <- .fi(gc_cube_new) |> .fi_filter_bands(bands = "EVI")
    evi2_band_1 <- .raster_open_rast(file_info_evi2$path[[1]])

    b05_100 <- as.numeric(b05_band_1[100] / 10000)
    b8a_100 <- as.numeric(b8a_band_1[100] / 10000)
    evi2_100 <- as.numeric(evi2_band_1[100] / 10000)

    evi2_calc_100 <- 2.5 * (b8a_100 - b05_100) / (b8a_100 + 2.4 * b05_100 + 1)
    expect_equal(evi2_100, evi2_calc_100, tolerance = 0.001)

    b05_150 <- as.numeric(b05_band_1[150] / 10000)
    b8a_150 <- as.numeric(b8a_band_1[150] / 10000)
    evi2_150 <- as.numeric(evi2_band_1[150] / 10000)

    evi2_calc_150 <- 2.5 * (b8a_150 - b05_150) / (b8a_150 + 2.4 * b05_150 + 1)
    expect_equal(evi2_150, evi2_calc_150, tolerance = 0.001)


    gc_cube_new <- sits_apply(gc_cube_new,
                              CIRE = B8A / B05 - 1,
                              normalized = FALSE,
                              multicores = 1,
                              output_dir = dir_images
    )
    expect_true(all(sits_bands(gc_cube_new) %in% c("CIRE", "EVI", "B05", "B8A")))

    file_info_cire <- .fi(gc_cube_new) |> .fi_filter_bands(bands = "CIRE")
    cire_band_1 <- .raster_open_rast(file_info_cire$path[[1]])

    cire_100 <- as.numeric(cire_band_1[100])
    cire_calc_100 <- b8a_100 / b05_100 - 1
    expect_equal(cire_100, cire_calc_100, tolerance = 0.001)

    cire_150 <- as.numeric(cire_band_1[150])
    cire_calc_150 <- b8a_150 / b05_150 - 1
    expect_equal(cire_150, cire_calc_150, tolerance = 0.001)

    unlink(dir_images, recursive = TRUE)
})

test_that("Kernel functions", {
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir,
        progress = FALSE
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
    v_obj <- matrix(.raster_get_values(r_obj), ncol = 255, byrow = TRUE)
    r_obj_md <- .raster_open_rast(cube_median$file_info[[1]]$path[[2]])
    v_obj_md <- matrix(.raster_get_values(r_obj_md), ncol = 255, byrow = TRUE)

    median_1 <- median(as.vector(v_obj[20:22, 20:22]))
    median_2 <- v_obj_md[21, 21]

    expect_true(median_1 == median_2)
    # Recovery
    Sys.setenv("SITS_DOCUMENTATION_MODE" = "FALSE")
    expect_message({
        cube_median <- sits_apply(
            data = cube,
            output_dir = tempdir(),
            NDVI_MEDIAN = w_median(NDVI),
            window_size = 3,
            memsize = 4,
            multicores = 1
        )
    }
    )
    cube_mean <- sits_apply(
        data = cube,
        output_dir = tempdir(),
        NDVI_MEAN = w_mean(NDVI),
        window_size = 3,
        memsize = 4,
        multicores = 2
    )
    r_obj <- .raster_open_rast(cube[1, ]$file_info[[1]]$path[[1]])
    v_obj <- matrix(.raster_get_values(r_obj), ncol = 255, byrow = TRUE)
    r_obj_m <- .raster_open_rast(cube_mean$file_info[[1]]$path[[2]])
    v_obj_m <- matrix(.raster_get_values(r_obj_m), ncol = 255, byrow = TRUE)

    mean_1 <- as.integer(mean(as.vector(v_obj[4:6, 4:6])))
    mean_2 <- v_obj_m[5, 5]
    expect_true(mean_1 == mean_2)

    cube_sd <- sits_apply(
        data = cube,
        output_dir = tempdir(),
        NDVI_SD = w_sd(NDVI),
        window_size = 3,
        memsize = 4,
        multicores = 2
    )
    r_obj <- .raster_open_rast(cube[1, ]$file_info[[1]]$path[[1]])
    v_obj <- matrix(.raster_get_values(r_obj), ncol = 255, byrow = TRUE)
    r_obj_sd <- .raster_open_rast(cube_sd$file_info[[1]]$path[[2]])
    v_obj_sd <- matrix(.raster_get_values(r_obj_sd), ncol = 255, byrow = TRUE)

    sd_1 <- as.integer(sd(as.vector(v_obj[4:6, 4:6])))
    sd_2 <- v_obj_sd[5, 5]
    expect_true(sd_1 == sd_2)

    cube_min <- sits_apply(
        data = cube,
        output_dir = tempdir(),
        NDVI_MIN = w_min(NDVI),
        window_size = 3,
        memsize = 4,
        multicores = 2
    )
    r_obj <- .raster_open_rast(cube[1, ]$file_info[[1]]$path[[1]])
    v_obj <- matrix(.raster_get_values(r_obj), ncol = 255, byrow = TRUE)
    r_obj_min <- .raster_open_rast(cube_min$file_info[[1]]$path[[2]])
    v_obj_min <- matrix(.raster_get_values(r_obj_min), ncol = 255, byrow = TRUE)

    min_1 <- min(as.vector(v_obj[4:6, 4:6]))
    min_2 <- v_obj_min[5, 5]
    expect_true(min_1 == min_2)

    cube_max <- sits_apply(
        data = cube,
        output_dir = tempdir(),
        NDVI_MAX = w_max(NDVI),
        window_size = 3,
        memsize = 4,
        multicores = 2
    )
    r_obj <- .raster_open_rast(cube[1, ]$file_info[[1]]$path[[1]])
    v_obj <- matrix(.raster_get_values(r_obj), ncol = 255, byrow = TRUE)
    r_obj_max <- .raster_open_rast(cube_max$file_info[[1]]$path[[2]])
    v_obj_max <- matrix(.raster_get_values(r_obj_max), ncol = 255, byrow = TRUE)

    max_1 <- max(as.vector(v_obj[4:6, 4:6]))
    max_2 <- v_obj_max[5, 5]
    expect_true(max_1 == max_2)

    tif_files <- grep("tif",
                      list.files(tempdir(), full.names = TRUE),
                      value = TRUE
    )

    success <- file.remove(tif_files)
})
test_that("Error", {
    rfor_model <- sits_train(
        samples_modis_ndvi,
        sits_rfor(num_trees = 30)
    )
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    sinop <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir,
        progress = FALSE,
        verbose = FALSE
    )
    expect_error(.check_bbox(sinop))

    output_dir <- paste0(tempdir(), "/apply")
    if (!dir.exists(output_dir)) {
        dir.create(output_dir)
    }
    unlink(list.files(output_dir,
                      pattern = "\\.tif$",
                      full.names = TRUE
    ))

    Sys.setenv("SITS_DOCUMENTATION_MODE" = "FALSE")
    expect_warning({
        cube_median <- sits_apply(
            data = sinop,
            output_dir = tempdir(),
            NDVI = w_median(NDVI),
            window_size = 3,
            memsize = 4,
            multicores = 2
        )
    })
    sinop_probs <- sits_classify(
        data = sinop,
        ml_model = rfor_model,
        output_dir = output_dir,
        memsize = 4,
        multicores = 1,
        progress = FALSE
    )
    expect_error(sits_apply(sinop_probs))

    unlink(output_dir, recursive = TRUE)
})
