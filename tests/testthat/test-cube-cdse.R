test_that("Creating S2 cubes from CDSE with ROI", {
    # Configure environment
    cdse_env_config <- .environment_cdse()
    # Patch environment variables
    .environment_patch(cdse_env_config)
    # Test
    roi <- c(
        lon_min = -48.28579, lat_min = -16.05026,
        lon_max = -47.30839, lat_max = -15.50026
    )
    s2_cube_cdse <- .try(
        {
            sits_cube(
                source = "CDSE",
                collection = "SENTINEL-2-L2A",
                roi = roi,
                bands = c("B05", "CLOUD"),
                start_date = as.Date("2018-07-18"),
                end_date = as.Date("2018-08-23"),
                progress = FALSE,
                multicores = 1L
            )
        },
        .default = NULL
    )

    if (purrr::is_null(s2_cube_cdse)) {
        .environment_rollback(cdse_env_config)

        testthat::skip("CDSE is not accessible")
    }

    expect_true(all(sits_bands(s2_cube_cdse) %in% c("B05", "CLOUD")))
    expect_equal(nrow(s2_cube_cdse), 3)
    bbox_cube <- sits_bbox(s2_cube_cdse, as_crs = "EPSG:4326")
    bbox_cube_1 <- sits_bbox(.tile(s2_cube_cdse), as_crs = "EPSG:4326")
    expect_true(bbox_cube["xmax"] >= bbox_cube_1["xmax"])
    expect_true(bbox_cube["ymax"] >= bbox_cube_1["ymax"])
    r_obj <- .raster_open_rast(s2_cube_cdse$file_info[[1]]$path[1])
    cube_nrows <- .tile_nrows(s2_cube_cdse)
    expect_true(.raster_nrows(r_obj) == cube_nrows)
    # Rollback environment changes
    .environment_rollback(cdse_env_config)
})
test_that("Creating S2 cubes from CDSE with tiles", {
    # Configure environment
    cdse_env_config <- .environment_cdse()
    # Patch environment variables
    .environment_patch(cdse_env_config)
    # Test
    s2_cube <- .try(
        {
            sits_cube(
                source = "CDSE",
                collection = "SENTINEL-2-L2A",
                tiles = "20LKP",
                bands = c("B05", "CLOUD"),
                start_date = as.Date("2018-07-18"),
                end_date = as.Date("2018-08-23"),
                progress = FALSE,
                multicores = 1L
            )
        },
        .default = NULL
    )

    if (purrr::is_null(s2_cube)) {
        .environment_rollback(cdse_env_config)

        testthat::skip("CDSE is not accessible")
    }

    expect_true(all(sits_bands(s2_cube) %in% c("B05", "CLOUD")))
    r <- .raster_open_rast(.tile_path(s2_cube))
    expect_equal(s2_cube$xmax[[1]], .raster_xmax(r), tolerance = 1)
    expect_equal(s2_cube$xmin[[1]], .raster_xmin(r), tolerance = 1)
    r_obj <- .raster_open_rast(s2_cube$file_info[[1]]$path[1])
    cube_nrows <- .tile_nrows(s2_cube)
    expect_true(.raster_nrows(r_obj) == cube_nrows)
    expect_true(s2_cube$tile == "20LKP")
    # Rollback environment changes
    .environment_rollback(cdse_env_config)
})

test_that("Creating Sentinel-1 RTC cubes from CDSE", {
    # Configure environment
    cdse_env_config <- .environment_cdse()
    # Patch environment variables
    .environment_patch(cdse_env_config)
    # Test
    cube_s1_rtc <- .try(
        {
            sits_cube(
                source = "CDSE",
                collection = "SENTINEL-1-RTC",
                bands = c("VV"),
                orbit = "descending",
                tiles = c("36NWH"),
                start_date = "2021-07-01",
                end_date = "2021-09-30",
                progress = FALSE,
                multicores = 1L
            )
        },
        .default = NULL
    )

    if (purrr::is_null(cube_s1_rtc)) {
        .environment_rollback(cdse_env_config)

        testthat::skip("CDSE is not accessible")
    }

    bbox <- sits_bbox(cube_s1_rtc[1,])
    expect_true(grepl("4326", bbox[["crs"]]))
    expect_equal(32, bbox[["xmin"]])
    expect_equal(34, bbox[["xmax"]])
    expect_equal(nrow(cube_s1_rtc$file_info[[1]]), 68)

    output_dir <- paste0(tempdir(), "/s1rtcreg")
    if (!dir.exists(output_dir)) {
        dir.create(output_dir)
    }

    cube_s1_rtc_reg <- sits_regularize(
        cube = cube_s1_rtc,
        period = "P1M",
        res = 240,
        tiles = c("36NWH"),
        multicores = 1,
        output_dir = output_dir,
        progress = TRUE
    )
    expect_equal(length(sits_timeline(cube_s1_rtc_reg)), 3)
    expect_true("36NWH" %in% cube_s1_rtc_reg$tile)
    expect_true("EPSG:32636" %in% cube_s1_rtc_reg$crs)

    bbox <- sits_bbox(cube_s1_rtc_reg, as_crs = "EPSG:4326")
    roi_cube_s1 <- sits_mgrs_to_roi("36NWH")

    expect_equal(bbox[["xmin"]], roi_cube_s1[["lon_min"]], tolerance = 0.01)
    expect_equal(bbox[["xmax"]], roi_cube_s1[["lon_max"]], tolerance = 0.03)
    expect_equal(bbox[["ymin"]], roi_cube_s1[["lat_min"]], tolerance = 0.25)
    expect_equal(bbox[["ymax"]], roi_cube_s1[["lat_max"]], tolerance = 0.01)
    expect_true(all(c("VV") %in% sits_bands(cube_s1_rtc_reg)))

    # Rollback environment changes
    .environment_rollback(cdse_env_config)
})
