test_that("Creating S2 cubes from CDSE (STAC) with ROI", {
    # Configure environment
    # # Configure environment
    cdse_env_config <- .environment_cdse()
    # Patch environment variables
    .environment_patch(cdse_env_config)
    # Test
    roi <- sits_tiles_to_roi("20LKP")

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
    expect_equal(nrow(s2_cube_cdse), 9)
    bbox_cube <- sits_bbox(s2_cube_cdse, as_crs = "EPSG:4326")
    bbox_cube_1 <- sits_bbox(.tile(s2_cube_cdse), as_crs = "EPSG:4326")
    expect_true(bbox_cube["xmax"] >= bbox_cube_1["xmax"])
    expect_true(bbox_cube["ymax"] >= bbox_cube_1["ymax"])
    rast <- .raster_open_rast(s2_cube_cdse$file_info[[1]]$path[1])
    cube_nrows <- .tile_nrows(s2_cube_cdse)
    expect_true(.raster_nrows(rast) == cube_nrows)
    # Rollback environment changes
    .environment_rollback(cdse_env_config)
    #    # Configure environment
})
test_that("Creating S2 cubes from CDSE (STAC) with one tile", {
    # Configure environment
    cdse_env_config <- .environment_cdse()
    # Patch environment variables
    .environment_patch(cdse_env_config)
    # Test
    s2_cube_20LKP <- .try(
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

    if (purrr::is_null(s2_cube_20LKP)) {
        .environment_rollback(cdse_env_config)
        testthat::skip("CDSE is not accessible")
    }

    expect_true(all(sits_bands(s2_cube_20LKP) %in% c("B05", "CLOUD")))
    r <- .raster_open_rast(.tile_path(s2_cube_20LKP))
    expect_equal(s2_cube_20LKP$xmax[[1]], .raster_xmax(r), tolerance = 1)
    expect_equal(s2_cube_20LKP$xmin[[1]], .raster_xmin(r), tolerance = 1)
    rast <- .raster_open_rast(s2_cube_20LKP$file_info[[1]]$path[1])
    cube_nrows <- .tile_nrows(s2_cube_20LKP)
    expect_true(.raster_nrows(rast) == cube_nrows)
    expect_true(s2_cube_20LKP$tile == "20LKP")
    # Rollback environment changes
    .environment_rollback(cdse_env_config)
})
test_that("Creating S2 cubes from CDSE (STAC) with multiple tiles", {
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
                tiles = c("52XDF", "37NCH"),
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
    expect_true(all(s2_cube[["tile"]] %in% c("52XDF", "37NCH")))
    # Rollback environment changes
    .environment_rollback(cdse_env_config)
})

test_that("Creating Sentinel-1 GRD cubes from CDSE (STAC)", {
    # Configure environment
    cdse_env_config <- .environment_cdse()
    # Patch environment variables
    .environment_patch(cdse_env_config)
    # Test
    cube_s1_grd <- .try(
        {
            sits_cube(
                source = "CDSE",
                collection = "SENTINEL-1-GRD",
                bands = c("VV", "VH"),
                orbit = "descending",
                roi = sits_tiles_to_roi("21LUJ"),
                start_date = "2023-01-01",
                end_date = "2023-03-01",
                multicores = 1L,
                progress = FALSE
            )
        },
        .default = NULL
    )

    if (purrr::is_null(cube_s1_grd)) {
        .environment_rollback(cdse_env_config)

        testthat::skip("CDSE is not accessible")
    }

    expect_gt(nrow(cube_s1_grd), 0)
    expect_true(all(c("VV", "VH") %in% sits_bands(cube_s1_grd)))
    expect_true(all(grepl("NoTilingSystem", cube_s1_grd[["tile"]])))
    expect_true(all(grepl("CRS84|4326", cube_s1_grd[["crs"]])))

    output_dir <- paste0(tempdir(), "/s1-cdse-grd-reg")
    if (!dir.exists(output_dir)) {
        dir.create(output_dir)
    }

    cube_s1_grd_reg <- sits_regularize(
        cube = cube_s1_grd,
        period = "P1M",
        res = 240,
        tiles = c("21LUJ"),
        multicores = 1,
        output_dir = output_dir,
        progress = FALSE
    )
    expect_equal(length(sits_timeline(cube_s1_grd_reg)), 2)
    expect_true("21LUJ" %in% cube_s1_grd_reg$tile)
    expect_true("EPSG:32721" %in% cube_s1_grd_reg$crs)
    expect_true(all(c("VV", "VH") %in% sits_bands(cube_s1_grd_reg)))

    bbox <- sits_bbox(cube_s1_grd_reg, as_crs = "EPSG:4326")
    roi_cube_s1 <- sits_tiles_to_roi("21LUJ")

    expect_equal(bbox[["xmin"]], roi_cube_s1[["lon_min"]], tolerance = 0.01)
    expect_equal(bbox[["xmax"]], roi_cube_s1[["lon_max"]], tolerance = 0.01)
    expect_equal(bbox[["ymin"]], roi_cube_s1[["lat_min"]], tolerance = 0.01)
    expect_equal(bbox[["ymax"]], roi_cube_s1[["lat_max"]], tolerance = 0.01)

    # Rollback environment changes
    .environment_rollback(cdse_env_config)
})
