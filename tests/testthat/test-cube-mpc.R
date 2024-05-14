test_that("Creating S2 cubes from MPC using tiles", {
    mpc_token <- Sys.getenv("MPC_TOKEN")
    Sys.setenv("MPC_TOKEN" = "")
    s2_cube <- .try(
        {
            sits_cube(
                source = "MPC",
                collection = "SENTINEL-2-L2A",
                tiles = "20LKP",
                bands = c("B05", "CLOUD"),
                start_date = as.Date("2018-07-18"),
                end_date = as.Date("2018-08-23"),
                progress = FALSE
            )
        },
        .default = NULL
    )
    testthat::skip_if(
        purrr::is_null(s2_cube),
        "MPC is not accessible"
    )
    Sys.setenv("MPC_TOKEN" = mpc_token)
    expect_true(all(sits_bands(s2_cube) %in% c("B05", "CLOUD")))
    r <- .raster_open_rast(.tile_path(s2_cube))
    expect_equal(s2_cube$xmax[[1]], .raster_xmax(r), tolerance = 1)
    expect_equal(s2_cube$xmin[[1]], .raster_xmin(r), tolerance = 1)
    r_obj <- .raster_open_rast(s2_cube$file_info[[1]]$path[1])
    cube_nrows <- .tile_nrows(s2_cube)
    expect_true(.raster_nrows(r_obj) == cube_nrows)

    s2_cube_s2a <- .try(
        {
            sits_cube(
                source = "MPC",
                collection = "SENTINEL-2-L2A",
                tiles = "20LKP",
                bands = c("B05", "CLOUD"),
                start_date = as.Date("2018-07-18"),
                end_date = as.Date("2018-08-23"),
                progress = FALSE,
                platform = "SENTINEL-2A"
            )
        },
        .default = NULL
    )
    n_images_1 <- nrow(s2_cube$file_info[[1]])
    n_images_2 <- nrow(s2_cube_s2a$file_info[[1]])
    expect_true(n_images_2 < n_images_1)
})
test_that("Creating S2 cubes from MPC with ROI", {
    roi <- c(
        lon_min = -48.28579, lat_min = -16.05026,
        lon_max = -47.30839, lat_max = -15.50026
    )
    s2_cube_mpc <- .try(
        {
            sits_cube(
                source = "MPC",
                collection = "SENTINEL-2-L2A",
                roi = roi,
                bands = c("B05", "CLOUD"),
                start_date = as.Date("2018-07-18"),
                end_date = as.Date("2018-08-23"),
                progress = FALSE
            )
        },
        .default = NULL
    )
    testthat::skip_if(purrr::is_null(s2_cube_mpc), "MPC is not accessible")
    expect_true(all(sits_bands(s2_cube_mpc) %in% c("B05", "CLOUD")))
    expect_equal(nrow(s2_cube_mpc), 3)
    bbox_cube <- sits_bbox(s2_cube_mpc, as_crs = "EPSG:4326")
    bbox_cube_1 <- sits_bbox(.tile(s2_cube_mpc), as_crs = "EPSG:4326")
    expect_true(bbox_cube["xmax"] >= bbox_cube_1["xmax"])
    expect_true(bbox_cube["ymax"] >= bbox_cube_1["ymax"])
    r_obj <- .raster_open_rast(s2_cube_mpc$file_info[[1]]$path[1])
    cube_nrows <- .tile_nrows(s2_cube_mpc)
    expect_true(.raster_nrows(r_obj) == cube_nrows)
})
test_that("Creating Sentinel-1 GRD cubes from MPC using tiles", {

    cube_s1_grd <-  sits_cube(
        source = "MPC",
        collection = "SENTINEL-1-GRD",
        bands = c("VV"),
        orbit = "descending",
        tiles = c("21LUJ","21LVJ"),
        start_date = "2021-08-01",
        end_date = "2021-09-30"
    )
    bbox <- sits_bbox(cube_s1_grd)
    roi_cube_s1 <- sits_mgrs_to_roi(c("21LUJ","21LVJ"))

    expect_true(bbox[["xmin"]] < roi_cube_s1[["lon_min"]])
    expect_true(bbox[["xmax"]] > roi_cube_s1[["lon_max"]])
    expect_true(bbox[["ymin"]] < roi_cube_s1[["lat_min"]])
    expect_true(bbox[["ymax"]] > roi_cube_s1[["lat_max"]])
    expect_true(all(c("VV") %in% sits_bands(cube_s1_grd)))

    r_obj <- .raster_open_rast(cube_s1_grd$file_info[[1]]$path[[1]])
    expect_true(terra::nrow(r_obj) == cube_s1_grd$file_info[[1]]$nrows[[1]])

    output_dir <- paste0(tempdir(), "/s1-grd-reg")
    if (!dir.exists(output_dir)) {
        dir.create(output_dir)
    }

    cube_s1_reg <- sits_regularize(
        cube = cube_s1_grd,
        period = "P1M",
        res = 240,
        tiles = c("21LUJ","21LVJ"),
        multicores = 1,
        output_dir = output_dir,
        progress = TRUE
    )
    expect_equal(length(sits_timeline(cube_s1_reg)), 2)
    expect_true(all(c("21LUJ", "21LVJ") %in% cube_s1_reg$tile))
    expect_true(all("EPSG:32721" %in% cube_s1_reg$crs))

    bbox <- sits_bbox(cube_s1_reg, as_crs = "EPSG:4326")
    roi_cube_s1 <- sits_mgrs_to_roi(c("21LUJ","21LVJ"))

    expect_equal(bbox[["xmin"]], roi_cube_s1[["lon_min"]], tolerance = 0.01)
    expect_equal(bbox[["xmax"]], roi_cube_s1[["lon_max"]], tolerance = 0.01)
    expect_equal(bbox[["ymin"]], roi_cube_s1[["lat_min"]], tolerance = 0.01)
    expect_equal(bbox[["ymax"]], roi_cube_s1[["lat_max"]], tolerance = 0.01)
    expect_true(all(c("VV") %in% sits_bands(cube_s1_reg)))

})
test_that("Creating Sentinel-1 RTC cubes from MPC", {
    cube_s1_rtc <-  sits_cube(
        source = "MPC",
        collection = "SENTINEL-1-RTC",
        bands = c("VV"),
        orbit = "descending",
        tiles = c("21LXJ", "21LYJ"),
        start_date = "2021-07-01",
        end_date = "2021-09-30"
    )
    bbox <- sits_bbox(cube_s1_rtc[1,])
    expect_true(grepl("32722", bbox[["crs"]]))
    expect_equal(117360, bbox[["xmin"]])
    expect_equal(407410, bbox[["xmax"]])
    expect_equal(nrow(cube_s1_rtc$file_info[[1]]), 7)

    output_dir <- paste0(tempdir(), "/s1rtcreg")
    if (!dir.exists(output_dir)) {
        dir.create(output_dir)
    }

    cube_s1_rtc_reg <- sits_regularize(
        cube = cube_s1_rtc,
        period = "P16D",
        res = 240,
        tiles = c("21LXJ", "21LYJ"),
        multicores = 1,
        output_dir = output_dir,
        progress = TRUE
    )
    expect_equal(length(sits_timeline(cube_s1_rtc_reg)), 3)
    expect_true(all(c("21LXJ", "21LYJ") %in%
                        cube_s1_rtc_reg$tile))
    expect_true("EPSG:32721" %in% cube_s1_rtc_reg$crs)

    bbox <- sits_bbox(cube_s1_rtc_reg, as_crs = "EPSG:4326")
    roi_cube_s1 <- sits_mgrs_to_roi(c("21LXJ", "21LYJ"))

    expect_equal(bbox[["xmin"]], roi_cube_s1[["lon_min"]], tolerance = 0.01)
    expect_equal(bbox[["xmax"]], roi_cube_s1[["lon_max"]], tolerance = 0.01)
    expect_equal(bbox[["ymin"]], roi_cube_s1[["lat_min"]], tolerance = 0.01)
    expect_equal(bbox[["ymax"]], roi_cube_s1[["lat_max"]], tolerance = 0.01)
    expect_true(all(c("VV") %in% sits_bands(cube_s1_rtc_reg)))

})
test_that("Creating LANDSAT cubes from MPC with ROI", {
    roi <- c(
        lon_min = -48.28579, lat_min = -16.05026,
        lon_max = -47.30839, lat_max = -15.50026
    )
    mpc_token <- Sys.getenv("MPC_TOKEN")
    Sys.setenv("MPC_TOKEN" = "")
    l8_cube_mpc <- .try(
        {
            sits_cube(
                source = "MPC",
                collection = "LANDSAT-C2-L2",
                roi = roi,
                bands = c("NIR08", "CLOUD"),
                start_date = as.Date("2018-07-18"),
                end_date = as.Date("2018-08-23"),
                progress = FALSE
            )
        },
        .default = NULL
    )
    testthat::skip_if(purrr::is_null(l8_cube_mpc), "MPC is not accessible")
    Sys.setenv("MPC_TOKEN" = mpc_token)

    expect_true(all(sits_bands(l8_cube_mpc) %in% c("NIR08", "CLOUD")))
    expect_equal(nrow(l8_cube_mpc), 2)
    bbox_cube <- sits_bbox(l8_cube_mpc, as_crs = "EPSG:4326")
    bbox_cube_1 <- sits_bbox(.tile(l8_cube_mpc), as_crs = "EPSG:4326")
    expect_true(bbox_cube["xmax"] >= bbox_cube_1["xmax"])
    expect_true(bbox_cube["ymax"] >= bbox_cube_1["ymax"])
    r_obj <- .raster_open_rast(l8_cube_mpc$file_info[[1]]$path[1])
    tile_nrows <- .tile_nrows(l8_cube_mpc)[[1]]
    expect_true(.raster_nrows(r_obj) == tile_nrows)
})
test_that("Creating LANDSAT cubes from MPC with WRS", {
    expect_error(
        sits_cube(
            source = "MPC",
            collection = "LANDSAT-C2-L2",
            tiles = "223067",
            bands = c("NIR08", "CLOUD"),
            start_date = as.Date("2018-07-18"),
            end_date = as.Date("2018-08-23"),
            progress = FALSE
        )
    )
})
