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
    roi_cube_s1 <- sits_tiles_to_roi(c("21LUJ","21LVJ"))

    expect_true(bbox[["xmin"]] < roi_cube_s1[["xmin"]])
    expect_true(bbox[["xmax"]] > roi_cube_s1[["xmax"]])
    expect_true(bbox[["ymin"]] < roi_cube_s1[["ymin"]])
    expect_true(bbox[["ymax"]] > roi_cube_s1[["ymax"]])
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
    roi_cube_s1 <- sits_tiles_to_roi(c("21LUJ","21LVJ"))

    expect_equal(bbox[["xmin"]], roi_cube_s1[["xmin"]], tolerance = 0.01)
    expect_equal(bbox[["xmax"]], roi_cube_s1[["xmax"]], tolerance = 0.01)
    expect_equal(bbox[["ymin"]], roi_cube_s1[["ymin"]], tolerance = 0.01)
    expect_equal(bbox[["ymax"]], roi_cube_s1[["ymax"]], tolerance = 0.01)
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
    expect_equal(length(sits_timeline(cube_s1_rtc_reg)), 5)
    expect_true(all(c("21LXJ", "21LYJ") %in%
                        cube_s1_rtc_reg$tile))
    expect_true("EPSG:32721" %in% cube_s1_rtc_reg$crs)

    bbox <- sits_bbox(cube_s1_rtc_reg, as_crs = "EPSG:4326")
    roi_cube_s1 <- sits_tiles_to_roi(c("21LXJ", "21LYJ"))

    expect_equal(bbox[["xmin"]], roi_cube_s1[["xmin"]], tolerance = 0.01)
    expect_equal(bbox[["xmax"]], roi_cube_s1[["xmax"]], tolerance = 0.01)
    expect_equal(bbox[["ymin"]], roi_cube_s1[["ymin"]], tolerance = 0.01)
    expect_equal(bbox[["ymax"]], roi_cube_s1[["ymax"]], tolerance = 0.01)
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
test_that("Creating cubes from MPC - MOD13Q1-6.1 based on ROI using sf object", {
    shp_file <- system.file(
        "extdata/shapefiles/mato_grosso/mt.shp",
        package = "sits"
    )
    sf_mt <- sf::read_sf(shp_file)
    # create a raster cube file based on the information about the files
    modis_cube <- .try(
        {
            sits_cube(
                source = "MPC",
                collection = "MOD13Q1-6.1",
                bands = c("NDVI", "EVI"),
                roi = sf_mt,
                start_date = "2018-09-01",
                end_date = "2019-08-29",
                progress = FALSE
            )
        },
        .default = NULL
    )
    testthat::skip_if(purrr::is_null(modis_cube),
                      message = "MPC is not accessible"
    )
    expect_true(all(sits_bands(modis_cube) %in% c("NDVI", "EVI")))
    bbox <- sits_bbox(modis_cube, as_crs = "EPSG:4326")
    bbox_shp <- sf::st_bbox(sf_mt)
    expect_lt(bbox["xmin"], bbox_shp["xmin"])
    expect_lt(bbox["ymin"], bbox_shp["ymin"])
    expect_gt(bbox["xmax"], bbox_shp["xmax"])
    expect_gt(bbox["ymax"], bbox_shp["ymax"])
    intersects <- .cube_intersects(modis_cube, sf_mt)
    expect_true(all(intersects))

    modis_cube2 <- modis_cube
    class(modis_cube2) <- "data.frame"
    in2 <- .cube_intersects(modis_cube2, sf_mt)
    expect_true(all(in2))
    expect_true(.tile_intersects(modis_cube2[1,], sf_mt))

    expect_false(.tile_within(modis_cube2[1,], sf_mt))
    expect_false(.tile_within(modis_cube2[6,], sf_mt))

    modis_cube3 <- .cube_filter_spatial(modis_cube2, sf_mt)
    expect_equal(nrow(modis_cube2), nrow(modis_cube3))

    modis_cube4 <- .cube_filter_bands(modis_cube2, "EVI")
    expect_true(.cube_bands(modis_cube4) %in% .cube_bands(modis_cube2))
    tile <- modis_cube2[1,]
    modis_evi <- .tile_filter_bands(tile, "EVI")
    expect_equal("EVI", sits_bands(modis_evi))

    modis_tiles <- .cube_tiles(modis_cube2)
    expect_true(all(c("h13v10", "h13v9") %in% .cube_tiles(modis_cube)))

    tile_h13v10 <- .cube_filter_tiles(modis_cube, "h13v10")
    expect_equal(nrow(tile_h13v10), 1)

})
test_that("Creating cubes from MPC - MOD09A1-6.1 based on ROI using sf object", {
    shp_file <- system.file(
        "extdata/shapefiles/mato_grosso/mt.shp",
        package = "sits"
    )
    sf_mt <- sf::read_sf(shp_file)
    # create a raster cube file based on the information about the files
    modis09a1_cube <- .try(
        {
            sits_cube(
                source = "MPC",
                collection = "MOD09A1-6.1",
                bands = c("BLUE", "RED", "GREEN"),
                roi = sf_mt,
                start_date = "2018-09-01",
                end_date = "2019-08-29",
                progress = FALSE
            )
        },
        .default = NULL
    )
    testthat::skip_if(purrr::is_null(modis09a1_cube),
                      message = "MPC is not accessible"
    )
    expect_true(all(sits_bands(modis09a1_cube) %in% c("BLUE", "RED", "GREEN")))
    bbox <- sits_bbox(modis09a1_cube, as_crs = "EPSG:4326")
    bbox_shp <- sf::st_bbox(sf_mt)
    expect_lt(bbox["xmin"], bbox_shp["xmin"])
    expect_lt(bbox["ymin"], bbox_shp["ymin"])
    expect_gt(bbox["xmax"], bbox_shp["xmax"])
    expect_gt(bbox["ymax"], bbox_shp["ymax"])
    intersects <- .cube_intersects(modis09a1_cube, sf_mt)
    expect_true(all(intersects))

    tile_h13v10 <- .cube_filter_tiles(modis09a1_cube, "h13v10")
    expect_equal(nrow(tile_h13v10), 1)

})
test_that("Creating cubes from MPC - MOD10A1-6.1 based on ROI using sf object", {
    shp_file <- system.file(
        "extdata/shapefiles/switzerland/ch.shp",
        package = "sits"
    )
    sf_ch <- sf::read_sf(shp_file)
    # create a raster cube file based on the information about the files
    modis10a1_cube <- .try(
        {
            sits_cube(
                source = "MPC",
                collection = "MOD10A1-6.1",
                bands = c("SNOW", "ALBEDO"),
                roi = sf_ch,
                start_date = "2018-11-01",
                end_date = "2019-03-30",
                progress = FALSE
            )
        },
        .default = NULL
    )
    testthat::skip_if(purrr::is_null(modis10a1_cube),
                      message = "MPC is not accessible"
    )
    expect_true(all(sits_bands(modis10a1_cube) %in% c("SNOW", "ALBEDO")))
    bbox <- sits_bbox(modis10a1_cube, as_crs = "EPSG:4326")
    bbox_shp <- sf::st_bbox(sf_ch)
    expect_lt(bbox["xmin"], bbox_shp["xmin"])
    expect_lt(bbox["ymin"], bbox_shp["ymin"])
    expect_gt(bbox["xmax"], bbox_shp["xmax"])
    expect_gt(bbox["ymax"], bbox_shp["ymax"])
    intersects <- .cube_intersects(modis10a1_cube, sf_ch)
    expect_true(all(intersects))


    tile_h18v4 <- .cube_filter_tiles(modis10a1_cube, "h18v4")
    expect_equal(nrow(tile_h18v4), 1)

})
test_that("Accessing COP-DEM-30 from MPC",{
    cube_dem <-  sits_cube(
        source = "MPC",
        collection = "COP-DEM-GLO-30",
        bands = "ELEVATION",
        tiles = c("22LBL")
    )
    expect_equal(nrow(cube_dem), 4)
    expect_equal(cube_dem$collection, rep("COP-DEM-GLO-30", 4))
    expect_equal(min(cube_dem$xmin), -54, tolerance = 0.01)
    expect_equal(max(cube_dem$xmax), -52, tolerance = 0.01)
    expect_equal(min(cube_dem$ymin), -14, tolerance = 0.01)
    expect_equal(max(cube_dem$ymax), -12, tolerance = 0.01)

    output_dir <- paste0(tempdir(), "/dem")
    if (!dir.exists(output_dir)) {
        dir.create(output_dir)
    }

    cube_dem_reg <-  sits_regularize(
        cube = cube_dem,
        tiles = c("22LBL"),
        res = 100,
        memsize = 12,
        multicores = 6,
        output_dir = output_dir
    )

    cube_s2 <-  sits_cube(
        source = "MPC",
        collection = "SENTINEL-2-L2A",
        bands = c("B02", "B8A", "B11"),
        tiles = c("22LBL"),
        start_date = "2021-07-01",
        end_date = "2021-09-30"
    )
    bbox_dem <- sits_bbox(cube_dem_reg)
    bbox_s2 <- sits_bbox(cube_s2)
    expect_equal(bbox_dem$xmin, bbox_s2$xmin)
    expect_equal(bbox_dem$ymin, bbox_s2$ymin)
    expect_equal(bbox_dem$xmax, bbox_s2$xmax)
    expect_equal(bbox_dem$ymax, bbox_s2$ymax)

})
