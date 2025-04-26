test_that("Creating LS5-SR cubes from DEA", {
    landsat_cube <- .try(
        {
            sits_cube(
                source = "DEAFRICA",
                collection = "LS5-SR",
                bands = c("B05", "CLOUD"),
                roi = c(
                    lon_min = 33.546,
                    lon_max = 34.999,
                    lat_min = 1.427,
                    lat_max = 3.726
                ),
                start_date = "2009-01-01",
                end_date = "2010-01-01",
                progress = FALSE
            )
        },
        .default = NULL
    )

    testthat::skip_if(purrr::is_null(landsat_cube),
        message = "DEAFRICA is not accessible"
    )

    expect_true(all(sits_bands(landsat_cube) %in% c("B05", "CLOUD")))
    expect_equal(nrow(landsat_cube), 6)
    bbox_cube <- sits_bbox(landsat_cube, as_crs = "EPSG:4326")
    bbox_cube_1 <- sits_bbox(.tile(landsat_cube), as_crs = "EPSG:4326")
    expect_true(bbox_cube["xmax"] >= bbox_cube_1["xmax"])
    expect_true(bbox_cube["ymax"] >= bbox_cube_1["ymax"])
    rast <- .raster_open_rast(landsat_cube$file_info[[1]]$path[1])
    cube_nrows <- .tile_nrows(landsat_cube)
    expect_true(.raster_nrows(rast) == cube_nrows)
})

test_that("Creating LS7-SR cubes from DEA", {
    landsat_cube <- .try(
        {
            sits_cube(
                source = "DEAFRICA",
                collection = "LS7-SR",
                bands = c("B05", "CLOUD"),
                roi = c(
                    lon_min = 33.546,
                    lon_max = 34.999,
                    lat_min = 1.427,
                    lat_max = 3.726
                ),
                start_date = "2009-01-01",
                end_date = "2010-01-01",
                progress = FALSE
            )
        },
        .default = NULL
    )

    testthat::skip_if(purrr::is_null(landsat_cube),
        message = "DEAFRICA is not accessible"
    )

    expect_true(all(sits_bands(landsat_cube) %in% c("B05", "CLOUD")))
    expect_equal(nrow(landsat_cube), 6)
    bbox_cube <- sits_bbox(landsat_cube, as_crs = "EPSG:4326")
    bbox_cube_1 <- sits_bbox(.tile(landsat_cube), as_crs = "EPSG:4326")
    expect_true(bbox_cube["xmax"] >= bbox_cube_1["xmax"])
    expect_true(bbox_cube["ymax"] >= bbox_cube_1["ymax"])
    rast <- .raster_open_rast(landsat_cube$file_info[[1]]$path[1])
    cube_nrows <- .tile_nrows(landsat_cube)
    expect_true(.raster_nrows(rast) == cube_nrows)
})

test_that("Creating LS8-SR cubes from DEA", {
    landsat_cube <- .try(
        {
            sits_cube(
                source = "DEAFRICA",
                collection = "LS8-SR",
                bands = c("B05", "CLOUD"),
                roi = c(
                    lon_min = 33.546,
                    lon_max = 34.999,
                    lat_min = 1.427,
                    lat_max = 3.726
                ),
                start_date = "2021-01-01",
                end_date = "2021-04-29",
                progress = FALSE
            )
        },
        .default = NULL
    )

    testthat::skip_if(purrr::is_null(landsat_cube),
        message = "DEAFRICA is not accessible"
    )

    expect_true(all(sits_bands(landsat_cube) %in% c("B05", "CLOUD")))
    expect_equal(nrow(landsat_cube), 6)
    bbox_cube <- sits_bbox(landsat_cube, as_crs = "EPSG:4326")
    bbox_cube_1 <- sits_bbox(.tile(landsat_cube), as_crs = "EPSG:4326")
    expect_true(bbox_cube["xmax"] >= bbox_cube_1["xmax"])
    expect_true(bbox_cube["ymax"] >= bbox_cube_1["ymax"])
    rast <- .raster_open_rast(landsat_cube$file_info[[1]]$path[1])
    cube_nrows <- .tile_nrows(landsat_cube)
    expect_true(.raster_nrows(rast) == cube_nrows)
})

test_that("Creating LS9-SR cubes from DEA", {
    landsat_cube <- .try(
        {
            sits_cube(
                source = "DEAFRICA",
                collection = "LS9-SR",
                bands = c("B05", "CLOUD"),
                roi = c(
                    lon_min = 33.546,
                    lon_max = 34.999,
                    lat_min = 1.427,
                    lat_max = 3.726
                ),
                start_date = "2022-01-01",
                end_date = "2022-04-01",
                progress = FALSE
            )
        },
        .default = NULL
    )

    testthat::skip_if(purrr::is_null(landsat_cube),
        message = "DEAFRICA is not accessible"
    )

    expect_true(all(sits_bands(landsat_cube) %in% c("B05", "CLOUD")))
    expect_equal(nrow(landsat_cube), 6)
    bbox_cube <- sits_bbox(landsat_cube, as_crs = "EPSG:4326")
    bbox_cube_1 <- sits_bbox(.tile(landsat_cube), as_crs = "EPSG:4326")
    expect_true(bbox_cube["xmax"] >= bbox_cube_1["xmax"])
    expect_true(bbox_cube["ymax"] >= bbox_cube_1["ymax"])
    rast <- .raster_open_rast(landsat_cube$file_info[[1]]$path[1])
    cube_nrows <- .tile_nrows(landsat_cube)
    expect_true(.raster_nrows(rast) == cube_nrows)
})

test_that("Creating S2 cubes from DEA using ROI", {
    # try to create a DEA cube
    dea_cube <- .try(
        {
            sits_cube(
                source = "DEAFRICA",
                collection = "SENTINEL-2-L2A",
                bands = c("B01", "B04", "B05"),
                roi = c(
                    lon_min = 17.379,
                    lat_min = 1.1573,
                    lon_max = 17.410,
                    lat_max = 1.1910
                ),
                start_date = "2019-01-01",
                end_date = "2019-10-28",
                progress = FALSE
            )
        },
        .default = NULL
    )

    testthat::skip_if(purrr::is_null(dea_cube),
        message = "DEAFRICA is not accessible"
    )

    expect_true(all(sits_bands(dea_cube) %in% c("B01", "B04", "B05")))
    expect_equal(nrow(dea_cube), 1)
    r <- .raster_open_rast(.tile_path(dea_cube))
    expect_equal(dea_cube$xmax[[1]], .raster_xmax(r), tolerance = 1)
    expect_equal(dea_cube$xmin[[1]], .raster_xmin(r), tolerance = 1)
})

test_that("Creating S2 cubes from DEA using tiles", {
    dea_cube <- .try(
        {
            sits_cube(
                source = "DEAFRICA",
                collection = "SENTINEL-2-L2A",
                bands = c("B02", "B8A", "B11"),
                tiles = c("37MDT", "37MET"),
                start_date = "2019-01-01",
                end_date = "2019-08-28",
                progress = FALSE
            )
        },
        .default = NULL
    )

    testthat::skip_if(purrr::is_null(dea_cube),
        message = "DEAFRICA is not accessible"
    )

    expect_true(all(sits_bands(dea_cube) %in% c("B02", "B8A", "B11")))
    expect_equal(nrow(dea_cube), 2)
    r <- .raster_open_rast(.tile_path(dea_cube))
    expect_equal(dea_cube$xmax[[1]], .raster_xmax(r), tolerance = 1)
    expect_equal(dea_cube$xmin[[1]], .raster_xmin(r), tolerance = 1)
    expect_true(all(dea_cube$tile %in% c("37MDT", "37MET")))
})

test_that("Creating Sentinel-1 RTC cubes from DEA using ROI", {
    cube_s1_rtc <- .try(
        {
            sits_cube(
                source = "DEAFRICA",
                collection = "SENTINEL-1-RTC",
                bands = c("VV"),
                orbit = "descending",
                roi = c(
                    lon_min = 17.379,
                    lat_min = 1.1573,
                    lon_max = 17.410,
                    lat_max = 1.1910
                ),
                start_date = "2021-07-01",
                end_date = "2021-09-30",
                progress = FALSE
            )
        },
        .default = NULL
    )

    testthat::skip_if(purrr::is_null(cube_s1_rtc),
        message = "DEAFRICA is not accessible"
    )

    expect_true(sits_bands(cube_s1_rtc) == "VV")
    expect_equal(nrow(cube_s1_rtc), 1)
    r <- .raster_open_rast(.tile_path(cube_s1_rtc))
    expect_equal(cube_s1_rtc$xmax[[1]], .raster_xmax(r), tolerance = 1)
    expect_equal(cube_s1_rtc$xmin[[1]], .raster_xmin(r), tolerance = 1)
})

test_that("Creating Sentinel-1 RTC cubes from DEA using tiles", {
    cube_s1_rtc <- .try(
        {
            sits_cube(
                source = "DEAFRICA",
                collection = "SENTINEL-1-RTC",
                bands = c("VV"),
                orbit = "ascending",
                tiles = c("36NWJ"),
                start_date = "2022-01-01",
                end_date = "2022-02-25",
                progress = FALSE
            )
        },
        .default = NULL
    )

    testthat::skip_if(purrr::is_null(cube_s1_rtc),
        message = "DEAFRICA is not accessible"
    )

    bbox <- sits_bbox(cube_s1_rtc)
    roi_cube_s1 <- sits_tiles_to_roi(c("36NWJ"))

    expect_true(bbox[["xmin"]] < roi_cube_s1[["xmin"]])
    expect_true(bbox[["xmax"]] > roi_cube_s1[["xmax"]])
    expect_true(bbox[["ymin"]] < roi_cube_s1[["ymin"]])
    expect_true(bbox[["ymax"]] > roi_cube_s1[["ymax"]])
    expect_true(all(c("VV") %in% sits_bands(cube_s1_rtc)))

    rast <- .raster_open_rast(cube_s1_rtc$file_info[[1]]$path[[1]])
    expect_true(.raster_nrows(rast) == cube_s1_rtc$file_info[[1]]$nrows[[1]])

    output_dir <- paste0(tempdir(), "/s1-rtc-reg")
    if (!dir.exists(output_dir)) {
        dir.create(output_dir)
    }

    cube_s1_reg <- sits_regularize(
        cube = cube_s1_rtc,
        period = "P1M",
        res = 240,
        tiles = c("36NWJ"),
        multicores = 1,
        output_dir = output_dir,
        progress = FALSE
    )
    expect_equal(length(sits_timeline(cube_s1_reg)), 2)
    expect_true("36NWJ" %in% cube_s1_reg$tile)
    expect_true(all("EPSG:32636" %in% cube_s1_reg$crs))

    bbox <- sits_bbox(cube_s1_reg, as_crs = "EPSG:4326")
    roi_cube_s1 <- sits_tiles_to_roi("36NWJ")

    expect_equal(bbox[["xmin"]], roi_cube_s1[["xmin"]], tolerance = 0.01)
    expect_equal(bbox[["xmax"]], roi_cube_s1[["xmax"]], tolerance = 0.01)
    expect_equal(bbox[["ymin"]], roi_cube_s1[["ymin"]], tolerance = 0.01)
    expect_equal(bbox[["ymax"]], roi_cube_s1[["ymax"]], tolerance = 0.01)

    expect_true(all(c("VV") %in% sits_bands(cube_s1_reg)))
})

test_that("Creating Landsat-8/9 Geomedian (Annual) from DEA", {
    landsat_cube <- .try(
        {
            sits_cube(
                source = "DEAFRICA",
                collection = "GM-LS8-LS9-ANNUAL",
                bands = c("B05"),
                roi = c(
                    lon_min = 33.546,
                    lon_max = 34.999,
                    lat_min = 1.427,
                    lat_max = 3.726
                ),
                start_date = "2021-01-01",
                end_date = "2022-01-01",
                progress = FALSE
            )
        },
        .default = NULL
    )

    testthat::skip_if(purrr::is_null(landsat_cube),
        message = "DEAFRICA is not accessible"
    )

    expect_true(all(sits_bands(landsat_cube) %in% c("B05")))
    expect_equal(nrow(landsat_cube), 12)
    bbox_cube <- sits_bbox(landsat_cube, as_crs = "EPSG:4326")
    bbox_cube_1 <- sits_bbox(.tile(landsat_cube), as_crs = "EPSG:4326")
    expect_true(bbox_cube["xmax"] >= bbox_cube_1["xmax"])
    expect_true(bbox_cube["ymax"] >= bbox_cube_1["ymax"])
    rast <- .raster_open_rast(landsat_cube$file_info[[1]]$path[1])
    cube_nrows <- .tile_nrows(landsat_cube)
    expect_true(.raster_nrows(rast) == cube_nrows)
})
test_that("Creating Sentinel-2 Geomedian (Annual) from DEA", {
    sentinel_cube <- .try(
        {
            sits_cube(
                source = "DEAFRICA",
                collection = "GM-S2-ANNUAL",
                bands = c("B05"),
                roi = c(
                    lon_min = 33.546,
                    lon_max = 34.999,
                    lat_min = 1.427,
                    lat_max = 3.726
                ),
                start_date = "2021-01-01",
                end_date = "2022-01-01",
                progress = FALSE
            )
        },
        .default = NULL
    )

    testthat::skip_if(purrr::is_null(sentinel_cube),
        message = "DEAFRICA is not accessible"
    )

    expect_true(all(sits_bands(sentinel_cube) %in% c("B05")))
    expect_equal(nrow(sentinel_cube), 12)
    bbox_cube <- sits_bbox(sentinel_cube, as_crs = "EPSG:4326")
    bbox_cube_1 <- sits_bbox(.tile(sentinel_cube), as_crs = "EPSG:4326")
    expect_true(bbox_cube["xmax"] >= bbox_cube_1["xmax"])
    expect_true(bbox_cube["ymax"] >= bbox_cube_1["ymax"])
    rast <- .raster_open_rast(sentinel_cube$file_info[[1]]$path[1])
    cube_nrows <- .tile_nrows(sentinel_cube)
    expect_true(.raster_nrows(rast) == cube_nrows)
})
test_that("Creating Sentinel-2 Geomedian (Semiannual) from DEA", {
    sentinel_cube <- .try(
        {
            sits_cube(
                source = "DEAFRICA",
                collection = "GM-S2-ANNUAL",
                bands = c("B05"),
                roi = c(
                    lon_min = 33.546,
                    lon_max = 34.999,
                    lat_min = 1.427,
                    lat_max = 3.726
                ),
                start_date = "2021-01-01",
                end_date = "2022-01-01",
                progress = FALSE
            )
        },
        .default = NULL
    )

    testthat::skip_if(purrr::is_null(sentinel_cube),
        message = "DEAFRICA is not accessible"
    )

    expect_true(all(sits_bands(sentinel_cube) %in% c("B05")))
    expect_equal(nrow(sentinel_cube), 12)
    bbox_cube <- sits_bbox(sentinel_cube, as_crs = "EPSG:4326")
    bbox_cube_1 <- sits_bbox(.tile(sentinel_cube), as_crs = "EPSG:4326")
    expect_true(bbox_cube["xmax"] >= bbox_cube_1["xmax"])
    expect_true(bbox_cube["ymax"] >= bbox_cube_1["ymax"])
    rast <- .raster_open_rast(sentinel_cube$file_info[[1]]$path[1])
    cube_nrows <- .tile_nrows(sentinel_cube)
    expect_true(.raster_nrows(rast) == cube_nrows)
})
test_that("Creating Sentinel-2 Geomedian (Rolling) from DEA", {
    sentinel_cube <- .try(
        {
            sits_cube(
                source = "DEAFRICA",
                collection = "GM-S2-ROLLING",
                bands = c("B05", "B8A"),
                roi = c(
                    lon_min = 33.546,
                    lon_max = 34.999,
                    lat_min = 1.427,
                    lat_max = 3.726
                ),
                start_date = "2021-01-01",
                end_date = "2022-01-01",
                progress = FALSE
            )
        },
        .default = NULL
    )

    testthat::skip_if(purrr::is_null(sentinel_cube),
        message = "DEAFRICA is not accessible"
    )

    expect_true(all(sits_bands(sentinel_cube) %in% c("B05", "B8A")))
    expect_equal(nrow(sentinel_cube), 12)
    bbox_cube <- sits_bbox(sentinel_cube, as_crs = "EPSG:4326")
    bbox_cube_1 <- sits_bbox(.tile(sentinel_cube), as_crs = "EPSG:4326")
    expect_true(bbox_cube["xmax"] >= bbox_cube_1["xmax"])
    expect_true(bbox_cube["ymax"] >= bbox_cube_1["ymax"])
    rast <- .raster_open_rast(sentinel_cube$file_info[[1]]$path[1])
    cube_nrows <- .tile_nrows(sentinel_cube)
    expect_true(.raster_nrows(rast) == cube_nrows)
})

test_that("Creating ALOS-PALSAR-MOSAIC cubes from DEA", {
    cube_alos <- .try(
        {
            sits_cube(
                source = "DEAFRICA",
                collection = "ALOS-PALSAR-MOSAIC",
                bands = c("HH", "HV", "CLOUD"),
                roi = c(
                    lon_min = 17.379,
                    lat_min = 1.1573,
                    lon_max = 17.410,
                    lat_max = 1.1910
                ),
                start_date = "2008-01-01",
                end_date = "2009-01-01",
                progress = FALSE
            )
        },
        .default = NULL
    )

    testthat::skip_if(purrr::is_null(cube_alos),
        message = "DEAFRICA is not accessible"
    )

    expect_true(all(sits_bands(cube_alos) %in% c("HH", "HV", "CLOUD")))
    expect_equal(nrow(cube_alos), 1)
    bbox_cube <- sits_bbox(cube_alos, as_crs = "EPSG:4326")
    bbox_cube_1 <- sits_bbox(.tile(cube_alos), as_crs = "EPSG:4326")
    expect_true(bbox_cube["xmax"] >= bbox_cube_1["xmax"])
    expect_true(bbox_cube["ymax"] >= bbox_cube_1["ymax"])
    rast <- .raster_open_rast(cube_alos$file_info[[1]]$path[1])
    cube_nrows <- .tile_nrows(cube_alos)
    expect_true(.raster_nrows(rast) == cube_nrows)
})

test_that("Creating NDVI-ANOMALY cubes from DEA", {
    cube_ndvi <- .try(
        {
            sits_cube(
                source = "DEAFRICA",
                collection = "NDVI-ANOMALY",
                bands = c("NDVI-MEAN"),
                roi = c(
                    lon_min = 17.379,
                    lat_min = 1.1573,
                    lon_max = 17.410,
                    lat_max = 1.1910
                ),
                start_date = "2018-06-01",
                end_date = "2018-12-31",
                progress = FALSE
            )
        },
        .default = NULL
    )

    testthat::skip_if(purrr::is_null(cube_ndvi),
        message = "DEAFRICA is not accessible"
    )

    expect_true(sits_bands(cube_ndvi) == "NDVI-MEAN")
    expect_equal(nrow(cube_ndvi), 1)
    bbox_cube <- sits_bbox(cube_ndvi, as_crs = "EPSG:4326")
    bbox_cube_1 <- sits_bbox(.tile(cube_ndvi), as_crs = "EPSG:4326")
    expect_true(bbox_cube["xmax"] >= bbox_cube_1["xmax"])
    expect_true(bbox_cube["ymax"] >= bbox_cube_1["ymax"])
    rast <- .raster_open_rast(cube_ndvi$file_info[[1]]$path[1])
    cube_nrows <- .tile_nrows(cube_ndvi)
    expect_true(.raster_nrows(rast) == cube_nrows)
})

test_that("Creating RAINFALL-CHIRPS-DAILY cubes from DEA", {
    cube_chirps <- .try(
        {
            sits_cube(
                source = "DEAFRICA",
                collection = "RAINFALL-CHIRPS-DAILY",
                bands = c("RAINFALL"),
                roi = c(
                    lon_min = 17.379,
                    lat_min = 1.1573,
                    lon_max = 17.410,
                    lat_max = 1.1910
                ),
                start_date = "2018-06-01",
                end_date = "2018-08-25",
                progress = FALSE
            )
        },
        .default = NULL
    )

    testthat::skip_if(purrr::is_null(cube_chirps),
        message = "DEAFRICA is not accessible"
    )

    expect_true(sits_bands(cube_chirps) == "RAINFALL")
    expect_equal(nrow(cube_chirps), 1)
    bbox_cube <- sits_bbox(cube_chirps, as_crs = "EPSG:4326")
    bbox_cube_1 <- sits_bbox(.tile(cube_chirps), as_crs = "EPSG:4326")
    expect_true(bbox_cube["xmax"] >= bbox_cube_1["xmax"])
    expect_true(bbox_cube["ymax"] >= bbox_cube_1["ymax"])
    rast <- .raster_open_rast(cube_chirps$file_info[[1]]$path[1])
    cube_nrows <- .tile_nrows(cube_chirps)
    expect_true(.raster_nrows(rast) == cube_nrows)
})

test_that("Creating RAINFALL-CHIRPS-MONTHLY cubes from DEA", {
    cube_chirps <- .try(
        {
            sits_cube(
                source = "DEAFRICA",
                collection = "RAINFALL-CHIRPS-MONTHLY",
                bands = c("RAINFALL"),
                roi = c(
                    lon_min = 17.379,
                    lat_min = 1.1573,
                    lon_max = 17.410,
                    lat_max = 1.1910
                ),
                start_date = "2018-06-01",
                end_date = "2018-08-25",
                progress = FALSE
            )
        },
        .default = NULL
    )

    testthat::skip_if(purrr::is_null(cube_chirps),
        message = "DEAFRICA is not accessible"
    )

    expect_true(sits_bands(cube_chirps) == "RAINFALL")
    expect_equal(nrow(cube_chirps), 1)
    bbox_cube <- sits_bbox(cube_chirps, as_crs = "EPSG:4326")
    bbox_cube_1 <- sits_bbox(.tile(cube_chirps), as_crs = "EPSG:4326")
    expect_true(bbox_cube["xmax"] >= bbox_cube_1["xmax"])
    expect_true(bbox_cube["ymax"] >= bbox_cube_1["ymax"])
    rast <- .raster_open_rast(cube_chirps$file_info[[1]]$path[1])
    cube_nrows <- .tile_nrows(cube_chirps)
    expect_true(.raster_nrows(rast) == cube_nrows)
})

test_that("Creating DEM-COP-30 cubes from DEA", {
    cube_dem <- .try(
        {
            sits_cube(
                source = "DEAFRICA",
                collection = "DEM-COP-30",
                bands = c("ELEVATION"),
                roi = c(
                    lon_min = 17.379,
                    lat_min = 1.1573,
                    lon_max = 17.410,
                    lat_max = 1.1910
                ),
                progress = FALSE
            )
        },
        .default = NULL
    )

    testthat::skip_if(purrr::is_null(cube_dem),
        message = "DEAFRICA is not accessible"
    )

    expect_true(sits_bands(cube_dem) == "ELEVATION")
    expect_equal(nrow(cube_dem), 1)
    bbox_cube <- sits_bbox(cube_dem, as_crs = "EPSG:4326")
    bbox_cube_1 <- sits_bbox(.tile(cube_dem), as_crs = "EPSG:4326")
    expect_true(bbox_cube["xmax"] >= bbox_cube_1["xmax"])
    expect_true(bbox_cube["ymax"] >= bbox_cube_1["ymax"])
    rast <- .raster_open_rast(cube_dem$file_info[[1]]$path[1])
    cube_nrows <- .tile_nrows(cube_dem)
    expect_true(.raster_nrows(rast) == cube_nrows)
})
