test_that("Creating LANDSAT cubes from USGS with ROI", {
    roi <- c(
        lon_min = -48.28579, lat_min = -16.05026,
        lon_max = -47.30839, lat_max = -15.50026
    )
    l8_cube_usgs <- .try(
        {
            sits_cube(
                source = "USGS",
                collection = "LANDSAT-C2L2-SR",
                roi = roi,
                bands = c("NIR08", "CLOUD"),
                start_date = as.Date("2018-07-18"),
                end_date = as.Date("2018-08-23"),
                progress = FALSE
            )
        },
        .default = NULL
    )
    testthat::skip_if(purrr::is_null(l8_cube_usgs), "USGS is not accessible")
    expect_true(all(sits_bands(l8_cube_usgs) %in% c("NIR08", "CLOUD")))
    expect_equal(nrow(l8_cube_usgs), 2)
    bbox_cube <- sits_bbox(l8_cube_usgs, as_crs = "EPSG:4326")
    bbox_cube_1 <- sits_bbox(.tile(l8_cube_usgs), as_crs = "EPSG:4326")
    expect_true(bbox_cube["xmax"] >= bbox_cube_1["xmax"])
    expect_true(bbox_cube["ymax"] >= bbox_cube_1["ymax"])
    rast <- .raster_open_rast(l8_cube_usgs$file_info[[1]]$path[1])
    tile_nrows <- .tile_nrows(l8_cube_usgs)[[1]]
    expect_true(.raster_nrows(rast) == tile_nrows)
})

test_that("Creating LANDSAT cubes from USGS with WRS", {
    l8_cube_223067 <- .try(
        {
            sits_cube(
                source = "USGS",
                collection = "LANDSAT-C2L2-SR",
                tiles = "223067",
                bands = c("NIR08"),
                start_date = as.Date("2018-07-18"),
                end_date = as.Date("2018-08-23"),
                platform = "LANDSAT-8",
                progress = FALSE
            )
        },
        .default = NULL
    )
    testthat::skip_if(purrr::is_null(l8_cube_223067), "USGS is not accessible")
    expect_true(all(sits_bands(l8_cube_223067) %in% c("NIR08")))
    expect_equal(nrow(l8_cube_223067), 1)
    rast <- .raster_open_rast(l8_cube_223067$file_info[[1]]$path[1])
    tile_nrows <- .tile_nrows(l8_cube_223067)[[1]]
    expect_true(.raster_nrows(rast) == tile_nrows)
})
