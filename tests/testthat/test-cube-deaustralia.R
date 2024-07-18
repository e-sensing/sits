test_that("Creating LS5-SR cubes from DEAustralia", {
    landsat_cube <- .try(
        {
            sits_cube(
                source = "DEAUSTRALIA",
                collection = "LS5-SR",
                bands = c("B05", "CLOUD"),
                roi   = c(
                    lon_min = 137.15991,
                    lon_max = 138.18467,
                    lat_min = -33.85777,
                    lat_max = -32.56690
                ),
                start_date = "2009-01-01",
                end_date = "2010-01-01",
                progress = FALSE
            )
        },
        .default = NULL
    )

    testthat::skip_if(purrr::is_null(landsat_cube),
                      message = "DEAustralia is not accessible"
    )

    expect_true(all(sits_bands(landsat_cube) %in% c("B05", "CLOUD")))
    expect_equal(nrow(landsat_cube), 8)
    bbox_cube <- sits_bbox(landsat_cube, as_crs = "EPSG:4326")
    bbox_cube_1 <- sits_bbox(.tile(landsat_cube), as_crs = "EPSG:4326")
    expect_true(bbox_cube["xmax"] >= bbox_cube_1["xmax"])
    expect_true(bbox_cube["ymax"] >= bbox_cube_1["ymax"])
    r_obj <- .raster_open_rast(landsat_cube$file_info[[1]]$path[1])
    cube_nrows <- .tile_nrows(landsat_cube)
    expect_true(.raster_nrows(r_obj) == cube_nrows)
})
test_that("Creating LS5-GEOMEDIAN cubes from DEAustralia", {
    landsat_cube <- .try(
        {
            sits_cube(
                source = "DEAUSTRALIA",
                collection = "LS5-GEOMEDIAN",
                bands = c("B05"),
                roi   = c(
                    lon_min = 137.15991,
                    lon_max = 138.18467,
                    lat_min = -33.85777,
                    lat_max = -32.56690
                ),
                start_date = "2009-01-01",
                end_date = "2010-01-01",
                progress = FALSE
            )
        },
        .default = NULL
    )

    testthat::skip_if(purrr::is_null(landsat_cube),
                      message = "DEAustralia is not accessible"
    )

    expect_true(all(sits_bands(landsat_cube) %in% c("B05")))
    expect_equal(nrow(landsat_cube), 5)
    bbox_cube <- sits_bbox(landsat_cube, as_crs = "EPSG:4326")
    bbox_cube_1 <- sits_bbox(.tile(landsat_cube), as_crs = "EPSG:4326")
    expect_true(bbox_cube["xmax"] >= bbox_cube_1["xmax"])
    expect_true(bbox_cube["ymax"] >= bbox_cube_1["ymax"])
    r_obj <- .raster_open_rast(landsat_cube$file_info[[1]]$path[1])
    cube_nrows <- .tile_nrows(landsat_cube)
    expect_true(.raster_nrows(r_obj) == cube_nrows)
})

test_that("Creating LS7-SR cubes from DEAustralia", {
    landsat_cube <- .try(
        {
            sits_cube(
                source = "DEAUSTRALIA",
                collection = "LS7-SR",
                bands = c("B05", "CLOUD"),
                roi   = c(
                    lon_min = 137.15991,
                    lon_max = 138.18467,
                    lat_min = -33.85777,
                    lat_max = -32.56690
                ),
                start_date = "2009-01-01",
                end_date = "2010-01-01",
                progress = FALSE
            )
        },
        .default = NULL
    )

    testthat::skip_if(purrr::is_null(landsat_cube),
                      message = "DEAustralia is not accessible"
    )

    expect_true(all(sits_bands(landsat_cube) %in% c("B05", "CLOUD")))
    expect_equal(nrow(landsat_cube), 8)
    bbox_cube <- sits_bbox(landsat_cube, as_crs = "EPSG:4326")
    bbox_cube_1 <- sits_bbox(.tile(landsat_cube), as_crs = "EPSG:4326")
    expect_true(bbox_cube["xmax"] >= bbox_cube_1["xmax"])
    expect_true(bbox_cube["ymax"] >= bbox_cube_1["ymax"])
    r_obj <- .raster_open_rast(landsat_cube$file_info[[1]]$path[1])
    cube_nrows <- .tile_nrows(landsat_cube)
    expect_true(.raster_nrows(r_obj) == cube_nrows)
})
test_that("Creating LS7-GEOMEDIAN cubes from DEAustralia", {
    landsat_cube <- .try(
        {
            sits_cube(
                source = "DEAUSTRALIA",
                collection = "LS7-GEOMEDIAN",
                bands = c("B05"),
                roi   = c(
                    lon_min = 137.15991,
                    lon_max = 138.18467,
                    lat_min = -33.85777,
                    lat_max = -32.56690
                ),
                start_date = "2009-01-01",
                end_date = "2010-01-01",
                progress = FALSE
            )
        },
        .default = NULL
    )

    testthat::skip_if(purrr::is_null(landsat_cube),
                      message = "DEAustralia is not accessible"
    )

    expect_true(all(sits_bands(landsat_cube) %in% c("B05")))
    expect_equal(nrow(landsat_cube), 5)
    bbox_cube <- sits_bbox(landsat_cube, as_crs = "EPSG:4326")
    bbox_cube_1 <- sits_bbox(.tile(landsat_cube), as_crs = "EPSG:4326")
    expect_true(bbox_cube["xmax"] >= bbox_cube_1["xmax"])
    expect_true(bbox_cube["ymax"] >= bbox_cube_1["ymax"])
    r_obj <- .raster_open_rast(landsat_cube$file_info[[1]]$path[1])
    cube_nrows <- .tile_nrows(landsat_cube)
    expect_true(.raster_nrows(r_obj) == cube_nrows)
})

test_that("Creating LS8-SR cubes from DEAustralia", {
    landsat_cube <- .try(
        {
            sits_cube(
                source = "DEAUSTRALIA",
                collection = "LS8-SR",
                bands = c("B05", "CLOUD"),
                roi   = c(
                    lon_min = 137.15991,
                    lon_max = 138.18467,
                    lat_min = -33.85777,
                    lat_max = -32.56690
                ),
                start_date = "2016-01-01",
                end_date = "2017-01-01",
                progress = FALSE
            )
        },
        .default = NULL
    )

    testthat::skip_if(purrr::is_null(landsat_cube),
                      message = "DEAustralia is not accessible"
    )

    expect_true(all(sits_bands(landsat_cube) %in% c("B05", "CLOUD")))
    expect_equal(nrow(landsat_cube), 8)
    bbox_cube <- sits_bbox(landsat_cube, as_crs = "EPSG:4326")
    bbox_cube_1 <- sits_bbox(.tile(landsat_cube), as_crs = "EPSG:4326")
    expect_true(bbox_cube["xmax"] >= bbox_cube_1["xmax"])
    expect_true(bbox_cube["ymax"] >= bbox_cube_1["ymax"])
    r_obj <- .raster_open_rast(landsat_cube$file_info[[1]]$path[1])
    cube_nrows <- .tile_nrows(landsat_cube)
    expect_true(.raster_nrows(r_obj) == cube_nrows)
})
test_that("Creating LS8-GEOMEDIAN cubes from DEAustralia", {
    landsat_cube <- .try(
        {
            sits_cube(
                source = "DEAUSTRALIA",
                collection = "LS8-GEOMEDIAN",
                bands = c("B05"),
                roi   = c(
                    lon_min = 137.15991,
                    lon_max = 138.18467,
                    lat_min = -33.85777,
                    lat_max = -32.56690
                ),
                start_date = "2016-01-01",
                end_date = "2017-01-01",
                progress = FALSE
            )
        },
        .default = NULL
    )

    testthat::skip_if(purrr::is_null(landsat_cube),
                      message = "DEAustralia is not accessible"
    )

    expect_true(all(sits_bands(landsat_cube) %in% c("B05")))
    expect_equal(nrow(landsat_cube), 5)
    bbox_cube <- sits_bbox(landsat_cube, as_crs = "EPSG:4326")
    bbox_cube_1 <- sits_bbox(.tile(landsat_cube), as_crs = "EPSG:4326")
    expect_true(bbox_cube["xmax"] >= bbox_cube_1["xmax"])
    expect_true(bbox_cube["ymax"] >= bbox_cube_1["ymax"])
    r_obj <- .raster_open_rast(landsat_cube$file_info[[1]]$path[1])
    cube_nrows <- .tile_nrows(landsat_cube)
    expect_true(.raster_nrows(r_obj) == cube_nrows)
})

test_that("Creating LS9-SR cubes from DEAustralia", {
    landsat_cube <- .try(
        {
            sits_cube(
                source = "DEAUSTRALIA",
                collection = "LS9-SR",
                bands = c("B05", "CLOUD"),
                roi   = c(
                    lon_min = 137.15991,
                    lon_max = 138.18467,
                    lat_min = -33.85777,
                    lat_max = -32.56690
                ),
                start_date = "2023-01-01",
                end_date = "2024-01-01",
                progress = FALSE
            )
        },
        .default = NULL
    )

    testthat::skip_if(purrr::is_null(landsat_cube),
                      message = "DEAustralia is not accessible"
    )

    expect_true(all(sits_bands(landsat_cube) %in% c("B05", "CLOUD")))
    expect_equal(nrow(landsat_cube), 8)
    bbox_cube <- sits_bbox(landsat_cube, as_crs = "EPSG:4326")
    bbox_cube_1 <- sits_bbox(.tile(landsat_cube), as_crs = "EPSG:4326")
    expect_true(bbox_cube["xmax"] >= bbox_cube_1["xmax"])
    expect_true(bbox_cube["ymax"] >= bbox_cube_1["ymax"])
    r_obj <- .raster_open_rast(landsat_cube$file_info[[1]]$path[1])
    cube_nrows <- .tile_nrows(landsat_cube)
    expect_true(.raster_nrows(r_obj) == cube_nrows)
})

test_that("Creating S2A cubes from DEAustralia using ROI", {
    # try to create a DEA cube
    sentinel_cube <- .try(
        {
            sits_cube(
                source = "DEAUSTRALIA",
                collection = "SENTINEL-2A",
                bands = c("B01", "B04", "B05"),
                roi   = c(
                    lon_min = 137.15991,
                    lon_max = 138.18467,
                    lat_min = -33.85777,
                    lat_max = -32.56690
                ),
                start_date = "2019-01-01",
                end_date = "2019-12-31",
                progress = FALSE
            )
        },
        .default = NULL
    )

    testthat::skip_if(purrr::is_null(sentinel_cube),
                      message = "DEAUSTRALIA is not accessible"
    )

    expect_true(all(sits_bands(sentinel_cube) %in% c("B01", "B04", "B05")))
    expect_equal(nrow(sentinel_cube), 9)
    r <- .raster_open_rast(.tile_path(sentinel_cube))
    expect_equal(sentinel_cube$xmax[[1]], .raster_xmax(r), tolerance = 1)
    expect_equal(sentinel_cube$xmin[[1]], .raster_xmin(r), tolerance = 1)
})
test_that("Creating S2A cubes from DEAustralia using tiles", {
    sentinel_cube <- .try(
        {
            sits_cube(
                source = "DEAUSTRALIA",
                collection = "SENTINEL-2A",
                bands = c("B02", "B8A", "B11"),
                tiles = c("53HQE","53HPE"),
                start_date = "2019-01-01",
                end_date = "2019-08-28",
                progress = FALSE
            )
        },
        .default = NULL
    )

    testthat::skip_if(purrr::is_null(sentinel_cube),
                      message = "DEAustralia is not accessible"
    )

    expect_true(all(sits_bands(sentinel_cube) %in% c("B02", "B8A", "B11")))
    expect_equal(nrow(sentinel_cube), 2)
    r <- .raster_open_rast(.tile_path(sentinel_cube))
    expect_equal(sentinel_cube$xmax[[1]], .raster_xmax(r), tolerance = 1)
    expect_equal(sentinel_cube$xmin[[1]], .raster_xmin(r), tolerance = 1)
    expect_true(all(sentinel_cube$tile %in% c("53HQE","53HPE")))
})

test_that("Creating S2B cubes from DEAustralia using ROI", {
    # try to create a DEA cube
    sentinel_cube <- .try(
        {
            sits_cube(
                source = "DEAUSTRALIA",
                collection = "SENTINEL-2B",
                bands = c("B01", "B04", "B05"),
                roi   = c(
                    lon_min = 137.15991,
                    lon_max = 138.18467,
                    lat_min = -33.85777,
                    lat_max = -32.56690
                ),
                start_date = "2019-01-01",
                end_date = "2019-12-31",
                progress = FALSE
            )
        },
        .default = NULL
    )

    testthat::skip_if(purrr::is_null(sentinel_cube),
                      message = "DEAUSTRALIA is not accessible"
    )

    expect_true(all(sits_bands(sentinel_cube) %in% c("B01", "B04", "B05")))
    expect_equal(nrow(sentinel_cube), 9)
    r <- .raster_open_rast(.tile_path(sentinel_cube))
    expect_equal(sentinel_cube$xmax[[1]], .raster_xmax(r), tolerance = 1)
    expect_equal(sentinel_cube$xmin[[1]], .raster_xmin(r), tolerance = 1)
})
test_that("Creating S2B cubes from DEAustralia using tiles", {
    sentinel_cube <- .try(
        {
            sits_cube(
                source = "DEAUSTRALIA",
                collection = "SENTINEL-2B",
                bands = c("B02", "B8A", "B11"),
                tiles = c("53HQE","53HPE"),
                start_date = "2019-01-01",
                end_date = "2019-08-28",
                progress = FALSE
            )
        },
        .default = NULL
    )

    testthat::skip_if(purrr::is_null(sentinel_cube),
                      message = "DEAustralia is not accessible"
    )

    expect_true(all(sits_bands(sentinel_cube) %in% c("B02", "B8A", "B11")))
    expect_equal(nrow(sentinel_cube), 2)
    r <- .raster_open_rast(.tile_path(sentinel_cube))
    expect_equal(sentinel_cube$xmax[[1]], .raster_xmax(r), tolerance = 1)
    expect_equal(sentinel_cube$xmin[[1]], .raster_xmin(r), tolerance = 1)
    expect_true(all(sentinel_cube$tile %in% c("53HQE","53HPE")))
})

test_that("Creating S2A/S2B cubes from DEAustralia using tiles", {
    s2a_cube <- .try(
        {
            sits_cube(
                source = "DEAUSTRALIA",
                collection = "SENTINEL-2A",
                bands = c("B02", "B8A"),
                tiles = c("53HQE","53HPE"),
                start_date = "2019-01-01",
                end_date = "2019-08-28",
                progress = FALSE
            )
        },
        .default = NULL
    )

    s2b_cube <- .try(
        {
            sits_cube(
                source = "DEAUSTRALIA",
                collection = "SENTINEL-2B",
                bands = c("B11"),
                tiles = c("53HQE","53HPE"),
                start_date = "2019-01-01",
                end_date = "2019-08-28",
                progress = FALSE
            )
        },
        .default = NULL
    )

    testthat::skip_if(purrr::is_null(c(s2a_cube, s2b_cube)),
                      message = "DEAustralia is not accessible"
    )

    sentinel_cube <- sits_merge(s2a_cube, s2b_cube)

    expect_true(all(sits_bands(sentinel_cube) %in% c("B02", "B8A", "B11")))
    expect_equal(nrow(sentinel_cube), 2)
    r <- .raster_open_rast(.tile_path(sentinel_cube))
    expect_equal(sentinel_cube$xmax[[1]], .raster_xmax(r), tolerance = 1)
    expect_equal(sentinel_cube$xmin[[1]], .raster_xmin(r), tolerance = 1)
    expect_true(all(sentinel_cube$tile %in% c("53HQE","53HPE")))
})

test_that("Creating LS-FC cubes from DEAustralia", {
    landsat_cube <- .try(
        {
            sits_cube(
                source = "DEAUSTRALIA",
                collection = "LS-FC",
                bands = c("BS", "PV", "NPV"),
                roi   = c(
                    lon_min = 137.15991,
                    lon_max = 138.18467,
                    lat_min = -33.85777,
                    lat_max = -32.56690
                ),
                start_date = "2023-01-01",
                end_date = "2023-06-01",
                progress = FALSE
            )
        },
        .default = NULL
    )

    testthat::skip_if(purrr::is_null(landsat_cube),
                      message = "DEAustralia is not accessible"
    )

    expect_true(all(sits_bands(landsat_cube) %in% c("BS", "PV", "NPV")))
    expect_equal(nrow(landsat_cube), 8)
    bbox_cube <- sits_bbox(landsat_cube, as_crs = "EPSG:4326")
    bbox_cube_1 <- sits_bbox(.tile(landsat_cube), as_crs = "EPSG:4326")
    expect_true(bbox_cube["xmax"] >= bbox_cube_1["xmax"])
    expect_true(bbox_cube["ymax"] >= bbox_cube_1["ymax"])
    r_obj <- .raster_open_rast(landsat_cube$file_info[[1]]$path[1])
    cube_nrows <- .tile_nrows(landsat_cube)
    expect_true(.raster_nrows(r_obj) == cube_nrows)
})
