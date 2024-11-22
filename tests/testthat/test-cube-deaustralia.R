test_that("Creating GA_LS5T_ARD_3 cubes from DEAustralia", {
    landsat_cube <- .try(
        {
            sits_cube(
                source = "DEAUSTRALIA",
                collection = "GA_LS5T_ARD_3",
                bands = c("SWIR-1", "CLOUD"),
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

    expect_true(all(sits_bands(landsat_cube) %in% c("SWIR-1", "CLOUD")))
    expect_equal(nrow(landsat_cube), 8)
    bbox_cube <- sits_bbox(landsat_cube, as_crs = "EPSG:4326")
    bbox_cube_1 <- sits_bbox(.tile(landsat_cube), as_crs = "EPSG:4326")
    expect_true(bbox_cube["xmax"] >= bbox_cube_1["xmax"])
    expect_true(bbox_cube["ymax"] >= bbox_cube_1["ymax"])
    r_obj <- .raster_open_rast(landsat_cube$file_info[[1]]$path[1])
    cube_nrows <- .tile_nrows(landsat_cube)
    expect_true(.raster_nrows(r_obj) == cube_nrows)
})
test_that("Creating GA_LS5T_GM_CYEAR_3 cubes from DEAustralia", {
    landsat_cube <- .try(
        {
            sits_cube(
                source = "DEAUSTRALIA",
                collection = "GA_LS5T_GM_CYEAR_3",
                bands = c("SWIR1"),
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

    expect_true(all(sits_bands(landsat_cube) %in% c("SWIR1")))
    expect_equal(nrow(landsat_cube), 5)
    bbox_cube <- sits_bbox(landsat_cube, as_crs = "EPSG:4326")
    bbox_cube_1 <- sits_bbox(.tile(landsat_cube), as_crs = "EPSG:4326")
    expect_true(bbox_cube["xmax"] >= bbox_cube_1["xmax"])
    expect_true(bbox_cube["ymax"] >= bbox_cube_1["ymax"])
    r_obj <- .raster_open_rast(landsat_cube$file_info[[1]]$path[1])
    cube_nrows <- .tile_nrows(landsat_cube)
    expect_true(.raster_nrows(r_obj) == cube_nrows)
})

test_that("Creating GA_LS7E_ARD_3 cubes from DEAustralia", {
    landsat_cube <- .try(
        {
            sits_cube(
                source = "DEAUSTRALIA",
                collection = "GA_LS7E_ARD_3",
                bands = c("SWIR-1", "CLOUD"),
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

    expect_true(all(sits_bands(landsat_cube) %in% c("SWIR-1", "CLOUD")))
    expect_equal(nrow(landsat_cube), 8)
    bbox_cube <- sits_bbox(landsat_cube, as_crs = "EPSG:4326")
    bbox_cube_1 <- sits_bbox(.tile(landsat_cube), as_crs = "EPSG:4326")
    expect_true(bbox_cube["xmax"] >= bbox_cube_1["xmax"])
    expect_true(bbox_cube["ymax"] >= bbox_cube_1["ymax"])
    r_obj <- .raster_open_rast(landsat_cube$file_info[[1]]$path[1])
    cube_nrows <- .tile_nrows(landsat_cube)
    expect_true(.raster_nrows(r_obj) == cube_nrows)
})
test_that("Creating GA_LS7E_GM_CYEAR_3 cubes from DEAustralia", {
    landsat_cube <- .try(
        {
            sits_cube(
                source = "DEAUSTRALIA",
                collection = "GA_LS7E_GM_CYEAR_3",
                bands = c("SWIR1"),
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

    expect_true(all(sits_bands(landsat_cube) %in% c("SWIR1")))
    expect_equal(nrow(landsat_cube), 5)
    bbox_cube <- sits_bbox(landsat_cube, as_crs = "EPSG:4326")
    bbox_cube_1 <- sits_bbox(.tile(landsat_cube), as_crs = "EPSG:4326")
    expect_true(bbox_cube["xmax"] >= bbox_cube_1["xmax"])
    expect_true(bbox_cube["ymax"] >= bbox_cube_1["ymax"])
    r_obj <- .raster_open_rast(landsat_cube$file_info[[1]]$path[1])
    cube_nrows <- .tile_nrows(landsat_cube)
    expect_true(.raster_nrows(r_obj) == cube_nrows)
})

test_that("Creating GA_LS8C_ARD_3 cubes from DEAustralia", {
    landsat_cube <- .try(
        {
            sits_cube(
                source = "DEAUSTRALIA",
                collection = "GA_LS8C_ARD_3",
                bands = c("NIR", "CLOUD"),
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

    expect_true(all(sits_bands(landsat_cube) %in% c("NIR", "CLOUD")))
    expect_equal(nrow(landsat_cube), 8)
    bbox_cube <- sits_bbox(landsat_cube, as_crs = "EPSG:4326")
    bbox_cube_1 <- sits_bbox(.tile(landsat_cube), as_crs = "EPSG:4326")
    expect_true(bbox_cube["xmax"] >= bbox_cube_1["xmax"])
    expect_true(bbox_cube["ymax"] >= bbox_cube_1["ymax"])
    r_obj <- .raster_open_rast(landsat_cube$file_info[[1]]$path[1])
    cube_nrows <- .tile_nrows(landsat_cube)
    expect_true(.raster_nrows(r_obj) == cube_nrows)
})

test_that("Creating GA_LS9C_ARD_3 cubes from DEAustralia", {
    landsat_cube <- .try(
        {
            sits_cube(
                source = "DEAUSTRALIA",
                collection = "GA_LS9C_ARD_3",
                bands = c("NIR", "CLOUD"),
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

    expect_true(all(sits_bands(landsat_cube) %in% c("NIR", "CLOUD")))
    expect_equal(nrow(landsat_cube), 8)
    bbox_cube <- sits_bbox(landsat_cube, as_crs = "EPSG:4326")
    bbox_cube_1 <- sits_bbox(.tile(landsat_cube), as_crs = "EPSG:4326")
    expect_true(bbox_cube["xmax"] >= bbox_cube_1["xmax"])
    expect_true(bbox_cube["ymax"] >= bbox_cube_1["ymax"])
    r_obj <- .raster_open_rast(landsat_cube$file_info[[1]]$path[1])
    cube_nrows <- .tile_nrows(landsat_cube)
    expect_true(.raster_nrows(r_obj) == cube_nrows)
})

test_that("Creating GA_LS8CLS9C_GM_CYEAR_3 cubes from DEAustralia", {
    landsat_cube <- .try(
        {
            sits_cube(
                source = "DEAUSTRALIA",
                collection = "GA_LS8CLS9C_GM_CYEAR_3",
                bands = c("SWIR1"),
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

    expect_true(all(sits_bands(landsat_cube) %in% c("SWIR1")))
    expect_equal(nrow(landsat_cube), 5)
    bbox_cube <- sits_bbox(landsat_cube, as_crs = "EPSG:4326")
    bbox_cube_1 <- sits_bbox(.tile(landsat_cube), as_crs = "EPSG:4326")
    expect_true(bbox_cube["xmax"] >= bbox_cube_1["xmax"])
    expect_true(bbox_cube["ymax"] >= bbox_cube_1["ymax"])
    r_obj <- .raster_open_rast(landsat_cube$file_info[[1]]$path[1])
    cube_nrows <- .tile_nrows(landsat_cube)
    expect_true(.raster_nrows(r_obj) == cube_nrows)
})

test_that("Creating GA_S2AM_ARD_3 cubes from DEAustralia using ROI", {
    # try to create a DEA cube
    sentinel_cube <- .try(
        {
            sits_cube(
                source = "DEAUSTRALIA",
                collection = "GA_S2AM_ARD_3",
                bands = c(
                    "COASTAL-AEROSOL",
                    "RED",
                    "RED-EDGE-1"
                ),
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

    expect_true(all(sits_bands(sentinel_cube) %in% c(
        "COASTAL-AEROSOL",
        "RED",
        "RED-EDGE-1"
    )))
    expect_equal(nrow(sentinel_cube), 9)
    r <- .raster_open_rast(.tile_path(sentinel_cube))
    expect_equal(sentinel_cube$xmax[[1]], .raster_xmax(r), tolerance = 1)
    expect_equal(sentinel_cube$xmin[[1]], .raster_xmin(r), tolerance = 1)
})
test_that("Creating GA_S2AM_ARD_3 cubes from DEAustralia using tiles", {
    sentinel_cube <- .try(
        {
            sits_cube(
                source = "DEAUSTRALIA",
                collection = "GA_S2AM_ARD_3",
                bands = c("BLUE", "NIR-2", "SWIR-2"),
                tiles = c("53HQE", "53HPE"),
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

    expect_true(all(sits_bands(sentinel_cube) %in% c(
        "BLUE",
        "NIR-2",
        "SWIR-2"
    )))
    expect_equal(nrow(sentinel_cube), 2)
    r <- .raster_open_rast(.tile_path(sentinel_cube))
    expect_equal(sentinel_cube$xmax[[1]], .raster_xmax(r), tolerance = 1)
    expect_equal(sentinel_cube$xmin[[1]], .raster_xmin(r), tolerance = 1)
    expect_true(all(sentinel_cube$tile %in% c("53HQE","53HPE")))
})

test_that("Creating GA_S2BM_ARD_3 cubes from DEAustralia using ROI", {
    # try to create a DEA cube
    sentinel_cube <- .try(
        {
            sits_cube(
                source = "DEAUSTRALIA",
                collection = "GA_S2BM_ARD_3",
                bands = c(
                    "COASTAL-AEROSOL",
                    "RED",
                    "RED-EDGE-1"
                ),
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

    expect_true(all(sits_bands(sentinel_cube) %in% c(
        "COASTAL-AEROSOL",
        "RED",
        "RED-EDGE-1"
    )))
    expect_equal(nrow(sentinel_cube), 9)
    r <- .raster_open_rast(.tile_path(sentinel_cube))
    expect_equal(sentinel_cube$xmax[[1]], .raster_xmax(r), tolerance = 1)
    expect_equal(sentinel_cube$xmin[[1]], .raster_xmin(r), tolerance = 1)
})
test_that("Creating GA_S2BM_ARD_3 cubes from DEAustralia using tiles", {
    sentinel_cube <- .try(
        {
            sits_cube(
                source = "DEAUSTRALIA",
                collection = "GA_S2BM_ARD_3",
                bands = c(
                    "RED-EDGE-1",
                    "NIR-2",
                    "SWIR-2"
                ),
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

    expect_true(all(sits_bands(sentinel_cube) %in% c(
        "RED-EDGE-1", "NIR-2", "SWIR-2"
    )))
    expect_equal(nrow(sentinel_cube), 2)
    r <- .raster_open_rast(.tile_path(sentinel_cube))
    expect_equal(sentinel_cube$xmax[[1]], .raster_xmax(r), tolerance = 1)
    expect_equal(sentinel_cube$xmin[[1]], .raster_xmin(r), tolerance = 1)
    expect_true(all(sentinel_cube$tile %in% c("53HQE","53HPE")))
})

test_that(
    "Creating GA_S2AM_ARD_3/GA_S2BM_ARD_3 cubes from DEAustralia using tiles",
{
    s2a_cube <- .try(
        {
            sits_cube(
                source = "DEAUSTRALIA",
                collection = "ga_s2am_ard_3",
                bands = c("BLUE", "NIR-2"),
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
                collection = "GA_S2BM_ARD_3",
                bands = c("BLUE", "RED"),
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

    expect_true(all(sits_bands(sentinel_cube) %in% c("BLUE")))
    expect_equal(nrow(sentinel_cube), 2)
    r <- .raster_open_rast(.tile_path(sentinel_cube))
    expect_equal(sentinel_cube[["xmax"]][[1]], .raster_xmax(r), tolerance = 1)
    expect_equal(sentinel_cube[["xmin"]][[1]], .raster_xmin(r), tolerance = 1)
    expect_true(all(sentinel_cube[["tile"]] %in% c("53HQE","53HPE")))
})

test_that("Creating GA_LS_FC_3 cubes from DEAustralia", {
    landsat_cube <- .try(
        {
            sits_cube(
                source = "DEAUSTRALIA",
                collection = "GA_LS_FC_3",
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

test_that("Creating GA_S2LS_INTERTIDAL_CYEAR_3 cubes from DEAustralia", {
    intertidal_cube <- .try(
        {
            sits_cube(
                source = "DEAUSTRALIA",
                collection = "GA_S2LS_INTERTIDAL_CYEAR_3",
                bands = c("ELEVATION", "EXPOSURE"),
                roi   = c(
                    lon_min = 137.15991,
                    lon_max = 138.18467,
                    lat_min = -33.85777,
                    lat_max = -32.56690
                ),
                start_date = "2016-01-01",
                end_date = "2019-06-01",
                progress = FALSE
            )
        },
        .default = NULL
    )

    testthat::skip_if(purrr::is_null(intertidal_cube),
                      message = "DEAustralia is not accessible"
    )

    expect_true(all(sits_bands(intertidal_cube) %in% c(
        "ELEVATION", "EXPOSURE"
    )))
    expect_equal(nrow(intertidal_cube), 14)
    bbox_cube <- sits_bbox(intertidal_cube, as_crs = "EPSG:4326")
    bbox_cube_1 <- sits_bbox(.tile(intertidal_cube), as_crs = "EPSG:4326")
    expect_true(bbox_cube["xmax"] >= bbox_cube_1["xmax"])
    expect_true(bbox_cube["ymax"] >= bbox_cube_1["ymax"])
    r_obj <- .raster_open_rast(intertidal_cube$file_info[[1]]$path[1])
    cube_nrows <- .tile_nrows(intertidal_cube)
    expect_true(.raster_nrows(r_obj) == cube_nrows)
})
