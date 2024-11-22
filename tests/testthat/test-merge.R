test_that("sits_merge - irregular cubes with same bands and tile", {
    # Test case: If the bands are the same, the cube will have the combined
    # timeline of both cubes. This is useful to merge data from the same sensors
    # from different satellites (e.g, Sentinel-2A with Sentinel-2B).
    # For irregular cubes, all dates are returned.

    # Test 1a: Single tile with different time period (irregular cube)
    s2a_cube <- .try(
        {
            sits_cube(
                source = "DEAUSTRALIA",
                collection = "ga_s2am_ard_3",
                bands = c("BLUE"),
                tiles = c("53HQE"),
                start_date = "2019-01-01",
                end_date = "2019-04-01",
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
                bands = c("BLUE"),
                tiles = c("53HQE"),
                start_date = "2019-04-01",
                end_date = "2019-06-10",
                progress = FALSE
            )
        },
        .default = NULL
    )

    testthat::skip_if(purrr::is_null(c(s2a_cube, s2b_cube)),
                      message = "DEAustralia is not accessible"
    )

    merged_cube <- sits_merge(s2a_cube, s2b_cube)

    expect_equal(nrow(merged_cube), 1)
    expect_equal(sits_bands(merged_cube), "BLUE")
    expect_equal(
        length(sits_timeline(merged_cube)),
        length(sits_timeline(s2a_cube)) + length(sits_timeline(s2b_cube))
    )

    r <- .raster_open_rast(.tile_path(merged_cube))
    expect_equal(merged_cube[["xmax"]][[1]], .raster_xmax(r), tolerance = 1)
    expect_equal(merged_cube[["xmin"]][[1]], .raster_xmin(r), tolerance = 1)

    # Test 1b: Single tile with different time period (irregular cube)
    s2_cube <- .try(
        {
            sits_cube(
                source = "DEAFRICA",
                collection = "SENTINEL-2-L2A",
                bands = c("B02"),
                tiles = c("36NWJ"),
                start_date = "2019-01-01",
                end_date = "2019-04-01",
                progress = FALSE
            )
        },
        .default = NULL
    )

    s1_cube <- .try(
        {
            sits_cube(
                source = "DEAFRICA",
                collection = "SENTINEL-1-RTC",
                bands = c("VV"),
                orbit = "ascending",
                tiles = c("36NWJ"),
                start_date = "2019-04-01",
                end_date = "2019-06-10",
                progress = FALSE
            )
        },
        .default = NULL
    )

    testthat::skip_if(purrr::is_null(c(s2_cube, s1_cube)),
                      message = "DEAFRICA is not accessible"
    )

    merged_cube <- sits_merge(s2_cube, s1_cube)

    expect_true(inherits(merged_cube, "combined_cube"))
    expect_equal(
        length(merged_cube[["tile"]]),
        length(s2_cube[["tile"]]) + length(s1_cube[["tile"]])
    )
    expect_equal(suppressWarnings(length(sits_timeline(merged_cube))), 5)
    expect_equal(
        unique(slider::slide_chr(merged_cube, .tile_bands)), c("B02", "VV")
    )

    # Test 2: Multiple tiles with different time period (irregular cube)
    s2a_cube <- .try(
        {
            sits_cube(
                source = "DEAUSTRALIA",
                collection = "ga_s2am_ard_3",
                bands = c("BLUE"),
                tiles = c("53HQE", "53HPE"),
                start_date = "2019-01-01",
                end_date = "2019-07-10",
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
                bands = c("BLUE"),
                tiles = c("53HQE", "53HPE"),
                start_date = "2019-01-01",
                end_date = "2019-07-10",
                progress = FALSE
            )
        },
        .default = NULL
    )

    testthat::skip_if(purrr::is_null(c(s2a_cube, s2b_cube)),
                      message = "DEAustralia is not accessible"
    )

    merged_cube <- sits_merge(s2a_cube, s2b_cube)

    expect_equal(nrow(merged_cube), 2)
    expect_equal(sits_bands(merged_cube), "BLUE")
    expect_equal(
        length(sits_timeline(merged_cube)),
        length(sits_timeline(s2a_cube)) + length(sits_timeline(s2b_cube))
    )
    r <- .raster_open_rast(.tile_path(merged_cube))
    expect_equal(merged_cube[["xmax"]][[1]], .raster_xmax(r), tolerance = 1)
    expect_equal(merged_cube[["xmin"]][[1]], .raster_xmin(r), tolerance = 1)
})

test_that("sits_merge - irregular cubes with same bands and different tile", {
    # Test case: If the bands are the same, the cube will have the combined
    # timeline of both cubes. This is useful to merge data from the same sensors
    # from different satellites (e.g, Sentinel-2A with Sentinel-2B).
    # For irregular cubes, all dates are returned.

    # Test 1: Different tiles with different time period (irregular cube)
    s2a_cube <- .try(
        {
            sits_cube(
                source = "DEAUSTRALIA",
                collection = "ga_s2am_ard_3",
                bands = c("BLUE"),
                tiles = c("53HQE"),
                start_date = "2019-01-01",
                end_date = "2019-04-01",
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
                bands = c("BLUE"),
                tiles = c("53JQF"),
                start_date = "2019-04-01",
                end_date = "2019-06-10",
                progress = FALSE
            )
        },
        .default = NULL
    )

    testthat::skip_if(purrr::is_null(c(s2a_cube, s2b_cube)),
                      message = "DEAustralia is not accessible"
    )

    merged_cube <- sits_merge(s2a_cube, s2b_cube)

    expect_true(inherits(merged_cube, "combined_cube"))
    expect_equal(suppressWarnings(length(sits_timeline(merged_cube))), 2)
})

test_that("sits_merge - irregular cubes with different bands and tile", {
    # Test case: if the bands are different and their timelines should be
    # compatible, the bands are joined. The resulting timeline is the one from
    # the first cube. This is useful to merge data from different sensors
    # (e.g, Sentinel-1 with Sentinel-2).
    # For irregular cubes, all dates are returned.

    s2a_cube <- .try(
        {
            sits_cube(
                source = "DEAUSTRALIA",
                collection = "ga_s2am_ard_3",
                bands = c("BLUE", "RED"),
                tiles = c("53HQE"),
                start_date = "2019-01-01",
                end_date = "2019-04-01",
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
                bands = c("BLUE"),
                tiles = c("53HQE", "53JQF"),
                start_date = "2019-04-01",
                end_date = "2019-06-10",
                progress = FALSE
            )
        },
        .default = NULL
    )

    testthat::skip_if(purrr::is_null(c(s2a_cube, s2b_cube)),
                      message = "DEAustralia is not accessible"
    )

    merged_cube <- sits_merge(s2a_cube, s2b_cube)
    expect_equal(sits_bands(merged_cube), "BLUE")
    expect_equal(merged_cube[["tile"]], "53HQE")
})

test_that("sits_merge - regular cubes with same bands and tile", {
    # Test case: If the bands are the same, the cube will have the combined
    # timeline of both cubes. This is useful to merge data from the same sensors
    # from different satellites (e.g, Sentinel-2A with Sentinel-2B).
    # For regular cubes, when timeline has the same length, use them. Otherwise,
    # use as timeline the intersect between timelines.

    # Test 1: Tiles with same time period (regular cube)
    modis_cube_a <- .try(
        {
            sits_cube(
                source = "BDC",
                collection = "MOD13Q1-6.1",
                bands = c("NDVI"),
                roi = sits_tiles_to_roi("22KGA"),
                start_date = "2019-01-01",
                end_date = "2019-04-01",
                progress = FALSE
            )
        },
        .default = NULL
    )

    modis_cube_b <- .try(
        {
            sits_cube(
                source = "BDC",
                collection = "MOD13Q1-6.1",
                bands = c("NDVI"),
                roi = sits_tiles_to_roi("22KGA"),
                start_date = "2019-03-01",
                end_date = "2019-06-10",
                progress = FALSE
            )
        },
        .default = NULL
    )

    testthat::skip_if(purrr::is_null(c(modis_cube_a, modis_cube_b)),
                      message = "BDC is not accessible"
    )

    merged_cube <- sits_merge(modis_cube_a, modis_cube_b)

    expect_equal(length(sits_timeline(merged_cube)), 2)
    expect_equal(sits_bands(merged_cube), "NDVI")
    expect_equal(merged_cube[["tile"]], "013011")

    # Test 2: no time-series overlaps (regular cube)
    modis_cube_a <- .try(
        {
            sits_cube(
                source = "BDC",
                collection = "MOD13Q1-6.1",
                bands = c("NDVI"),
                roi = sits_tiles_to_roi("22KGA"),
                start_date = "2019-01-01",
                end_date = "2019-04-01",
                progress = FALSE
            )
        },
        .default = NULL
    )

    modis_cube_b <- .try(
        {
            sits_cube(
                source = "BDC",
                collection = "MOD13Q1-6.1",
                bands = c("NDVI"),
                roi = sits_tiles_to_roi("22KGA"),
                start_date = "2019-04-01",
                end_date = "2019-06-10",
                progress = FALSE
            )
        },
        .default = NULL
    )

    testthat::skip_if(purrr::is_null(c(modis_cube_a, modis_cube_b)),
                      message = "BDC is not accessible"
    )

    expect_error(sits_merge(modis_cube_a, modis_cube_b))
})

test_that("sits_merge - regular cubes with same bands and different tile", {
    # Test case: If the bands are the same, the cube will have the combined
    # timeline of both cubes. This is useful to merge data from the same sensors
    # from different satellites (e.g, Sentinel-2A with Sentinel-2B).
    # For regular cubes, then timeline has the same length, use them. Otherwise,
    # use as timeline the intersect between timelines.

    # Test 1: Different tiles
    modis_cube_a <- .try(
        {
            sits_cube(
                source = "BDC",
                collection = "MOD13Q1-6.1",
                bands = c("NDVI"),
                roi = sits_tiles_to_roi("22LBH"),
                start_date = "2019-01-01",
                end_date = "2019-04-01",
                progress = FALSE
            )
        },
        .default = NULL
    )

    modis_cube_b <- .try(
        {
            sits_cube(
                source = "BDC",
                collection = "MOD13Q1-6.1",
                bands = c("NDVI"),
                roi = sits_tiles_to_roi("22KGA"),
                start_date = "2019-02-01",
                end_date = "2019-06-10",
                progress = FALSE
            )
        },
        .default = NULL
    )

    testthat::skip_if(purrr::is_null(c(modis_cube_a, modis_cube_b)),
                      message = "BDC is not accessible"
    )

    merged_cube <- sits_merge(modis_cube_a, modis_cube_b)

    expect_equal(length(sits_timeline(merged_cube)), 4)
    expect_equal(
        sits_timeline(modis_cube_b)[seq_len(4)], sits_timeline(merged_cube)
    )
    expect_equal(sits_bands(merged_cube), "NDVI")
    expect_equal(merged_cube[["tile"]], c("012010", "013011"))

    # Test 2: Tile variation in one of the cubes
    s2_cube_a <- .try(
        {
            sits_cube(
                source = "BDC",
                collection = "SENTINEL-2-16D",
                bands = c("B02"),
                roi = sits_tiles_to_roi(c("20LMR", "20LNR")),
                start_date = "2019-01-01",
                end_date = "2019-04-01",
                progress = FALSE
            )
        },
        .default = NULL
    )

    s2_cube_b <- .try(
        {
            sits_cube(
                source = "BDC",
                collection = "SENTINEL-2-16D",
                bands = c("B02"),
                roi = sits_tiles_to_roi(c("20LMR")),
                start_date = "2019-04-01",
                end_date = "2019-06-10",
                progress = FALSE
            )
        },
        .default = NULL
    )

    merged_cube <- sits_merge(s2_cube_a, s2_cube_b)

    expect_equal(sits_timeline(merged_cube), sits_timeline(s2_cube_a))
    expect_equal(nrow(merged_cube), 4)
})

test_that("sits_merge - regular cubes with different bands and tile", {
    # Test case: if the bands are different and their timelines should be
    # compatible, the bands are joined. The resulting timeline is the one from
    # the first cube. This is useful to merge data from different sensors
    # (e.g, Sentinel-1 with Sentinel-2).
    # For regular cubes, then timeline has the same length, use them. Otherwise,
    # use as timeline the intersect between timelines.

    s2_cube_a <- .try(
        {
            sits_cube(
                source = "BDC",
                collection = "SENTINEL-2-16D",
                bands = c("B02"),
                roi = sits_tiles_to_roi(c("20LMR", "20LNR")),
                start_date = "2019-01-01",
                end_date = "2019-04-01",
                progress = FALSE
            )
        },
        .default = NULL
    )

    s2_cube_b <- .try(
        {
            sits_cube(
                source = "BDC",
                collection = "SENTINEL-2-16D",
                bands = c("B02", "B03"),
                roi = sits_tiles_to_roi(c("20LMR")),
                start_date = "2019-04-01",
                end_date = "2019-06-10",
                progress = FALSE
            )
        },
        .default = NULL
    )

    merged_cube <- sits_merge(s2_cube_a, s2_cube_b)

    expect_equal(sits_timeline(merged_cube), sits_timeline(s2_cube_a))
    expect_equal(nrow(merged_cube), 4)
    expect_equal(sits_bands(merged_cube), "B02")
})

test_that("sits_merge - regularize combined cubes", {
    # Test 1: Same sensor
    output_dir <- paste0(tempdir(), "/merge-reg-1")
    dir.create(output_dir, showWarnings = FALSE)

    s2a_cube <- .try(
        {
            sits_cube(
                source = "DEAUSTRALIA",
                collection = "ga_s2am_ard_3",
                bands = c("BLUE"),
                tiles = c("53HQE"),
                start_date = "2019-01-01",
                end_date = "2019-04-01",
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
                bands = c("BLUE"),
                tiles = c("53JQF"),
                start_date = "2019-02-01",
                end_date = "2019-06-10",
                progress = FALSE
            )
        },
        .default = NULL
    )

    testthat::skip_if(purrr::is_null(c(s2a_cube, s2b_cube)),
                      message = "DEAustralia is not accessible"
    )

    # merge
    merged_cube <- sits_merge(s2a_cube, s2b_cube)

    # regularize
    regularized_cube <- sits_regularize(
        cube = merged_cube,
        period = "P8D",
        res = 720,
        output_dir = output_dir
    )

    # test
    expect_equal(nrow(regularized_cube), 2)
    expect_equal(length(sits_timeline(regularized_cube)), 7)
    expect_equal(sits_bands(regularized_cube), "BLUE")
    expect_equal(.cube_xres(regularized_cube), 720)

    # Test 2: Different sensor
    output_dir <- paste0(tempdir(), "/merge-reg-2")
    dir.create(output_dir, showWarnings = FALSE)

    s2_cube <- .try(
        {
            sits_cube(
                source = "MPC",
                collection = "SENTINEL-2-L2A",
                bands = c("B02"),
                tiles = c("19LEF"),
                start_date = "2019-01-01",
                end_date = "2019-04-01",
                progress = FALSE
            )
        },
        .default = NULL
    )

    s1_cube <- .try(
        {
            sits_cube(
                source = "MPC",
                collection = "SENTINEL-1-RTC",
                bands = c("VV"),
                tiles = c("19LEF"),
                orbit = "descending",
                start_date = "2019-02-01",
                end_date = "2019-06-10",
                progress = FALSE
            )
        },
        .default = NULL
    )

    testthat::skip_if(purrr::is_null(c(s2_cube, s1_cube)),
                      message = "MPC is not accessible"
    )

    # merge
    merged_cube <- sits_merge(s2_cube, s1_cube)

    # regularize
    regularized_cube <- sits_regularize(
        cube = merged_cube,
        period = "P8D",
        res = 720,
        output_dir = output_dir
    )

    # test
    expect_equal(regularized_cube[["tile"]], "19LEF")
    expect_equal(length(sits_timeline(regularized_cube)), 7)
    expect_equal(sits_bands(regularized_cube), c("B02", "VV"))
    expect_equal(.cube_xres(regularized_cube), 720)
})

test_that("sits_merge - cubes with different classes", {
    s2_cube <- .try(
        {
            sits_cube(
                source = "MPC",
                collection = "SENTINEL-2-L2A",
                bands = c("B02"),
                tiles = c("19LEF"),
                start_date = "2019-01-01",
                end_date = "2019-04-01",
                progress = FALSE
            )
        },
        .default = NULL
    )

    s1_cube <- .try(
        {
            sits_cube(
                source = "MPC",
                collection = "SENTINEL-1-RTC",
                bands = c("VV"),
                tiles = c("19LEF"),
                orbit = "descending",
                start_date = "2019-02-01",
                end_date = "2019-06-10",
                progress = FALSE
            )
        },
        .default = NULL
    )

    testthat::skip_if(purrr::is_null(c(s2_cube, s1_cube)),
                      message = "MPC is not accessible"
    )

    # merge
    merged_cube_1 <- sits_merge(s2_cube, s1_cube)
    merged_cube_2 <- sits_merge(s1_cube, s2_cube)

    # test
    expect_equal(nrow(merged_cube_1), nrow(merged_cube_2))
    expect_equal(sort(merged_cube_1[["tile"]]), sort(merged_cube_2[["tile"]]))
})

test_that("sits_merge - special case - dem cube", {
    # create S2 cube
    s2_dir <- paste0(tempdir(), "/s2")
    dir.create(s2_dir, showWarnings = FALSE)
    s2_cube <- sits_cube(
        source = "MPC",
        collection = "SENTINEL-2-L2A",
        tiles = "19HBA",
        bands = c("B04", "B8A", "B12", "CLOUD"),
        start_date = "2021-01-01",
        end_date = "2021-03-31",
        progress = FALSE
    )

    s2_cube_reg <- sits_regularize(
        cube = s2_cube,
        period = "P16D",
        res = 720,
        output_dir = s2_dir,
        progress = FALSE
    )

    # create DEM cube
    dem_dir <- paste0(tempdir(), "/dem")
    dir.create(dem_dir, showWarnings = FALSE)
    dem_cube <- sits_cube(
        source = "MPC",
        collection = "COP-DEM-GLO-30",
        bands = "ELEVATION",
        tiles = "19HBA",
        progress = FALSE
    )

    dem_cube_reg <- sits_regularize(
        cube = dem_cube,
        res = 720,
        bands = "ELEVATION",
        tiles = "19HBA",
        output_dir = dem_dir,
        progress = FALSE
    )

    # merge
    merged_cube <- sits_merge(s2_cube_reg, dem_cube_reg)

    # test
    expect_equal(nrow(merged_cube[["file_info"]][[1]]), 24)
    expect_equal(sits_bands(merged_cube), c("B04", "B12", "B8A", "ELEVATION"))
})

test_that("sits_merge - special case - hls cube", {
    # define roi
    roi <- c(
        lon_min = -45.6422, lat_min = -24.0335,
        lon_max = -45.0840, lat_max = -23.6178
    )

    hls_cube_s2 <- sits_cube(
        source = "HLS",
        collection = "HLSS30",
        roi = roi,
        bands = c("BLUE", "GREEN", "RED", "CLOUD"),
        start_date = as.Date("2020-06-01"),
        end_date = as.Date("2020-09-01"),
        progress = FALSE
    )

    hls_cube_l8 <- sits_cube(
        source = "HLS",
        collection = "HLSL30",
        roi = roi,
        bands = c("BLUE", "GREEN", "RED", "CLOUD"),
        start_date = as.Date("2020-06-01"),
        end_date = as.Date("2020-09-01"),
        progress = FALSE
    )

    # merge
    merged_cube <- sits_merge(hls_cube_s2, hls_cube_l8)

    # test
    expect_equal(length(sits_timeline(merged_cube)), 19)
    expect_equal(sits_bands(merged_cube), c("BLUE", "GREEN", "RED", "CLOUD"))
})
