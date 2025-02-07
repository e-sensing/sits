test_that("same bands and equal tiles - irregular cubes", {
    # Test case: If the bands are the same, the cube will have the combined
    # timeline of both cubes. This is useful to merge data from the same sensors
    # from different satellites (e.g, Sentinel-2A with Sentinel-2B).

    # Test 1: Single tile with different time period
    # Case 6 in Table
    s2a_cube <- .try(
        {
            sits_cube(
                source = "DEAUSTRALIA",
                collection = "GA_S2AM_ARD_3",
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
                start_date = "2019-03-01",
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
})

test_that("same bands with multiple equal tiles - irregular cubes", {
    # Test 2: Multiple tiles with different time period
    # # Another version of Case 6
    s2a_cube <- .try(
        {
            sits_cube(
                source = "DEAUSTRALIA",
                collection = "GA_S2AM_ARD_3",
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

test_that("same bands with equal tiles - regular cubes", {
    # Test 3: Tiles with same time period - CASE 2
    modis_cube_a <- suppressWarnings(
        .try(
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
    )

    modis_cube_b <- suppressWarnings(
        .try(
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
    )

    testthat::skip_if(purrr::is_null(c(modis_cube_a, modis_cube_b)),
                      message = "BDC is not accessible"
    )

    expect_error(
        sits_merge(modis_cube_a, modis_cube_b)
    )
})

test_that("same bands case and different tiles - irregular cubes", {
    # Test case: If the bands are the same, the cube will have the combined
    # timeline of both cubes. This is useful to merge data from the same sensors
    # from different satellites (e.g, Sentinel-2A with Sentinel-2B).

    # Test 1: Aligned timelines (DOES THIS CASE MAKE SENSE????)
    s2a_cube <- suppressWarnings(
        .try(
            {
                sits_cube(
                    source = "DEAUSTRALIA",
                    collection = "GA_S2AM_ARD_3",
                    bands = c("BLUE"),
                    tiles = c("53HQE"),
                    start_date = "2019-01-01",
                    end_date = "2019-04-01",
                    progress = FALSE
                )
            },
            .default = NULL
        )
    )

    s2b_cube <- suppressWarnings(
        .try(
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
    )

    testthat::skip_if(purrr::is_null(c(s2a_cube, s2b_cube)),
                      message = "DEAustralia is not accessible"
    )

    merged_cube <- sits_merge(s2a_cube, s2b_cube)

    expect_true(inherits(merged_cube, "combined_cube"))
    expect_equal(suppressWarnings(length(sits_timeline(merged_cube))), 2)
})

test_that("same bands case and different tiles - regular cubes", {
    # Test 2: Overlapping timelines (DOES THIS CASE MAKE SENSE????)
    modis_cube_a <- suppressWarnings(
        .try(
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
    )

    modis_cube_b <- suppressWarnings(
        .try(
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
    )

    testthat::skip_if(purrr::is_null(c(modis_cube_a, modis_cube_b)),
                      message = "BDC is not accessible"
    )

    expect_error(
        sits_merge(modis_cube_a, modis_cube_b)
    )
})

test_that("different bands case and equal tiles - irregular cubes", {
    # Test case: if the bands are different and their timelines should be
    # compatible, the bands are joined. The resulting timeline is the one from
    # the first cube. This is useful to merge data from different sensors
    # (e.g, Sentinel-1 with Sentinel-2).

    # Test 1a: Aligned timelines - CASE 6
    s2a_cube <- suppressWarnings(
        .try(
            {
                sits_cube(
                    source = "DEAUSTRALIA",
                    collection = "GA_S2AM_ARD_3",
                    bands = c("RED"),
                    tiles = c("53HQE"),
                    start_date = "2019-04-01",
                    end_date = "2019-06-10",
                    progress = FALSE
                )
            },
            .default = NULL
        )
    )

    s2b_cube <- suppressWarnings(
        .try(
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
    )

    testthat::skip_if(purrr::is_null(c(s2a_cube, s2b_cube)),
                      message = "DEAustralia is not accessible"
    )

    # timeline created with the zipper algorithm
    merged_cube <- sits_merge(s2a_cube, s2b_cube)
    expect_equal(length(sits_timeline(merged_cube)), 21)
    expect_equal(sits_bands(merged_cube), c("BLUE", "RED"))
    expect_equal(merged_cube[["tile"]], "53HQE")
})

test_that("different bands case and equal tiles - regular cubes", {
    # Test 1b: Aligned timelines - CASE 1
    s2_cube_a <- suppressWarnings(
        .try(
            {
                sits_cube(
                    source = "BDC",
                    collection = "SENTINEL-2-16D",
                    bands = c("B02"),
                    roi = sits_tiles_to_roi(c("20LMR")),
                    start_date = "2019-01-01",
                    end_date = "2019-04-01",
                    progress = FALSE
                )
            },
            .default = NULL
        )
    )

    s2_cube_b <- suppressWarnings(
        .try(
            {
                sits_cube(
                    source = "BDC",
                    collection = "SENTINEL-2-16D",
                    bands = c("B03"),
                    roi = sits_tiles_to_roi(c("20LMR")),
                    start_date = "2019-01-01",
                    end_date = "2019-04-01",
                    progress = FALSE
                )
            },
            .default = NULL
        )
    )

    merged_cube <- sits_merge(s2_cube_a, s2_cube_b)
    expect_equal(sits_timeline(merged_cube), sits_timeline(s2_cube_a))
    expect_equal(nrow(merged_cube), 4)
})

test_that("different bands case, equal tiles and different intervals - regular cubes", {
    s2_cube_a <- suppressWarnings(
        .try(
            {
                sits_cube(
                    source = "BDC",
                    collection = "SENTINEL-2-16D",
                    bands = c("B02"),
                    roi = sits_tiles_to_roi(c("20LNR")),
                    start_date = "2019-01-01",
                    end_date = "2019-04-01",
                    progress = FALSE
                )
            },
            .default = NULL
        )
    )

    s2_cube_b <- suppressWarnings(
        .try(
            {
                sits_cube(
                    source = "BDC",
                    collection = "SENTINEL-2-16D",
                    bands = c("B03"),
                    roi = sits_tiles_to_roi(c("20LNR")),
                    start_date = "2019-04-01",
                    end_date = "2019-06-01",
                    progress = FALSE
                )
            },
            .default = NULL
        )
    )
    # merge and test
    expect_error(sits_merge(s2_cube_a, s2_cube_b))
})

test_that("different bands case and equal tiles - rainfall", {
    # Test 2b: Overlapping timelines - CASE 6
    rainfall <- suppressWarnings(
        .try(
            {
                sits_cube(
                    source = "DEAFRICA",
                    collection = "RAINFALL-CHIRPS-MONTHLY",
                    roi = sits_tiles_to_roi("38LQK"),
                    start_date  = "2022-01-01",
                    end_date    = "2022-06-01",
                    progress = FALSE
                )
            },
            .default = NULL
        )
    )

    s2b_cube <- suppressWarnings(
        .try(
            {
                sits_cube(
                    source = "DEAFRICA",
                    collection = "SENTINEL-2-L2A",
                    bands = c("B02"),
                    tiles = c("38LQK"),
                    start_date = "2022-01-01",
                    end_date = "2022-06-01",
                    progress = FALSE
                )
            },
            .default = NULL
        )
    )

    testthat::skip_if(purrr::is_null(c(rainfall, s2b_cube)),
                      message = "DEAFRICA is not accessible"
    )

    # merge
    merged_cube <- sits_merge(rainfall, s2b_cube)
    # test
    expect_true("combined_cube" %in% class(merged_cube))
    # test timeline compatibility
    merged_tl <- suppressWarnings(unname(sits_timeline(merged_cube)))
    # result timeline must be compatible (cube 1 is the reference in this case)
    expect_true(
        min(merged_tl[[2]]) >= min(merged_tl[[1]]) &
            max(merged_tl[[2]]) <= max(merged_tl[[2]])
    )
})

test_that("different bands case and different intervals - irregular cubes", {
    # Test 3: Different timelines - CASE 6
    s2a_cube <- suppressWarnings(
        .try(
            {
                sits_cube(
                    source = "DEAUSTRALIA",
                    collection = "ga_s2am_ard_3",
                    bands = c("RED"),
                    tiles = c("53HQF"),
                    start_date = "2019-01-01",
                    end_date = "2019-04-01",
                    progress = FALSE
                )
            },
            .default = NULL
        )
    )

    s2b_cube <- suppressWarnings(
        .try(
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
    )

    testthat::skip_if(purrr::is_null(c(s2a_cube, s2b_cube)),
                      message = "DEAustralia is not accessible"
    )

    merged_cube <- expect_error(sits_merge(s2a_cube, s2b_cube))
})

test_that("different bands case and different collections - irregular cubes", {
    # Test 4: Different sensor with same timeline - CASE 8
    s2_cube <- suppressWarnings(
        .try(
            {
                sits_cube(
                    source = "AWS",
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
    )

    s1_cube <- suppressWarnings(
        .try(
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
    )

    testthat::skip_if(purrr::is_null(c(s2_cube, s1_cube)),
                      message = "MPC is not accessible"
    )

    # merge
    merged_cube <- sits_merge(s2_cube, s1_cube)
    expect_equal(sits_bands(merged_cube[1,]), "B02")
    expect_equal(sits_bands(merged_cube[2,]), "VV")
    expect_equal(merged_cube[["tile"]], c("19LEF", "NoTilingSystem"))
    expect_true("combined_cube" %in% class(merged_cube))
    # test timeline compatibility
    merged_tl <- suppressWarnings(unname(sits_timeline(merged_cube)))
    # result timeline must be compatible (cube 1 is the reference in this case)
    expect_true(
        min(merged_tl[[2]]) >= min(merged_tl[[1]]) &
        max(merged_tl[[2]]) <= max(merged_tl[[2]])
    )
})

test_that("different bands case and different tiles - regular cubes", {
    # Test case: if the bands are different and their timelines should be
    # compatible, the bands are joined. The resulting timeline is the one from
    # the first cube. This is useful to merge data from different sensors
    # (e.g, Sentinel-1 with Sentinel-2).

    s2_cube_a <- suppressWarnings(
        .try(
            {
                sits_cube(
                    source = "BDC",
                    collection = "SENTINEL-2-16D",
                    bands = c("B02"),
                    roi = sits_tiles_to_roi(c("20LNR")),
                    start_date = "2019-01-01",
                    end_date = "2019-04-01",
                    progress = FALSE
                )
            },
            .default = NULL
        )
    )

    s2_cube_b <- suppressWarnings(
        .try(
            {
                sits_cube(
                    source = "BDC",
                    collection = "SENTINEL-2-16D",
                    bands = c("B03"),
                    roi = sits_tiles_to_roi(c("20LMR")),
                    start_date = "2019-01-01",
                    end_date = "2019-04-01",
                    progress = FALSE
                )
            },
            .default = NULL
        )
    )
    # merge
    merged_cube <- expect_error(sits_merge(s2_cube_a, s2_cube_b))
})

test_that("same bands, same time and same interval - regular cubes", {
    s2a_cube <- suppressWarnings(
        .try(
            {
                sits_cube(
                    source = "DEAUSTRALIA",
                    collection = "ga_s2am_ard_3",
                    bands = c("BLUE"),
                    tiles = c("52LFK"),
                    start_date = "2019-01-01",
                    end_date = "2019-04-01",
                    progress = FALSE
                )
            },
            .default = NULL
        )
    )

    s2b_cube <- suppressWarnings(
        .try(
            {
                sits_cube(
                    source = "DEAUSTRALIA",
                    collection = "GA_S2BM_ARD_3",
                    bands = c("BLUE"),
                    tiles = c("52LFK"),
                    start_date = "2019-02-01",
                    end_date = "2019-06-10",
                    progress = FALSE
                )
            },
            .default = NULL
        )
    )

    testthat::skip_if(purrr::is_null(c(s2a_cube, s2b_cube)),
                      message = "DEAustralia is not accessible"
    )

    # merge
    expect_error(sits_merge(s2a_cube, s2b_cube))
})

test_that("regularize combined cubes with different sensor", {
   # Test 2: Different sensor - CASE 8
    output_dir <- paste0(tempdir(), "/merge-reg-2")
    dir.create(output_dir, showWarnings = FALSE)




    s2_cube <- suppressWarnings(
        .try(
            {
                sits_cube(
                    source = "AWS",
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
    )

    s1_cube <- suppressWarnings(
        .try(
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
    )

    testthat::skip_if(purrr::is_null(c(s2_cube, s1_cube)),
                      message = "MPC is not accessible"
    )

    # merge
    merged_cube <- sits_merge(s2_cube, s1_cube)

    # regularize
    regularized_cube <- suppressWarnings(
        sits_regularize(
            cube = merged_cube,
            period = "P8D",
            res = 720,
            output_dir = output_dir,
            progress = FALSE
        )
    )

    # test
    expect_equal(regularized_cube[["tile"]], "19LEF")
    expect_equal(length(sits_timeline(regularized_cube)), 7)
    expect_equal(sits_bands(regularized_cube), c("B02", "VV"))
    expect_equal(.cube_xres(regularized_cube), 720)

    unlink(output_dir, recursive = TRUE)
})

test_that("sits_merge - cubes with different classes", {
    # CASE 8
    s2_cube <- .try(
        {
            sits_cube(
                source = "AWS",
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
    # # INCLUDE NEW CASE????
    s2_dir <- paste0(tempdir(), "/s2")
    dir.create(s2_dir, showWarnings = FALSE)
    s2_cube <- suppressWarnings(
        .try(
            {
                sits_cube(
                    source = "AWS",
                    collection = "SENTINEL-2-L2A",
                    tiles = "19HBA",
                    bands = c("B04", "B8A", "B12", "CLOUD"),
                    start_date = "2021-01-01",
                    end_date = "2021-03-31",
                    progress = FALSE
                )
            },
            .default = NULL
        )
    )

    testthat::skip_if(purrr::is_null(s2_cube),
                      message = "MPC is not accessible"
    )

    s2_cube_reg <- suppressWarnings(
        sits_regularize(
            cube = s2_cube,
            period = "P16D",
            res = 720,
            output_dir = s2_dir,
            progress = FALSE
        )
    )

    # create DEM cube
    dem_dir <- paste0(tempdir(), "/dem")
    dir.create(dem_dir, showWarnings = FALSE)
    dem_cube <- .try(
        {
            sits_cube(
                source = "MPC",
                collection = "COP-DEM-GLO-30",
                bands = "ELEVATION",
                tiles = "19HBA",
                progress = FALSE
            )
        },
        .default = NULL
    )

    testthat::skip_if(purrr::is_null(dem_cube),
                      message = "MPC is not accessible"
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

    unlink(s2_dir, recursive = TRUE)
    unlink(dem_dir, recursive = TRUE)
})

test_that("sits_merge - special case - hls cube", {
    # CASE 6
    # define roi
    roi <- c(
        lon_min = -45.6422, lat_min = -24.0335,
        lon_max = -45.0840, lat_max = -23.6178
    )

    hls_cube_s2 <- .try(
        {
            sits_cube(
                source = "HLS",
                collection = "HLSS30",
                roi = roi,
                bands = c("BLUE", "GREEN", "RED", "CLOUD"),
                start_date = as.Date("2020-06-01"),
                end_date = as.Date("2020-09-01"),
                progress = FALSE
            )
        },
        .default = NULL
    )

    hls_cube_l8 <- .try(
        {
            sits_cube(
                source = "HLS",
                collection = "HLSL30",
                roi = roi,
                bands = c("BLUE", "GREEN", "RED", "CLOUD"),
                start_date = as.Date("2020-06-01"),
                end_date = as.Date("2020-09-01"),
                progress = FALSE
            )
        },
        .default = NULL
    )

    testthat::skip_if(purrr::is_null(c(hls_cube_s2, hls_cube_l8)),
                      message = "HLS is not accessible"
    )

    # merge
    merged_cube <- sits_merge(hls_cube_s2, hls_cube_l8)

    # test
    expect_equal(length(sits_timeline(merged_cube)), 19)
    expect_equal(sits_bands(merged_cube), c("BLUE", "CLOUD", "GREEN", "RED"))
})
