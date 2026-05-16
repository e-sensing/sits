test_that("same bands (1), interval, tiles (1) | regular -> regular", {
    modis_cube <- suppressWarnings(
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

    testthat::skip_if(purrr::is_null(modis_cube),
        message = "BDC is not accessible"
    )

    merged_cube <- sits_merge(modis_cube, modis_cube)

    expect_true(.cube_is_regular(merged_cube))
    expect_equal(nrow(modis_cube), 1)
    expect_equal(
        nrow(merged_cube[["file_info"]][[1]]),
        nrow(modis_cube[["file_info"]][[1]])
    )
})

test_that("same bands (1) | diff interval | same tiles (1) |
          regular -> error   | General case", {
    modis_cube_a <- suppressWarnings(
        .try(
            {
                sits_cube(
                    source = "BDC",
                    collection = "MOD13Q1-6.1",
                    bands = c("NDVI"),
                    roi = sits_tiles_to_roi("22KGA"),
                    start_date = "2019-04-01",
                    end_date = "2019-07-01",
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
                    end_date = "2019-08-01",
                    progress = FALSE
                )
            },
            .default = NULL
        )
    )

    testthat::skip_if(purrr::is_null(c(modis_cube_a, modis_cube_b)),
        message = "BDC is not accessible"
    )

    modis_cube_c <- sits_merge(modis_cube_a, modis_cube_b)

    expect_true(
        all(sits_timeline(modis_cube_a) %in%
                sits_timeline(modis_cube_c))
    )
    expect_true(
        all(sits_timeline(modis_cube_b) %in%
                sits_timeline(modis_cube_c))
    )
})

test_that("diff bands (1) | diff interval | same tiles (1) |
          regular -> regular | General case", {
    modis_cube_a <- suppressWarnings(
        .try(
            {
                sits_cube(
                    source = "BDC",
                    collection = "MOD13Q1-6.1",
                    bands = c("NDVI"),
                    roi = sits_tiles_to_roi("22KGA"),
                    start_date = "2019-04-01",
                    end_date = "2019-07-01",
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
                    bands = c("EVI"),
                    roi = sits_tiles_to_roi("22KGA"),
                    start_date = "2019-02-01",
                    end_date = "2019-08-01",
                    progress = FALSE
                )
            },
            .default = NULL
        )
    )

    testthat::skip_if(purrr::is_null(c(modis_cube_a, modis_cube_b)),
        message = "BDC is not accessible"
    )

    merged_cube <- sits_merge(modis_cube_a, modis_cube_b)

    expect_true(.cube_is_regular(merged_cube))
    expect_equal(
        sits_timeline(merged_cube),
        sits_timeline(modis_cube_a)
    )
    expect_equal(
        sits_bands(merged_cube), c("EVI", "NDVI")
    )
})

test_that("same bands (1) | diff interval | diff tiles (1) |
          regular -> error   | General case", {
    modis_cube_a <- suppressWarnings(
        .try(
            {
                sits_cube(
                    source = "BDC",
                    collection = "MOD13Q1-6.1",
                    bands = c("NDVI"),
                    roi = sits_tiles_to_roi("22KGA"),
                    start_date = "2019-04-01",
                    end_date = "2019-07-01",
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
                    roi = sits_tiles_to_roi("22KFG"),
                    start_date = "2019-02-01",
                    end_date = "2019-08-01",
                    progress = FALSE
                )
            },
            .default = NULL
        )
    )

    testthat::skip_if(purrr::is_null(c(modis_cube_a, modis_cube_b)),
        message = "BDC is not accessible"
    )

    expect_error(sits_merge(modis_cube_a, modis_cube_b))
})

test_that("diff bands (1) | diff interval | diff tiles (1) |
          regular -> error   | General case", {
    modis_cube_a <- suppressWarnings(
        .try(
            {
                sits_cube(
                    source = "BDC",
                    collection = "MOD13Q1-6.1",
                    bands = c("EVI"),
                    roi = sits_tiles_to_roi("22KGA"),
                    start_date = "2019-04-01",
                    end_date = "2019-07-01",
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
                    roi = sits_tiles_to_roi("22KFG"),
                    start_date = "2019-02-01",
                    end_date = "2019-08-01",
                    progress = FALSE
                )
            },
            .default = NULL
        )
    )

    testthat::skip_if(purrr::is_null(c(modis_cube_a, modis_cube_b)),
        message = "BDC is not accessible"
    )

    expect_error(sits_merge(modis_cube_a, modis_cube_b))
})

test_that("diff bands (1) | same interval | same tiles (1) |
          irregular -> irregular | General case", {
    s2_cube <- suppressWarnings(
        .try(
            {
                sits_cube(
                    source = "AWS",
                    collection = "SENTINEL-2-L2A",
                    bands = c("B02"),
                    tiles = c("22KGA"),
                    start_date = "2019-01-01",
                    end_date = "2019-04-01",
                    progress = FALSE
                )
            },
            .default = NULL
        )
    )
    testthat::skip_if(purrr::is_null(s2_cube),
                      message = "AWS is not accessible"
    )

    s1_cube <- suppressWarnings(
        .try(
            {
                sits_cube(
                    source = "MPC",
                    collection = "SENTINEL-1-RTC",
                    bands = c("VV"),
                    tiles = c("22KGA"),
                    orbit = "descending",
                    start_date = "2019-02-01",
                    end_date = "2019-06-10",
                    progress = FALSE
                )
            },
            .default = NULL
        )
    )

    testthat::skip_if(purrr::is_null(s1_cube),
        message = "MPC is not accessible"
    )


    # merge
    expect_error(sits_merge(s2_cube, s1_cube))

    dir_images <- paste0(tempdir(), "/images_merge_s1_s2_irr/")
    if (!dir.exists(dir_images)) {
        suppressWarnings(dir.create(dir_images))
    }

    s2_reg <- suppressWarnings(
        sits_regularize(
            cube = s2_cube,
            period = "P1M",
            res = 240,
            multicores = 2,
            output_dir = dir_images,
            progress = FALSE
        )
    )

    s1_reg <- suppressWarnings(
        sits_regularize(
            cube = s1_cube,
            period = "P1M",
            res = 240,
            multicores = 1,
            tiles = "22KGA",
            output_dir = dir_images,
            progress = FALSE
        )
    )

    merged_cube <- sits_merge(s2_reg, s1_reg)
    expect_true(all(sits_bands(merged_cube) %in% c("B02", "VV")))
    # test timeline compatibility
    merged_tl <- suppressWarnings(unname(sits_timeline(merged_cube)))
    # result timeline must be compatible (cube 1 is the reference in this case)
    expect_true(
        min(merged_tl[[2]]) >= min(merged_tl[[1]]) &
            max(merged_tl[[2]]) <= max(merged_tl[[2]])
    )
})

test_that("diff bands (1) | same interval | same tiles (1) |
          irregular -> irregular | HLS case", {
    roi <- c(
        lon_min = -45.6422, lat_min = -24.0335,
        lon_max = -45.0840, lat_max = -23.6178
    )

    hls_cube_s2 <- .try(
        {
            sits_cube(
                source = "MPC",
                collection = "HLSS30",
                roi = roi,
                bands = c("BLUE", "GREEN", "RED", "CLOUD"),
                start_date = as.Date("2024-06-01"),
                end_date = as.Date("2024-09-01"),
                progress = FALSE
            )
        },
        .default = NULL
    )
    testthat::skip_if(purrr::is_null(hls_cube_s2),
                      message = "HLS is not accessible"
    )

    hls_cube_l8 <- .try(
        {
            sits_cube(
                source = "MPC",
                collection = "HLSL30",
                roi = roi,
                bands = c("BLUE", "GREEN", "RED", "CLOUD"),
                start_date = as.Date("2024-06-01"),
                end_date = as.Date("2024-09-01"),
                progress = FALSE
            )
        },
        .default = NULL
    )

    testthat::skip_if(purrr::is_null(hls_cube_l8),
        message = "HLS in MPC is not accessible"
    )

    # merge
    merged_cube <- sits_merge(hls_cube_s2, hls_cube_l8)

    # test
    expect_equal(length(sits_timeline(merged_cube)), 25)
    expect_equal(sits_bands(merged_cube), c("BLUE", "CLOUD", "GREEN", "RED"))
})

test_that("dem cube | regularize", {
    s2_dir <- paste0(tempdir(), "/s2")
    dem_dir <- paste0(tempdir(), "/dem")

    dir.create(s2_dir, showWarnings = FALSE)
    dir.create(dem_dir, showWarnings = FALSE)

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

    testthat::skip_if(purrr::is_null(c(s2_cube, dem_cube)),
        message = "MPC is not accessible"
    )

    # Regularize S2
    s2_cube_reg <- suppressWarnings(
        sits_regularize(
            cube = s2_cube,
            period = "P16D",
            res = 720,
            output_dir = s2_dir,
            progress = FALSE
        )
    )

    # Regularize DEM
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
