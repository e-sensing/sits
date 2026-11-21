# remote cubes check the merge on real file ids; test-merge.R covers the flows
cube_dates <- function(cube) {
    sort(unique(do.call(c, purrr::map(cube[["file_info"]], "date"))))
}

test_that("Merge remote - BDC, other band, other interval", {
    open_bdc <- function(band, start_date, end_date) {
        suppressWarnings(.try(
            sits_cube(
                source = "BDC", collection = "MOD13Q1-6.1", bands = band,
                roi = sits_tiles_to_roi("22KGA"),
                start_date = start_date, end_date = end_date,
                progress = FALSE
            ),
            .default = NULL
        ))
    }
    ndvi <- open_bdc("NDVI", "2019-04-01", "2019-07-01")
    evi <- open_bdc("EVI", "2019-02-01", "2019-08-01")
    testthat::skip_if(is.null(ndvi) || is.null(evi),
        message = "BDC is not accessible"
    )
    merged <- sits_merge(ndvi, evi)
    expect_true(.cube_is_regular(merged))
    expect_equal(sits_timeline(merged), sits_timeline(ndvi))
    expect_equal(sits_bands(merged), c("EVI", "NDVI"))
})

test_that("Merge remote - HLS", {
    roi <- c(
        lon_min = -45.6422, lat_min = -24.0335,
        lon_max = -45.0840, lat_max = -23.6178
    )
    open_hls <- function(collection) {
        .try(
            sits_cube(
                source = "MPC", collection = collection, roi = roi,
                bands = c("RED", "CLOUD"),
                start_date = as.Date("2024-06-01"),
                end_date = as.Date("2024-07-01"),
                progress = FALSE
            ),
            .default = NULL
        )
    }
    s30 <- open_hls("HLSS30")
    l30 <- open_hls("HLSL30")
    testthat::skip_if(is.null(s30) || is.null(l30),
        message = "HLS in MPC is not accessible"
    )
    merged <- sits_merge(s30, l30)
    expect_true(all(merged[["collection"]] == "HLSS30"))
    expect_equal(sits_bands(merged), c("CLOUD", "RED"))
    expect_equal(
        cube_dates(merged),
        sort(unique(c(cube_dates(s30), cube_dates(l30))))
    )
})

test_that("Merge remote - regularized S2 and DEM", {
    s2 <- suppressWarnings(.try(
        sits_cube(
            source = "AWS", collection = "SENTINEL-2-L2A",
            tiles = "19HBA", bands = c("B04", "CLOUD"),
            start_date = "2021-01-01", end_date = "2021-02-15",
            progress = FALSE
        ),
        .default = NULL
    ))
    dem <- .try(
        sits_cube(
            source = "MPC", collection = "COP-DEM-GLO-30",
            bands = "ELEVATION", tiles = "19HBA", progress = FALSE
        ),
        .default = NULL
    )
    testthat::skip_if(is.null(s2) || is.null(dem),
        message = "AWS or MPC is not accessible"
    )
    s2_dir <- file.path(tempdir(), "merge_remote_s2")
    dem_dir <- file.path(tempdir(), "merge_remote_dem")
    purrr::walk(c(s2_dir, dem_dir), dir.create, showWarnings = FALSE)
    on.exit(unlink(c(s2_dir, dem_dir), recursive = TRUE), add = TRUE)
    # 360 m divides the 109800 m tile, so S2 and the other cube share one grid
    s2_reg <- suppressWarnings(sits_regularize(
        cube = s2, period = "P16D", res = 360,
        output_dir = s2_dir, progress = FALSE
    ))
    dem_reg <- sits_regularize(
        cube = dem, res = 360, bands = "ELEVATION", tiles = "19HBA",
        output_dir = dem_dir, progress = FALSE
    )
    merged <- sits_merge(s2_reg, dem_reg)
    expect_equal(sits_bands(merged), c("B04", "ELEVATION"))
    expect_equal(
        nrow(merged[["file_info"]][[1L]]),
        2L * length(sits_timeline(s2_reg))
    )
})

test_that("Merge remote - S2 and S1", {
    s2 <- suppressWarnings(.try(
        sits_cube(
            source = "AWS", collection = "SENTINEL-2-L2A",
            bands = "B02", tiles = "22KGA",
            start_date = "2019-02-01", end_date = "2019-04-01",
            progress = FALSE
        ),
        .default = NULL
    ))
    s1 <- suppressWarnings(.try(
        sits_cube(
            source = "MPC", collection = "SENTINEL-1-RTC",
            bands = "VV", tiles = "22KGA", orbit = "descending",
            start_date = "2019-02-01", end_date = "2019-04-01",
            progress = FALSE
        ),
        .default = NULL
    ))
    testthat::skip_if(is.null(s2) || is.null(s1),
        message = "AWS or MPC is not accessible"
    )
    # irregular cubes are not merged
    expect_error(
        sits_merge(s2, s1), .conf("messages", ".merge_type"), fixed = TRUE
    )
    out_dir <- file.path(tempdir(), "merge_remote_s1_s2")
    dir.create(out_dir, showWarnings = FALSE)
    on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)
    # 360 m divides the 109800 m tile, so S2 and the other cube share one grid
    s2_reg <- suppressWarnings(sits_regularize(
        cube = s2, period = "P1M", res = 360,
        output_dir = out_dir, progress = FALSE
    ))
    s1_reg <- suppressWarnings(sits_regularize(
        cube = s1, period = "P1M", res = 360, tiles = "22KGA",
        output_dir = out_dir, progress = FALSE
    ))
    merged <- sits_merge(s2_reg, s1_reg)
    expect_equal(sits_bands(merged), c("B02", "VV"))
})
