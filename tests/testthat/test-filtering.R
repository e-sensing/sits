test_that("Generic filter", {
    point_ndvi <- sits_select(point_mt_6bands, bands = "NDVI")
    point_ndvi_whit <- sits_filter(point_ndvi)
    expect_true(length(sits_timeline(point_ndvi))
                == length(sits_timeline(point_ndvi_whit)))
})

test_that("Generic filter-error", {
    point_ndvi <- sits_select(point_mt_6bands, bands = "NDVI")
    lambda <- 2
    expect_error(
        sits_filter(point_ndvi, lambda)
    )
})

test_that("Whittaker filter", {
    # skip_on_cran()
    point_ndvi <- sits_select(point_mt_6bands, bands = "NDVI")
    point_ws <- sits_whittaker(point_ndvi, lambda = 3.0)
    expect_true(length(sits_timeline(point_ndvi))
    == length(sits_timeline(point_ws)))
})

test_that("Savitsky Golay filter", {
    # skip_on_cran()
    point_ndvi <- sits_select(point_mt_6bands, bands = "NDVI")
    point_sg <- sits_sgolay(point_ndvi)
    expect_true(length(sits_timeline(point_ndvi)) ==
        length(sits_timeline(point_sg)))
})

test_that("Spline interpolation", {
    point_ndvi <- sits_select(point_mt_6bands, bands = "NDVI")
    n_times <- length(sits_timeline(point_ndvi))
    point_int <- sits_interp(point_ndvi, fun = stats::spline, n = 3 * n_times)

    # filtered data has less sd
    expect_true(sd(sits_time_series(point_ndvi)$NDVI) >
        sd(sits_time_series(point_int)$NDVI))
})

test_that("Linear interpolation", {
    point_ndvi <- sits_select(point_mt_6bands, bands = "NDVI")
    point_int <- sits_interp(point_ndvi)

    # filtered data has less sd
    expect_true(sd(sits_time_series(point_ndvi)$NDVI) >
        sd(sits_time_series(point_int)$NDVI))
})
