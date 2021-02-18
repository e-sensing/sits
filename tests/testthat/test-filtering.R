context("Filtering")

test_that("Generic filter", {
    point_ndvi_whit <- sits_filter(point_ndvi)
    expect_true(length(sits_time_series_dates(point_ndvi))
    == length(sits_time_series_dates(point_ndvi_whit)))
})

test_that("Generic filter-error", {
    lambda <- 2
    expect_error(
        sits_filter(point_ndvi, lambda),
        "sits_filter: filter is not a valid function"
    )
})

test_that("Envelope filter", {
    testthat::skip_on_cran()
    library(dtwclust)
    point_env <- sits_envelope(point_ndvi, bands_suffix = "env")
    expect_true(all(sits_time_series(point_env)$NDVI.env
    >= sits_time_series(point_ndvi)$NDVI))
})

test_that("Whittaker filter", {
    # skip_on_cran()
    data(point_ndvi)
    point_ws <- sits_whittaker(point_ndvi, lambda = 3.0)
    expect_true(length(sits_time_series_dates(point_ndvi))
    == length(sits_time_series_dates(point_ws)))
})

test_that("Savitsky Golay filter", {
    # skip_on_cran()
    data(point_ndvi)
    point_sg <- sits_sgolay(point_ndvi)
    expect_true(length(sits_time_series_dates(point_ndvi)) ==
        length(sits_time_series_dates(point_sg)))
})

test_that("Interpolation filter", {
    data(point_ndvi)
    n_times <- length(sits_time_series_dates(point_ndvi))
    point_int <- sits_interp(point_ndvi, fun = stats::spline, n = 3 * n_times)

    # filtered data has less sd
    expect_true(sd(sits_time_series(point_ndvi)$NDVI) >
        sd(sits_time_series(point_int)$NDVI))
})

test_that("Linear interpolation filter", {
    data(point_ndvi)
    point_int <- sits_linear_interp(point_ndvi)

    # filtered data has less sd
    expect_true(sd(sits_time_series(point_ndvi)$NDVI) >
        sd(sits_time_series(point_int)$NDVI))
})

test_that("Kalman filter", {
    data(point_ndvi)
    kf <- sits_kalman(point_ndvi)
    expect_true(ncol(sits_time_series(kf)) == 4)
})

test_that("Arima filter", {
    point_cld <- sits_ndvi_arima(point_ndvi, bands_suffix = "ar")

    # filtered data has less sd
    expect_true(sd(sits_time_series(point_ndvi)$NDVI) >
        sd(sits_time_series(point_cld)$NDVI.ar.wf))
})

test_that("Missing values", {
    point_ndvi_2 <- sits_missing_values(point_ndvi, miss_value = -3000)
    expect_true(length(sits_time_series_dates(point_ndvi))
    == length(sits_time_series_dates(point_ndvi_2)))
})
