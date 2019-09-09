context("Filtering")
test_that("Envelope filter", {
    #skip_on_cran()
    library(dtwclust)
    data(prodes_226_064)
    point_ndvi <- sits_select_bands(prodes_226_064[1,], ndvi)
    point_env  <- sits_envelope(point_ndvi)
    expect_true(all(sits_time_series(point_env)$ndvi.env >= sits_time_series(point_ndvi)$ndvi ))
})

test_that("Cloud filter", {
    #skip_on_cran()
    data(prodes_226_064)
    point_ndvi <- sits_select_bands(prodes_226_064[1,], ndvi)
    point_cld  <- sits_cloud_removal(point_ndvi)
    expect_true(NROW(sits_time_series(point_cld)) == NROW(sits_time_series(point_ndvi)))
})

test_that("Whittaker filter", {
    #skip_on_cran()
    data(point_ndvi)
    point_ws <- sits_whittaker(point_ndvi, lambda = 3.0)
    expect_true(length(sits_time_series_dates(point_ndvi)) == length(sits_time_series_dates(point_ws)))
})

test_that("Savitsky Golay filter", {
    #skip_on_cran()
    data(point_ndvi)
    point_sg <- sits_sgolay(point_ndvi)
    expect_true(length(sits_time_series_dates(point_ndvi)) == length(sits_time_series_dates(point_sg)))
})

test_that("Interpolation filter", {
    data(point_ndvi)
    n_times  <- length(sits_time_series_dates(point_ndvi))
    point_int <- sits_interp(point_ndvi, fun = stats::spline, n = 3 * n_times)

    # filtered data has less sd
    expect_true(sd(sits_time_series(point_ndvi)$ndvi) > sd(sits_time_series(point_int)$ndvi))
})

test_that("Linear interpolation filter", {
    data(point_ndvi)
    point_int <- sits_linear_interp(point_ndvi)

    # filtered data has less sd
    expect_true(sd(sits_time_series(point_ndvi)$ndvi) > sd(sits_time_series(point_int)$ndvi))
})

test_that("Kalman filter", {
    data(point_ndvi)
    kf.tb <- sits_kalman(point_ndvi)
    expect_true(ncol(sits_time_series(kf.tb)) == 4)
})

test_that("Arima filter", {
    data(prodes_226_064)
    point_ndvi <- sits_select_bands(prodes_226_064[1,], ndvi)
    point_cld  <- sits_ndvi_arima(point_ndvi)

    # filtered data has less sd
    expect_true(sd(sits_time_series(point_ndvi)$ndvi) > sd(sits_time_series(point_cld)$ndvi.ar.whit))
})
