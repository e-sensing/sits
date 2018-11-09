context("Filtering")
test_that("Envelope filter", {
    #skip_on_cran()
    library(dtwclust)
    data(prodes_226_064)
    point_ndvi.tb <- sits_select_bands(prodes_226_064[1,], ndvi)
    point_env.tb  <- sits_envelope(point_ndvi.tb)
    expect_true(all(point_env.tb$time_series[[1]][,2] >= point_ndvi.tb$time_series[[1]][,2]))
})

test_that("Cloud filter", {
    #skip_on_cran()
    data(prodes_226_064)
    point_ndvi.tb <- sits_select_bands(prodes_226_064[1,], ndvi)
    point_cld.tb  <- sits_cloud_filter(point_ndvi.tb)
    expect_true(min(point_cld.tb$time_series[[1]][,2]) >= min(point_ndvi.tb$time_series[[1]][,2]))
})

test_that("Whittaker filter", {
    #skip_on_cran()
    data(point_ndvi)
    point_ws <- sits_whittaker(point_ndvi, lambda = 3.0)
    expect_true(NROW(point_ndvi$time_series[[1]]) == NROW(point_ws$time_series[[1]]))
})

test_that("Savitsky Golay filter", {
    #skip_on_cran()
    data(point_ndvi)
    point_sg <- sits_sgolay(point_ndvi)
    expect_true(NROW(point_ndvi$time_series[[1]]) == NROW(point_sg$time_series[[1]]))
})

test_that("Interpolation filter", {
    data(point_ndvi)
    n_times <- NROW (point_ndvi$time_series[[1]])
    point_int.tb <- sits_interp(point_ndvi, fun = stats::spline, n = 3 * n_times)

    # filtered data has less sd
    expect_equal(sd(point_ndvi$time_series[[1]]$ndvi), 0.2281, tolerance = 0.0001)
    expect_equal(sd(point_int.tb$time_series[[1]]$ndvi), 0.2253, tolerance = 0.0001)
})

test_that("Linear interpolation filter", {
    data(point_ndvi)
    n_times <- NROW (point_ndvi$time_series[[1]])
    point_int.tb <- sits_linear_interp(point_ndvi)

    # filtered data has less sd
    expect_equal(sd(point_ndvi$time_series[[1]]$ndvi), 0.2281, tolerance = 0.0001)
    expect_equal(sd(point_int.tb$time_series[[1]]$ndvi), 0.2244, tolerance = 0.0001)
})

test_that("Kalman filter", {
    data(point_ndvi)

    kf.tb <- sits_kalman(point_ndvi)

    expect_equal(length(names(kf.tb$time_series[[1]])), 4)
})

test_that("Arima filter", {
    data(prodes_226_064)
    point_ndvi.tb <- sits_select_bands(prodes_226_064[1,], ndvi)
    point_cld.tb <- sits_ndvi_arima_filter(point_ndvi.tb)

    # filtered data has less sd
    expect_equal(sd(point_ndvi$time_series[[1]]$ndvi), 0.2281, tolerance = 0.0001)
    expect_equal(sd(point_cld.tb$time_series[[1]]$ndvi.ar.whit), 0.0595, tolerance = 0.0001)
})
