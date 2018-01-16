testthat::context("Filtering")
testthat::test_that("Envelope filter", {
    data(prodes_226_064)
    point_ndvi.tb <- sits_select(prodes_226_064[1,], bands = c("ndvi"))
    point_env.tb  <- sits_envelope(point_ndvi.tb)
    testthat::expect_true(all(point_env.tb$time_series[[1]][,2] >= point_ndvi.tb$time_series[[1]][,2]))
})

testthat::test_that("Cloud filter", {
    data(prodes_226_064)
    point_ndvi.tb <- sits_select(prodes_226_064[1,], bands = c("ndvi"))
    point_cld.tb  <- sits_cloud_filter(point_ndvi.tb)
    testthat::expect_true(min(point_cld.tb$time_series[[1]][,2]) >= min(point_ndvi.tb$time_series[[1]][,2]))
})

testthat::test_that("Whittaker filter", {
    data(point_ndvi)
    point_ws <- sits_whittaker(point_ndvi, lambda = 3.0)
    testthat::expect_true(NROW(point_ndvi$time_series[[1]]) == NROW(point_ws$time_series[[1]]))
})

testthat::test_that("Savitsky Golay filter", {
    data(point_ndvi)
    point_sg <- sits_sgolay(point_ndvi)
    testthat::expect_true(NROW(point_ndvi$time_series[[1]]) == NROW(point_sg$time_series[[1]]))
})
