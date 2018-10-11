context("Filtering")
test_that("Envelope filter", {
    #skip_on_cran()
    library(dtwclust)
    data(prodes_226_064)
    point_ndvi.tb <- sits_select(prodes_226_064[1,], bands = c("ndvi"))
    point_env.tb  <- sits_envelope(point_ndvi.tb)
    expect_true(all(point_env.tb$time_series[[1]][,2] >= point_ndvi.tb$time_series[[1]][,2]))
})

test_that("Cloud filter", {
    #skip_on_cran()
    data(prodes_226_064)
    point_ndvi.tb <- sits_select(prodes_226_064[1,], bands = c("ndvi"))
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
