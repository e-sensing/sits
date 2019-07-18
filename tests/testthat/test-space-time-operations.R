context("Space Time Operations")

test_that("All", {
    reproj <- sits:::.sits_latlong_to_proj(-10, -20, 4326)

    expect_equal(reproj[1], -10)
    expect_equal(reproj[2], -20)

    reproj <- sits:::.sits_proj_to_latlong(-10, -20, 4326)

    expect_equal(reproj[1], -10)
    expect_equal(reproj[2], -20)
})

test_that("Convert", {
    wtss_coverage <- sits_coverage(service = "WTSS", name = "MOD13Q1")

    res <- sits:::.sits_convert_resolution(wtss_coverage)

    expect_equal(res[[1]], 0.002134755, tol = 0.00001)
    expect_equal(res[[2]], 0.002083333, tol = 0.00001)
})

test_that("Inside", {
    wtss_coverage <- sits_coverage(service = "WTSS", name = "MOD13Q1")

    point <- data.frame(X = 1, Y = 1)

    expect_false(sits:::.sits_xy_inside_raster(point, wtss_coverage))

    point <- data.frame(X = -41, Y = -10)

    expect_true(sits:::.sits_xy_inside_raster(point, wtss_coverage))
})
