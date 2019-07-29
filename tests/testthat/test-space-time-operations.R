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
    wtss_cube <- sits_cube(service = "WTSS", name = "MOD13Q1")

    res <- sits:::.sits_convert_resolution(wtss_cube)

    expect_equal(res[[1]], 230.7679, tol = 0.001)
    expect_equal(res[[2]], 232.3231, tol = 0.001)
})

test_that("Inside", {
    wtss_cube <- sits_cube(service = "WTSS", name = "MOD13Q1")

    point <- data.frame(X = 1, Y = 1)

    expect_false(sits:::.sits_xy_inside_raster(point, wtss_cube))

    point <- data.frame(X = -41, Y = -10)

    expect_true(sits:::.sits_xy_inside_raster(point, wtss_cube))
})
