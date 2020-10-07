context("Space Time Operations")

test_that("All", {
    reproj <- sits:::.sits_latlong_to_proj(-10, -20, 4326)

    expect_equal(as.numeric(reproj[1,1]), -10)
    expect_equal(as.numeric(reproj[1,2]), -20)

    reproj <- sits:::.sits_proj_to_latlong(-10, -20, 4326)

    expect_equal(as.numeric(reproj[1,1]), -10)
    expect_equal(as.numeric(reproj[1,2]), -20)
})

test_that("Inside", {
    wtss_cube <- sits_cube(type = "WTSS",
                           URL = "http://www.esensing.dpi.inpe.br/wtss/",
                           name = "MOD13Q1")

    point <- data.frame(X = 1, Y = 1)

    expect_false(sits:::.sits_raster_xy_inside(point, wtss_cube))

    point <- data.frame(X = -41, Y = -10)

    expect_true(sits:::.sits_raster_xy_inside(point, wtss_cube))
})
test_that("Time Series Dates", {
    times <- sits_time_series_dates(cerrado_2classes)
    expect_true(length(times) == 23)

})

