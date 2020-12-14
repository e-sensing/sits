context("Space Time Operations")

test_that("All", {
    reproj <- sits:::.sits_latlong_to_proj(-10, -20, 4326)

    expect_equal(as.numeric(reproj[1, 1]), -10)
    expect_equal(as.numeric(reproj[1, 2]), -20)

    reproj <- sits:::.sits_proj_to_latlong(-10, -20, 4326)

    expect_equal(as.numeric(reproj[1, 1]), -10)
    expect_equal(as.numeric(reproj[1, 2]), -20)
})
test_that("Time Series Dates", {
    times <- sits_time_series_dates(cerrado_2classes)
    expect_true(length(times) == 23)
})
