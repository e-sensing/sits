context("ZOO")

test_that("Writing a zoo time series", {
    data(cerrado_2classes)
    zoo_lst <- sits_to_zoo(cerrado_2classes[1:5, ])

    expect_equal(length(zoo_lst), 5)
    expect_equal(dim(zoo_lst[[1]])[1], 23)
    expect_equal(dim(zoo_lst[[1]])[2], 2)
})

test_that("Reading a ZOO time series", {
    # skip_on_cran()
    data(ts_zoo)
    data <- sits_from_zoo(ts_zoo,
        longitude = -54.2313, latitude = -14.0482,
        label = "Cerrado", name = "mod13q1"
    )

    expect_equal(sum(sits_time_series(data)$ndvi), 13.6291, tolerance = 1e-3)
    expect_true(nrow(ts_zoo) == length(sits_time_series_dates(data)))
})
