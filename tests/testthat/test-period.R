test_that("Period windows", {
    wind <- .period_windows(
        period = "P6M",
        step = "P1M",
        start_date = "2024-01-01",
        end_date = "2024-12-31")

    expect_equal(length(wind), 12)
    expect_equal(wind[[1]][["start"]], as.Date("2024-01-01"))
})
