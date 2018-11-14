context("WTSS")

test_that("Info", {
    invisible(capture.output(sits_info_wtss()))

    expect_true(TRUE)
})

