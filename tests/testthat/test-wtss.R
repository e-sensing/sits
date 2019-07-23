context("WTSS")

test_that("Info", {
    invisible(capture.output(sits_services()))

    expect_true(TRUE)
})

