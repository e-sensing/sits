context("WTSS")

test_that("Info", {
    invisible(capture.output(sits_infoWTSS()))

    expect_true(TRUE)
})

