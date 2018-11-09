context("Info")

test_that("NDWI", {
    invisible(capture.output(info <- sits_info_services()))

    expect_type(info, "list")
})

