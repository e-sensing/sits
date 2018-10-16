context("Info")

test_that("NDWI", {
    info <- sits_info_services()

    expect_type(info, "list")
})

