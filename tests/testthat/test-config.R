context("Config")

test_that("All", {
    sits_config()
    invisible(capture.output(sits_show_config()))
})

test_that("Internal", {
    expect_type(sits:::.sits_get_account("SATVEG"), "character")

    expect_equal(length(sits:::.sits_get_services()), 3)
    expect_equal(length(sits:::.sits_get_timeline("WTSS-INPE", "MOD13Q1")), 423)
    expect_equal(length(sits:::.sits_get_tcap_brightness()), 7)
    expect_equal(length(sits:::.sits_get_tcap_greenness()), 7)
    expect_equal(length(sits:::.sits_get_tcap_wetness()), 7)

    expect_error(sits:::.sits_get_bbox("RASTER"), "r_obj is mandatory when using a RASTER service")
    expect_equal(sum(sits:::.sits_get_bbox("RASTER", NULL, raster::raster())), 0)

    expect_equal(sits:::.sits_get_color(), "#737373")
    expect_true(sits:::.sits_get_memory_bloat() >= 0)

    expect_equal(sits:::.sits_get_services("WTSS"), "WTSS-INPE")
})
