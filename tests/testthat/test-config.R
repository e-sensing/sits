context("Config")

test_that("All", {
    sits_config()
    invisible(capture.output(sits_show_config()))
})

test_that("Brightness", {
    ls8 <- sits:::.sits_get_tcap_brightness("LANDSAT8")

    expect_equal(names(ls8[3]), "red")
    expect_equal(ls8[[3]], 0.473, tol = 0.001)
    expect_equal(length(ls8), 6)

    mod <- sits:::.sits_get_tcap_brightness()

    expect_equal(names(mod[3]), "red")
    expect_equal(mod[[3]], 0.44, tol = 0.001)
    expect_equal(length(mod), 7)

    expect_error(sits:::.sits_get_tcap_brightness("ABC"), "Unable to retrieve tasseled cap coefficients")
})

test_that("Greenness", {
    ls8 <- sits:::.sits_get_tcap_greenness("LANDSAT8")

    expect_equal(names(ls8[3]), "red")
    expect_equal(ls8[[3]], -0.5424, tol = 0.001)
    expect_equal(length(ls8), 6)

    mod <- sits:::.sits_get_tcap_greenness()

    expect_equal(names(mod[3]), "red")
    expect_equal(mod[[3]], -0.4064, tol = 0.001)
    expect_equal(length(mod), 7)

    expect_error(sits:::.sits_get_tcap_greenness("ABC"), "Unable to retrieve tasseled cap coefficients")
})

test_that("Wetness", {
    ls8 <- sits:::.sits_get_tcap_wetness("LANDSAT8")

    expect_equal(names(ls8[3]), "red")
    expect_equal(ls8[[3]], 0.328, tol = 0.001)
    expect_equal(length(ls8), 6)

    mod <- sits:::.sits_get_tcap_wetness()

    expect_equal(names(mod[3]), "red")
    expect_equal(mod[[3]], -0.4064, tol = 0.001)
    expect_equal(length(mod), 7)

    expect_error(sits:::.sits_get_tcap_wetness("ABC"), "Unable to retrieve tasseled cap coefficients")
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
