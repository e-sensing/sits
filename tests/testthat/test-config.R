context("Config")

test_that("All", {
    sits_config()
    invisible(capture.output(sits_show_config()))
})

test_that("Brightness", {
    ls8 <- sits:::.sits_tcap_brightness("OLI")

    expect_equal(names(ls8[3]), "red")
    expect_equal(ls8[[3]], 0.473, tol = 0.001)
    expect_equal(length(ls8), 6)

    mod <- sits:::.sits_tcap_brightness("MODIS")

    expect_equal(names(mod[3]), "red")
    expect_equal(mod[[3]], 0.44, tol = 0.001)
    expect_equal(length(mod), 7)

    expect_error(sits:::.sits_tcap_brightness("ABC"), "Unable to retrieve tasseled cap coefficients")
})

test_that("Greenness", {
    ls8 <- sits:::.sits_tcap_greenness("OLI")

    expect_equal(names(ls8[3]), "red")
    expect_equal(ls8[[3]], -0.5424, tol = 0.001)
    expect_equal(length(ls8), 6)

    mod <- sits:::.sits_tcap_greenness("MODIS")

    expect_equal(names(mod[3]), "red")
    expect_equal(mod[[3]], -0.4064, tol = 0.001)
    expect_equal(length(mod), 7)

    expect_error(sits:::.sits_tcap_greenness("ABC"), "Unable to retrieve tasseled cap coefficients")
})

test_that("Wetness", {
    ls8 <- sits:::.sits_tcap_wetness("OLI")

    expect_equal(names(ls8[3]), "red")
    expect_equal(ls8[[3]], 0.328, tol = 0.001)
    expect_equal(length(ls8), 6)

    mod <- sits:::.sits_tcap_wetness("MODIS")

    expect_equal(names(mod[3]), "red")
    expect_equal(mod[[3]], -0.4064, tol = 0.001)
    expect_equal(length(mod), 7)

    expect_error(sits:::.sits_tcap_wetness("ABC"), "Unable to retrieve tasseled cap coefficients")
})

test_that("Internal", {

    services <- sits:::.sits_services()
    expect_true("WTSS" %in% services)
    expect_true("SATVEG" %in% services)
    expect_true("LOCALHOST" %in% services)

    cubes <- sits:::.sits_cubes_service("SATVEG")
    expect_true(length(cubes) > 1)

    bbox <- sits:::.sits_bbox_service("SATVEG", cubes[1])
    expect_true(bbox["xmin"] < bbox["xmax"])

    expect_equal(sits:::.sits_color("NoClass"), "#737373")

    expect_true(sits:::.sits_memory_bloat() > 1)

    bands <- sits:::.sits_bands_service("SATVEG", cubes[1])

    expect_true(.sits_minimum_values("MODIS", bands)[1] > -100000)
    expect_true(.sits_maximum_values("MODIS", bands)[1] <  100000)

})
