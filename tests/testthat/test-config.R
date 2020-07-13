context("Config")

test_that("All", {
    sits_config()
    invisible(capture.output(sits_config_show()))
})

test_that("User configuration file", {
    if (!dir.exists("~/.sits"))
        dir.create("~/.sits")
    if(file.exists("~/.sits/config.yml"))
        file.remove("~/.sits/config.yml")
    file_config <- file("~/.sits/config.yml")
    writeLines(c("default:","      R_memory_bloat           : 10.0"), file_config)
    close(file_config)
    sits_config()
    expect_true(sits:::.sits_config_memory_bloat() == 10.0)
    invisible(capture.output(sits_config_show()))

    file.remove("~/.sits/config.yml")
})

test_that("Brightness", {
    ls8 <- sits:::.sits_config_tcap_brightness("OLI")

    expect_equal(names(ls8[3]), "red")
    expect_equal(ls8[[3]], 0.473, tol = 0.001)
    expect_equal(length(ls8), 6)

    mod <- sits:::.sits_config_tcap_brightness("MODIS")

    expect_equal(names(mod[3]), "red")
    expect_equal(mod[[3]], 0.44, tol = 0.001)
    expect_equal(length(mod), 7)

    expect_error(sits:::.sits_config_tcap_brightness("ABC"),
                 "Unable to retrieve tasseled cap coefficients")
})

test_that("Greenness", {
    ls8 <- sits:::.sits_config_tcap_greenness("OLI")

    expect_equal(names(ls8[3]), "red")
    expect_equal(ls8[[3]], -0.5424, tol = 0.001)
    expect_equal(length(ls8), 6)

    mod <- sits:::.sits_config_tcap_greenness("MODIS")

    expect_equal(names(mod[3]), "red")
    expect_equal(mod[[3]], -0.4064, tol = 0.001)
    expect_equal(length(mod), 7)

    expect_error(sits:::.sits_config_tcap_greenness("ABC"),
                 "Unable to retrieve tasseled cap coefficients")
})

test_that("Wetness", {
    ls8 <- sits:::.sits_config_tcap_wetness("OLI")

    expect_equal(names(ls8[3]), "red")
    expect_equal(ls8[[3]], 0.328, tol = 0.001)
    expect_equal(length(ls8), 6)

    mod <- sits:::.sits_config_tcap_wetness("MODIS")

    expect_equal(names(mod[3]), "red")
    expect_equal(mod[[3]], -0.4064, tol = 0.001)
    expect_equal(length(mod), 7)

    expect_error(sits:::.sits_config_tcap_wetness("ABC"),
                 "Unable to retrieve tasseled cap coefficients")
})

test_that("Internal", {

    cubes <- sits:::.sits_config_satveg_cubes()
    expect_true(length(cubes) > 1)

    bbox <- sits:::.sits_config_satveg_bbox(cubes[1])
    expect_true(bbox["xmin"] < bbox["xmax"])

    expect_equal(sits:::.sits_config_color("NoClass"), "#737373")

    expect_true(sits:::.sits_config_memory_bloat() > 1)

    bands <- sits:::.sits_config_satveg_bands(cubes[1])

    expect_true(.sits_config_minimum_values("MODIS", bands)[1] > -100000)
    expect_true(.sits_config_maximum_values("MODIS", bands)[1] <  100000)

})
