context("Config")

test_that("Internal", {
    cubes <- sits:::.sits_config_satveg_cubes()
    expect_true(length(cubes) > 1)

    bbox <- sits:::.sits_config_satveg_bbox(cubes[1])
    expect_true(bbox["xmin"] < bbox["xmax"])

    expect_equal(sits:::.sits_config_color("NoClass"), "#737373")

    expect_true(sits:::.sits_config_memory_bloat() > 1)

    bands <- sits:::.sits_config_satveg_bands()

    expect_true(sits:::.sits_config_minimum_values("MODIS", bands)[1] > -100000)
    expect_true(sits:::.sits_config_maximum_values("MODIS", bands)[1] < 100000)
})

test_that("Show", {
    con <- file(paste0(tempdir(),"config.txt"))
    writeLines(capture.output(sits_config_show()), con)
    close(con)

    lin <- readLines(paste0(tempdir(),"config.txt"))

    expect_equal(lin[1], "default:")
    expect_true(grepl("bloat", lin[5]))
    expect_true(grepl("rstac", lin[8]))
})
