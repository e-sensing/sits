context("Config")

test_that("Internal", {
    cubes <- sits:::.sits_config_satveg_cubes()
    expect_true(length(cubes) > 1)

    bbox <- sits:::.sits_config_satveg_bbox(cubes[1])
    expect_true(bbox["xmin"] < bbox["xmax"])

    expect_true(sits:::.config_memory_bloat() > 1)

    bands <- sits:::.sits_config_satveg_bands()
})

test_that("Show", {
    con <- file(paste0(tempdir(), "config.txt"))
    writeLines(capture.output(sits_config_show()), con)
    close(con)

    lin <- readLines(paste0(tempdir(), "config.txt"))

    expect_equal(lin[2], "R_memory_bloat          : 4")
    expect_true(grepl("bloat", lin[2]))
    expect_true(grepl("rstac", lin[6]))
})
