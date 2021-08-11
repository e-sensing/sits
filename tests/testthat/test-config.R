test_that("Show", {
    con <- file(paste0(tempdir(), "config.txt"))
    writeLines(capture.output(sits_config_show()), con)
    close(con)

    lin <- readLines(paste0(tempdir(), "config.txt"))

    expect_equal(lin[2], "R_memory_bloat          : 4")
    expect_true(grepl("bloat", lin[2]))
    expect_true(grepl("rstac", lin[6]))
})
