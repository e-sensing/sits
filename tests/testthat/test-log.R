context("Log")
test_that("Log", {
    #skip_on_cran()
    library(sits)
    sits:::.sits_log_error("abc")
    sits:::.sits_log_debug("def")

    err <- sits_log_show_errors()
    expect_true(as.logical(grep("abc", err[1,])))

    deb <- sits_log_show_debug()
    expect_true(as.logical(grep("def", deb[1,])))

    expect_error(sits:::.sits_log_csv(NULL), "Cannot save NULL CSV data")
    expect_error(sits:::.sits_log_data(NULL), "Cannot save NULL data")

    sits:::.sits_log_data(1, "abc.rda")

    dirname(sits:::sits.env$debug_file) %>%
        paste0("/abc.rda") %>%
        file.remove() %>%
        expect_true()
})
test_that("Mem", {
    m <- sits:::.sits_mem_used()
    expect_true (m > 0.0)

})

