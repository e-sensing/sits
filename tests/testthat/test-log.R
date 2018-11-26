context("Log")
test_that("Log", {
    #skip_on_cran()

    sits:::.sits_log_error("abc")
    sits:::.sits_log_debug("def")

    expect_error(sits:::.sits_log_csv(NULL), "Cannot save NULL CSV data")
    expect_error(sits:::.sits_log_data(NULL), "Cannot save NULL data")

    sits:::.sits_log_data(1, "abc.rda")

    dirname(sits:::sits.env$debug_file) %>%
        paste0("/abc.rda") %>%
        file.remove() %>%
        expect_true()
})
