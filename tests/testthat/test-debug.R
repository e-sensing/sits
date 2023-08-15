test_that("debug", {
    # start debugging
    flag <- .debug()
    expect_false(flag)
    expect_false(sits_env[["debug_flag"]])
    .debug(flag = TRUE, output_dir = tempdir())
    expect_true(sits_env[["debug_flag"]])
    .debug_log(event = "test", key = "debug_log", value = "start")
    .debug_log(event = "test", key = "debug_log", value = "end")
    log_file <- list.files(paste0(tempdir(), "/.sits"))
    log_csv <- utils::read.csv(paste0(tempdir(), "/.sits/", log_file))
    expect_true(all(names(log_csv) %in% c("date_time", "pid", "event",
                                      "elapsed_time", "mem_used",
                                      "max_mem_used", "key", "value")))
    expect_equal(log_csv[1, "value"], " start")
    expect_equal(log_csv[2, "value"], " end")
    sits_env[["debug_flag"]] <- NULL
    flag <- .debug()
    expect_false(flag)
    .debug(flag = FALSE)

})
