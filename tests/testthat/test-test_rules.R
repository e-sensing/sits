test_that("Tests start no process through the system or a shell", {
    # a test that needs a new R session runs its code in a sits worker, which
    # gets the library paths of the main process; a process of the system
    # reads the settings of the user and can load another sits (AGENTS.md)
    calls <- c("system", "system2", "shell", "pipe")
    packages <- c("processx", "callr", "sys")
    files <- list.files(
        testthat::test_path(), pattern = "[.]R$", full.names = TRUE
    )
    found <- unlist(lapply(files, function(file) {
        data <- utils::getParseData(parse(file, keep.source = TRUE))
        hit <- (data[["token"]] == "SYMBOL_FUNCTION_CALL" &
            data[["text"]] %in% calls) |
            (data[["token"]] == "SYMBOL_PACKAGE" &
                data[["text"]] %in% packages)
        if (!any(hit)) {
            return(NULL)
        }
        paste0(basename(file), ":", data[["line1"]][hit], " ",
            data[["text"]][hit])
    }))
    expect_identical(found, NULL)
})
