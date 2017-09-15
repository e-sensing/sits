library(sits)

context("Test sits package")

test_that("Can read RDS file and build patterns",{
    cerrado.tb <- readRDS(file = system.file("extdata/time_series/cerrado_test.rds", package="sits"))
    expect_equal (NROW(cerrado.tb), 149)
    patterns.tb <- sits_patterns(cerrado.tb)
    expect_equal (NROW(patterns.tb), 2)
})
