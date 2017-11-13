library(sits)

context("Test sits package")

test_that("Can read RDS file",{
    cerrado.tb <- readRDS(file = system.file("extdata/time_series/cerrado_2classes.rds", package="sits"))
    expect_equal (NROW(cerrado.tb), 746)
})
