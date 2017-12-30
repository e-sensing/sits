library(sits)

context("Test sits package")

test_that("Can read data file",{
    data("cerrado_2classes")
    expect_equal (NROW(cerrado_2classes), 746)
})
