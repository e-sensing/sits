library(sits)

context("Test sits package")

test_that("Can read compressed JSON files and build patterns",{
    cerrado.tb <- sits_getdata(file = system.file("extdata/samples/cerrado_test.json.gz", package="sits"))
    expect_equal (NROW(cerrado.tb), 75)
    patterns.tb <- sits_patterns(cerrado.tb)
    expect_equal (NROW(patterns.tb), 2)
})
