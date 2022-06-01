library(testthat)
library(sits)
if (sits_run_tests()) {
    test_check("sits")
}
