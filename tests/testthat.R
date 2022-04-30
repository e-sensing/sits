library(testthat)
library(sits)
if (sits_active_tests()) {
    test_check("sits")
}
