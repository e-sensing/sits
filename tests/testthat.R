library(testthat)
library(sits)
if (Sys.getenv("NOT_CRAN", unset = 0) == 1) {
    test_check("sits")
}
