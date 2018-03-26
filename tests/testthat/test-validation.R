testthat::context("Validation")
testthat::test_that("Does k-fold validate and build confusion matrix", {
    testthat::skip_on_cran()
    data(cerrado_2classes)
    pred_ref.tb <-  sits_kfold_validate(cerrado_2classes, folds = 2)
    testthat::expect_true(NROW(pred_ref.tb) == NROW(cerrado_2classes))
})
