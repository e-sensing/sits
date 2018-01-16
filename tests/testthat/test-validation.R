testthat::context("Validation")
testthat::test_that("Does k-fold validate and build confusion matrix", {
    data(cerrado_2classes)
    pred_ref.tb <-  sits_kfold_validate(cerrado_2classes, folds = 2)
    conf.mx <- sits_conf_matrix(pred_ref.tb)

    testthat::expect_true(NROW(pred_ref.tb) == NROW(cerrado_2classes))
    testthat::expect_true(NROW(conf.mx$table) > 1)
})
