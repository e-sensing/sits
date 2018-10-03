testthat::context("Validation")
testthat::test_that("Does k-fold validate and build confusion matrix", {
    testthat::skip_on_cran()
    data(cerrado_2classes)

    set.seed(12345)

    pred_ref.tb <- sits_kfold_validate(cerrado_2classes)
    conf_matrix <- sits_conf_matrix(pred_ref.tb)

    testthat::expect_true(NROW(pred_ref.tb) == NROW(cerrado_2classes))
    testthat::expect_true(all(names(pred_ref.tb) == c("predicted", "reference")))
    testthat::expect_true(dim(pred_ref.tb)[1] == 746)
    testthat::expect_equal(conf_matrix$overall[[1]], 0.9745308, tolerance = 1e-5) # Accuracy

    pred_ref.tb <- sits_kfold_validate(cerrado_2classes, folds = 2)
    conf_matrix <- sits_conf_matrix(pred_ref.tb)

    testthat::expect_true(NROW(pred_ref.tb) == NROW(cerrado_2classes))
    testthat::expect_true(all(names(pred_ref.tb) == c("predicted", "reference")))
    testthat::expect_true(dim(pred_ref.tb)[1] == 746)
    testthat::expect_equal(conf_matrix$overall[[1]], 0.963807, tolerance = 1e-5) # Accuracy

    cerrado_2classes$label = "NoClass"

    testthat::expect_error(sits_kfold_validate(cerrado_2classes), "sits_cross_validate: please provide a labelled set of time series")
    testthat::expect_error(sits_kfold_validate(1), "data input is not a valid SITS tibble")
})
