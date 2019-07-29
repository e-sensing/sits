context("Validation")
test_that("Does k-fold validate and build confusion matrix", {
    #skip_on_cran()
    data(cerrado_2classes)

    set.seed(12345)

    pred_ref.tb <- sits_kfold_validate(cerrado_2classes)
    invisible(capture.output(conf_matrix <- sits_conf_matrix(pred_ref.tb)))

    expect_true(NROW(pred_ref.tb) == NROW(cerrado_2classes))
    expect_true(all(names(pred_ref.tb) == c("predicted", "reference")))
    expect_true(dim(pred_ref.tb)[1] == 746)
    expect_equal(conf_matrix$overall[[1]], 0.9745308, tolerance = 1e-2) # Accuracy

    pred_ref.tb <- sits_kfold_validate(cerrado_2classes, folds = 2)
    invisible(capture.output(conf_matrix <- sits_conf_matrix(pred_ref.tb)))

    expect_true(NROW(pred_ref.tb) == NROW(cerrado_2classes))
    expect_true(all(names(pred_ref.tb) == c("predicted", "reference")))
    expect_true(dim(pred_ref.tb)[1] == 746)
    expect_equal(conf_matrix$overall[[1]], 0.963807, tolerance = 1e-2) # Accuracy

    cerrado_2classes$label = "NoClass"

    expect_error(sits_kfold_validate(cerrado_2classes), "sits_cross_validate: please provide a labelled set of time series")
})

