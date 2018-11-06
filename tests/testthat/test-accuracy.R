context("Accuracy")
test_that("XLS", {
    data(cerrado_2classes)
    pred_ref.tb <- sits_kfold_validate(cerrado_2classes, folds = 2)
    invisible(capture.output(conf.mx <- sits_conf_matrix(pred_ref.tb)))
    results <- list()
    conf.mx$name <- "confusion_matrix"
    results[[length(results) + 1]] <- conf.mx
    sits_toXLSX(results, file = "confusion_matrix.xlsx")

    expect_true(file.remove("confusion_matrix.xlsx"))
})

test_that("Accuracy", {
    coverage_wtss <- sits_coverage(service = "WTSS-INPE", name = "MOD13Q1")

    data.tb <- sits_getdata(file = system.file("extdata/samples/samples_matogrosso.csv", package = "sits"),
                        coverage = coverage_wtss,
                        bands = c("ndvi", "evi"),
                        .n_save = 0)

    data("cerrado_2classes")
    svm_model <- sits_train(cerrado_2classes, ml_method = sits_svm())
    class.tb <- sits_classify(data.tb, svm_model)

    names(class.tb)

    invisible(capture.output(conf.mx <- sits_conf_matrix(class.tb)))

    expect_equal(length(names(conf.mx)), 6)

    class.acc <- sits_accuracy_area(class.tb)

    expect_equal(class.acc@accuracySummary$conf.int, 0.95)
    expect_true(class.acc@accuracySummary$OverallAccuracy[[1]] <= 1)
})
