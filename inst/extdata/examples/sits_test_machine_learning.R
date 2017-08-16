library (sits)

cerrado.tb <- sits_getdata(file = system.file("extdata/samples/cerrado.json", package="sits"))

patterns.tb <- sits_patterns(cerrado.tb)

matches.tb <- sits_TWDTW_matches(cerrado.tb, patterns.tb, bands = c("ndvi", "evi"), keep = TRUE)

sits_plot(matches.tb[1,], patterns.tb, type = "alignments")

obj.svm <- sits_train_svm(matches.tb)

predict.tb <- sits_predict(matches.tb, obj.svm)

sits_accuracy(ref.vec = predict.tb$label, pred.vec = predict.tb$predicted)

sits_kfold_validate(cerrado.tb, folds = 2)

power <- function(exponent) {
    function(x) {
        x ^ exponent
    }
}

svm_k <- function (kernel) {
    function (...) {
        sits_train_svm(kernel = kernel, ...) }
}

svm_s <- svm_k ("linear")

sits_kfold_validate(cerrado.tb, folds = 2, ml_method = svm_s)

classific.tb <- sits_TWDTW_classify(matches.tb, patterns.tb)

classific.tb

sits_plot(classific.tb[1,], patterns.tb, type = "classification")
