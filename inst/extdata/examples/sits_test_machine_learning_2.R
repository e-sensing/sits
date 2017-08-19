library (sits)

embrapa.tb <- sits_getdata(file = system.file("extdata/samples/embraba_cerrado_forest.json", package="sits"))

bands <-  c("ndvi", "evi", "mir")

embrapa.tb <- sits_select(embrapa.tb, bands)

patterns.tb <- sits_patterns(embrapa.tb)

sits_plot (patterns.tb, type = "patterns")

matches.tb <- sits_TWDTW_matches(embrapa.tb, patterns.tb, bands = bands, keep = TRUE)

obj.svm <- sits_svm(matches.tb)

predict.tb <- sits_predict(matches.tb, obj.svm)

caret::confusionMatrix(predict.tb$predicted, predict.tb$label)

sits_accuracy(ref.vec = predict.tb$label, pred.vec = predict.tb$predicted)

sits_kfold_validate(embrapa.tb, folds = 5)
