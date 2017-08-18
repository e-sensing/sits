library (sits)

prodes.tb <- sits_getdata(file = system.file("extdata/samples/prodes_226_64_novos.json", package="sits"))

patterns.tb <- sits_patterns(prodes.tb)

sits_plot (patterns.tb, type = "patterns")

matches.tb <- sits_TWDTW_matches(prodes.tb, patterns.tb, bands = c("ndvi", "evi"), keep = TRUE)

sits_plot(matches.tb[1,], patterns.tb, type = "alignments")

obj.svm <- sits_svm(matches.tb, cost = 100)

predict.tb <- sits_predict(matches.tb, obj.svm)

sits_accuracy(ref.vec = predict.tb$label, pred.vec = predict.tb$predicted)

sits_kfold_validate(cerrado.tb, folds = 3)
