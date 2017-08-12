library (sits)

cerrado.tb <- sits_getdata(file = system.file("extdata/samples/cerrado.json", package="sits"))

patterns.tb <- sits_patterns(cerrado.tb)

matches.tb <- sits_TWDTW_matches(cerrado.tb, patterns.tb, bands = c("ndvi", "evi"), keep = TRUE)

sits_plot(matches.tb[1,], patterns.tb, type = "alignments")

sits_plot(matches.tb[1,], patterns.tb, type = "matches")

obj.svm <- sits_train_svm(matches.tb)

predict.tb <- sits_predict(matches.tb, obj.svm)

sits_accuracy(ref.vec = predict.tb$label, pred.vec = predict.tb$predicted)

classific.tb <- sits_TWDTW_classify(matches.tb, patterns.tb)

classific.tb

sits_plot(classific.tb[1,], patterns.tb, type = "classification")
