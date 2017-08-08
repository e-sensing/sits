library (sits)

matogrosso.tb <- sits_getdata(file = system.file("extdata/samples/matogrosso.json", package="sits"))

patterns.tb <- sits_patterns(matogrosso.tb)

matches.tb <- sits_TWDTW_matches(matogrosso.tb, patterns.tb, bands = c("ndvi", "evi"))

model.fit <- sits_train_svm(matches.tb)
