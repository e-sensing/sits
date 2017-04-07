


#install library
library (sits)
# get a local data set
cerrado.tb <- sits_getdata("./inst/extdata/samples/cerrado.json")

# divide into training set and test set
cerrado_t1.tb <- cerrado.tb[1:20,]
cerrado_t2.tb <- cerrado.tb[721:746,]

train.tb <- dplyr::bind_rows(cerrado_t1.tb, cerrado_t2.tb)
# test samples
test.tb <- cerrado.tb[21:720,]
test1.tb <- cerrado.tb[21:22,]
# get the patterns using gam
patterns.tb <- sits_patterns(train.tb)
sits_plot (patterns.tb, type = "patterns")

bands <- c("ndvi", "evi")

result.tb <- sits_TWDTW (test1.tb, patterns.tb, bands)
