# R script 2017-03-09
# sits package by Gilberto Câmara
# https://github.com/gilbertocamara/sits

# comparing patterns obtained from Generalized Additive Models and from Clustering
library(sits)

# retrieve a set of samples from a JSON file

cerrado.tb <- sits_getdata(file = "./inst/extdata/samples/cerrado.json")

samples.tb <- sits_label_perc(cerrado.tb, perc = 0.1)

patterns1.tb <- sits_patterns (samples.tb)
sits_plot (patterns1.tb, type = "patterns")

bands <- c("ndvi", "evi")
results.tb <- sits_TWDTW (cerrado.tb[1:3,], patterns1.tb, bands)

patterns2.tb <- sits_patterns (cerrado.tb, method = "dendogram", n_clusters = 2)
sits_plot (patterns2.tb, type = "patterns")

val <- sits_validate(cerrado.tb)

cerrado_s.tb <- sits_smooth(cerrado.tb)

# use a set of patterns
patterns = sits_getdata(file = "./inst/extdata/patterns/patterns_MatoGrosso.json")

patterns1.tb <- dplyr::filter (patterns, "Pasture" %in% label)



