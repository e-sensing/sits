################################################################################
# Script created during a presentation of the SITS package
# by Dr. Gilberto Camara
#
# National Institute for Space Research
# Sao Jose dos Campos, SP,  Brazil
# 2017/03/09
################################################################################
library(sits)


# Satellite bands used for the classification
satbands <- c("ndvi", "evi")


# Load and explore some sample data
cerrado.tb <- sits_getdata(file = "./inst/extdata/samples/cerrado.json")
cerrado.tb %>%
     sits_smooth() %>%
     sits_plot(type = "together")

# Plot a subset of the sample time series
sits_plot(cerrado.tb[1:30, ])


# Take the sample time series and organize them into training and validation sets
cerrado_l.tb <- dplyr::filter(cerrado.tb, label == "Cerrado")                   # "Cerrado" is an vegetation type from the Brazilian savanna
cerrado_p.tb <- dplyr::filter(cerrado.tb, label == "Pasture")
train1.tb <- cerrado_l.tb[1:20, ]
train2.tb <- cerrado_p.tb[1:20, ]
val1.tb <- cerrado_l.tb[-(1:20), ]
val2.tb <- cerrado_p.tb[-(1:20), ]
train.tb <- dplyr::bind_rows(train1.tb, train2.tb)
val.tb <- dplyr::bind_rows(val1.tb, val2.tb)


# Build classification patterns from the training data using a generalised
# additive model
patterns1.tb <- sits_patterns(train.tb)
sits_plot(patterns1.tb, type = "patterns")


# Do the classification of the samples using the patterns
# WARNING: This step could take some minutes
results.tb <- sits_TWDTW(series.tb = val.tb, patterns.tb = patterns1.tb,
                         bands = satbands)
sits_plot(results.tb[1:5, ], type = "classification")
(confusion.mx <- sits_assess(results.tb, 1))


# Split the training data into clusters in order to improve the classification
# results
clusters1.tb <- sits_cluster(train1.tb, n_clusters = 2, bands = satbands)
clusters2.tb <- sits_cluster(train2.tb, n_clusters = 2, bands = satbands)
clusters.tb <- dplyr::bind_rows(clusters1.tb, clusters2.tb)
clusters.tb[1,]$label <- "Cerrado"
clusters.tb[2,]$label <- "Pasture"
results1.tb <- sits_TWDTW(series.tb = val.tb, patterns.tb = clusters.tb,
                         bands = satbands)
(confusion2.mx <- sits_assess(results1.tb, 1))
