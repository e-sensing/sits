# load the sits library
library (sits)

# retrieve a set of samples
samples.tb <- readRDS(system.file("extdata/time_series/embrapa_mt.rds",
                                  package = "sits"))
# print the set of samples
samples.tb

#print the first time series
samples.tb[1,]$time_series

# prints the bands of the SITS tibble
sits_bands (samples.tb)
# shows the labels and their frequencies
sits_labels (samples.tb)
# a list for relabelling the samples
new_labels <- list("Cerrado"       = "Savanna",
                   "Pasture"       = "Grasslands",
                   "Soy_Corn"      = "Double_Cropping",
                   "Soy_Cotton"    = "Double_Cropping",
                   "Soy_Sunflower" = "Double_Cropping",
                   "Soy_Fallow"    = "Single_Cropping",
                   "Soy_Millet"    = "Single_Cropping",
                   "Fallow_Cotton" = "Single_Cropping")
# apply the sits_relabel function
samples2.tb <- sits_relabel(samples.tb, new_labels)
# view the result
sits_labels(samples2.tb)

# select the "ndvi" and "evi" bands
samples_2bands.tb <- sits_select(samples.tb, bands = c("ndvi", "evi"))
# select the samples with  "Cerrado and Pasture" labels
samples_2classes.tb <- dplyr::filter (samples_2bands.tb, label %in% c("Cerrado", "Pasture"))
sits_labels (samples_2classes.tb)


# filter some labels using the dplyr package
samples2.tb <- sits_select (samples.tb, label == "Cerrado", label == "Pasture")
sits_labels(samples2.tb)

