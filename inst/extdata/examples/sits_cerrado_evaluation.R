# R script 2017-03-09
# sits package by Gilberto Câmara
# https://github.com/gilbertocamara/sits

# comparing patterns obtained from Generalized Additive Models and from Clustering
library(sits)

# retrieve a set of samples from a JSON file
cerrado.tb <- sits_getdata(file = system.file("extdata/samples/cerrado6.json", package="sits"))

# perform accuracy assessment
cm <- sits_validate (cerrado.tb, method = "centroids", bands = c("ndvi","evi", "nir"), times = 50, perc = 0.1)
