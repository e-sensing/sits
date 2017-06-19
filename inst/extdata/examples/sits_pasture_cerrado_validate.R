# R script 2017-03-09
# sits package by Gilberto Câmara
# https://github.com/gilbertocamara/sits

# cross-validation for FAO Bolivia Study

library(sits)

# retrieve a set of samples from a JSON file
cerrado.tb <- sits_getdata(file = system.file("extdata/samples/cerrado.json", package="sits"))

# perform accuracy assessment - já foi feito
valid2 <- sits_validate (cerrado.tb, method = "gam", times = 10, perc = 0.5, file = "./valid1.json", .multicores = 3)

