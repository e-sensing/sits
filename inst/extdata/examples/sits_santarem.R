# Examples
# devtools::install_gitbub("gilbertocamara/sits")
library(sits)

# configure the WTSS service
URL <- "http://www.dpi.inpe.br/tws/wtss"
cov <- "mod13q1_512"
bands <- c("ndvi", "evi", "nir")

train.tb <- sits_getdata (file = "./inst/extdata/samples/primeiroConjunto.csv",
                          URL = URL, coverage = cov, bands = bands)

isabel_pat.tb <- sits_patterns (train.tb)

sits_plot(isabel_pat.tb, type = "patterns")

test.tb <- sits_getdata (file = "./inst/extdata/samples/segundoConjunto.csv",
                         URL = URL, coverage = cov, bands = bands)

res.tb <- sits_TWDTW (test.tb, isabel_pat.tb, bands)

sits_plot(res.tb, type = "classification")
