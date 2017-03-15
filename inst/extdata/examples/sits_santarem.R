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
test1.tb <- test.tb[1:4,]
test2.tb <- test.tb[6:10,]
test3.tb <- test.tb[14:19,]


test1.tb <- dplyr::bind_rows(test1.tb,test2.tb)
test1.tb <- dplyr::bind_rows(test1.tb,test3.tb)

res.tb <- sits_TWDTW (test1.tb, isabel_pat.tb, bands)

assess <- sits_assess(res.tb)
