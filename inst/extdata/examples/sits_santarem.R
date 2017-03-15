# Examples
# devtools::install_gitbub("gilbertocamara/sits")
library(sits)

# configure the WTSS service
URL <- "http://www.dpi.inpe.br/tws/wtss"
#what is contained in the WTSS service
sits_infoWTSS(URL)
cov <- "mod13q1_512"
sits_coverageWTSS(URL, cov)

bands <- c("ndvi", "evi", "nir")

long <- -54.7030
lat <- -2.7462

point.tb <- sits_getdata (longitude = long, latitude = lat,
                          URL = URL, coverage = cov, bands = bands)
point.tb %>%
     sits_select("ndvi") %>%
     sits_smooth() %>%
     sits_plot()

res.tb <- sits_TWDTW (point.tb, isabel_pat.tb, bands, keep = TRUE, span = 180)

sits_plot(res.tb, type = "classification")
sits_plot(res.tb, type = "alignments")
sits_plot(res.tb, type = "matches", label = "Soja-comercial", k=4)





train.tb <- sits_getdata (file = "./inst/extdata/samples/primeiroConjunto.csv",
                          URL = URL, coverage = cov, bands = bands)

isabel_pat.tb <- sits_patterns (train.tb)



sits_plot(isabel_pat.tb, type = "patterns")

test.tb <- sits_getdata (file = "./inst/extdata/samples/segundoConjunto.csv",
                         URL = URL, coverage = cov, bands = bands)
res.tb <- sits_TWDTW (test.tb[15,], isabel_pat.tb, bands)

sits_plot(res.tb, type = "classification")

