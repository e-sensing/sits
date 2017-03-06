# first, get information about the WTSS (web time series service)
# see WTSS paper for more information ("Web Services for Big Data")

URL <- "http://www.dpi.inpe.br/tws/wtss"


# then, configure the WTSS service
inpe <- sits_configWTSS (URL,
                         coverage = "chronos:modis:mod13q1_512",
                         bands = c("ndvi", "evi", "nir"))

# read sample information from a CSV archive,
# recover the time series from the WTSS service
# put data and metadata on a SITS table
embrapa.tb  <- sits_getdata(file = "./inst/extdata/samples/damien.csv", wtss = inpe, n_max = 20)

embrapa.tb %>%
     sits_select (bands = "evi") %>%
     sits_plot ()

# classify point using TWDTW

# read a pattern table from a JSON file
patterns.tb <- sits_getdata ("./inst/extdata/patterns/patterns_8classes_6bands.json")
# plot patterns
sits_plot (patterns.tb, type = "patterns")

# classify samples using TWDTW
bands <- c("ndvi", "evi", "nir")
results.tb <- sits_TWDTW(embrapa.tb, patterns.tb, bands)

sits_plot(results.tb[1,], type = "classification")
sits_plot(results.tb[1,], type = "alignments")
