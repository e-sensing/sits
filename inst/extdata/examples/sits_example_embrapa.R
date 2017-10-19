embrapa.tb <- readRDS (
    system.file(file = "extdata/time_series/embrapa_mt.rds", package = "sits"))
embrapa.tb

# select two bands
embrapa2.tb <- sits_select (embrapa.tb, bands = c("ndvi", "evi"))

#select WTSS server
URL <- "http://www.dpi.inpe.br/tws/wtss"

# Which coverages are in WTSS
sits_infoWTSS(URL)
# select the MOD13Q1 coverage
coverage <- "mod13q1_512"
# Find information about coverage
mod13q1.tb <- sits_coverageWTSS(URL, coverage)

# get coverage timeline
timeline <- sits_timeline (mod13q1.tb)

# get the patterns of the samples
patterns.tb <- sits_patterns (embrapa2.tb, timeline)

# plot the patterns
sits_plot (patterns.tb, type = "patterns")

#plot the samples of the Cerrado class
sits_plot (sits_select(embrapa2.tb, label == "Cerrado"), type = "together")



