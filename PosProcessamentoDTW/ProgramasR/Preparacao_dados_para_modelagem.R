
#devtools::install_github('luizassis/wtss')
#devtools::install_github('vwmaus/dtwSat')
#install.packages(c('dtwclust', 'dplyr', 'ensurer', 'entropy', 'kohonen', 
#                   'ptw', 'purrr', 'purrrlyr', 'readr', 'rfUtilities', 'signal', 
#                   'roxygen2', 'tidyr', 'raster', 'snow'))

library(sits)

cerrado.tb <- sits_getdata(file = './inst/extdata/samples/cerrado.json')
sits_plot(cerrado.tb, type = 'together')

sits_plot(cerrado.tb[1,])
ts <- cerrado.tb[1,]$time_series
ts
patterns.tb <- sits_patterns(cerrado.tb)

sits_plot(patterns.tb, type = "patterns")
patterns2.tb <- sits_patterns(cerrado.tb, method = "dendogram", apply_gam = T)
sits_plot(patterns2.tb, type = "patterns")
patterns2.tb

matches.tb <- sits_TWDTW_matches(cerrado.tb, patterns.tb, span = 0, bands = c('ndvi', 'evi'))
dtw_ali <- do.call("rbind", lapply(seq_along(matches.tb$matches), function(i){
     out <- data.frame(id = i, matches.tb$matches[[i]][], ref = matches.tb$label[i])
     names(out)[6] <- "pred"
     out
}))

dtw_ali.tb <- tibble::as_tibble(dtw_ali)
dtw_ali.tb

classif.tb <- sits_TWDTW_classify(matches.tb, start_date = '2000-08-01', end_date = '2015-07-31',
                                  interval = '12 months')
sits_plot(classif.tb, type = 'alignments')



mato_grosso.tb <- sits_getdata(file = './inst/extdata/samples/matogrosso.json')
mato_grosso.tb


