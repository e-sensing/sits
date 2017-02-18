# satellite image time series
# install_github("gilbertocamara/sits")
library(sits)

# first, get information about the WTSS (web time series service)
# see WTSS paper for more information ("Web Services for Big Data")
sits_infoWTSS(URL = "http://www.dpi.inpe.br/tws/wtss")

# then, configure the WTSS service
sits_configWTSS (URL = "http://www.dpi.inpe.br/tws/wtss",
                 coverage = "mod13q1_512",
                 bands = c("ndvi", "evi", "nir"))

# select samples for pasture and savanna
cerrado.tb <- sits_fromJSON ("./data/Samples/cerrado.json")

patterns.tb <- sits_patterns(cerrado.tb)



#select only savanna samples
savanna.tb <- dplyr::filter (cerrado.tb, label == "Savanna")

sits_plot(savanna.tb, type="together")

start_date <- savanna.tb[1,]$start_date
end_date   <- savanna.tb[1,]$end_date
freq <- 8

ts <- savanna.tb %>%
     sits_select ("ndvi") %>%
     sits_align(start_date) %>%
     .$time_series

ts2 <- ts %>%
     reshape2::melt (id.vars = "Index") %>%
     dplyr::select (Index, value) %>%
     dplyr::transmute (x = Index, y = value) %>%
     dplyr::transmute (x = as.numeric(x), y)

formula <- y ~ s(x)

# Get formula variables
vars <-  all.vars(formula)

fit <-  gam(data = ts2, formula = formula)

pred_time = seq(from = as.Date(start_date), to = as.Date(end_date), by = freq)

time <- data.frame(as.numeric(pred_time))
names(time) = vars[2]
pat <- predict.gam(fit, newdata = time)

# read a pattern table from a JSON file
patterns.tb <- sits_fromJSON ("./data/patterns/patterns_Rodrigo_8classes_6bands.json")

# plot patterns
sits_plot (patterns.tb, type = "patterns")

