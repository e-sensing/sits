library (sits)
library (kohonen)

# load patterns from savanna and pasture (expert = Rodrigo)
cerrado.tb <- sits_fromJSON(system.file("extdata/samples/cerrado.json", package="sits"))

time_series <- cerrado.tb$time_series

time.df <- data.frame()

for (i in 1:length(time_series)) {
     col_ndvi <- time_series[[i]]$ndvi
     row_ndvi <- t(col_ndvi)
     time.df <- dplyr::bind_rows (time.df, as.data.frame(row_ndvi))
}
samples.mt <- as.matrix(time.df)
