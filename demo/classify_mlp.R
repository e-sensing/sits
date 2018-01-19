library (sits)
library (keras)

# Retrieve the set of samples for the Mato Grosso region (provided by EMBRAPA)
data("cerrado_13classes_modis_col6")

# select the bands "ndvi", "evi", "nir", and "mir"
samples.tb <- sits_select(cerrado_13classes_modis_col6, bands = c("ndvi","evi", "nir", "mir"))

# find the distance from the data
distances.tb <- sits_distances(samples.tb, adj_fun  = function(x){BBmisc::normalize(x, method = "range")})


ml_model = sits_deeplearning(distances.tb,
                              units = c(400,200,100),
                              activation = "relu",
                              dropout_rates = c(0.4, 0.3, 0.2),
                              optimizer = keras::optimizer_adam(),
                              epochs = 100,
                              batch_size = 128,
                              validation_split = 0.2)


# Retrieve a time series
data("ts_2000_2016")

# select the bands "ndvi", "evi", "nir", and "mir"
point.tb <- sits_select(ts_2000_2016, bands = c("ndvi","evi","nir", "mir"))

# classify the point
class.tb <- sits_classify_model(point.tb, samples.tb, ml_model, adj_fun = function(x) {BBmisc::normalize(x, method = "range")})

# plot the classification
sits_plot(class.tb)
