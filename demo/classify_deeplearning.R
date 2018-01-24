library(sits)
library(keras)

# Retrieve the set of samples for the Mato Grosso region (provided by EMBRAPA)
data("cerrado_13classes_modis_col5")

# select the bands "ndvi", "evi", "nir", and "mir"
samples.tb <- sits_select(cerrado_13classes_modis_col5, bands = c("ndvi","evi", "nir", "mir"))

# find the distance from the data
distances.tb <- sits_distances(samples.tb, adj_fun  = function(x) {identity(x)})


ml_model = sits_deeplearning(distances.tb,
                             units            = c(400, 200, 100, 50),
                             activation       = 'relu',
                             dropout_rates    = c(0.40, 0.30, 0.20, 0.10),
                             optimizer = keras::optimizer_adam(),
                             epochs = 300,
                             batch_size = 128,
                             validation_split = 0.2)

sits_keras_diagnostics()


conv_model <- sits_convnets(distances.tb,
                            filters          = c(100, 100, 100),
                            activation       = 'relu',
                            kernels          = c(5, 5, 5),
                            optimizer        = keras::optimizer_adam(),
                            epochs           = 250,
                            batch_size       = 128,
                            validation_split = 0.2)


# Retrieve a time series
data("ts_2000_2016")

# select the bands "ndvi", "evi", "nir", and "mir"
point.tb <- sits_select(ts_2000_2016, bands = c("ndvi","evi","nir", "mir"))

# classify the point
class.tb <- sits_classify_model(point.tb, samples.tb, ml_model, adj_fun = function(x) {identity(x)})

# plot the classification
sits_plot(class.tb)
