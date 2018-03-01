library(sits)
library(keras)

# Retrieve the set of samples for the Mato Grosso region (provided by EMBRAPA)
data("samples_MT_9classes")

# select the bands "ndvi", "evi", "nir", and "mir"
samples.tb <- sits_select(samples_MT_9classes, bands = c("ndvi","evi", "nir", "mir"))


ml_model <-  sits_train(samples.tb,
                        ml_method = sits_deeplearning(
                             units            = c(512, 512, 512),
                             activation       = 'elu',
                             dropout_rates    = c(0.40, 0.40, 0.30),
                             optimizer = keras::optimizer_adam(),
                             epochs = 500,
                             batch_size = 128,
                             validation_split = 0.2),
                        adj_fun  = function(x) {identity(x)}
)

sits_keras_diagnostics()

# Retrieve a time series
data("point_MT_6bands")

# select the bands "ndvi", "evi", "nir", and "mir"
point.tb <- sits_select(point_MT_6bands, bands = c("ndvi","evi","nir", "mir"))

# classify the point
class.tb <- sits_classify(point.tb, samples.tb, ml_model, adj_fun = function(x) {identity(x)})

# plot the classification
sits_plot(class.tb)

