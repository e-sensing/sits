library (sits)
library (mxnet)

# Retrieve the set of samples for the Mato Grosso region (provided by EMBRAPA)
data(samples_MT_9classes)

# print the bands available
sits_bands (samples_MT_9classes)

# select the bands "ndvi", "evi", "nir", and "mir"
samples.tb <- sits_select (samples_MT_9classes, bands = c("ndvi","evi","nir","mir"))

# find the distance from the data

# estimate a multi-layer perceptron model
ml_model <- sits_train (samples.tb, ml_method = sits_mlp(hidden_node=c(32),
                                                           learning.rate = 0.0001,
                                                           activation = "sigmoid",
                                                           out_activation="softmax",
                                                           optimizer = "adam",
                                                           num.round = 5000,
                                                           array.batch.size = 32,
                                                           stop.metric = 0.95),
                        dist_method = sits_distances_from_data (shift = 0.0))

svm_model <- sits_train (samples.tb, ml_method = sits_svm(), dist_method = sits_distances_from_data(shift = 3.0))

# Retrieve a time series
data ("ts_2000_2016")

# select the bands "ndvi", "evi", "nir", and "mir"
point.tb <- sits_select (ts_2000_2016, bands = c("ndvi","evi","nir","mir"))

# classify the point
class1.tb <- sits_classify_model(point.tb, samples.tb, ml_model, dist_method = sits_distances_from_data(shift = 0.0))

# plot the classification
sits_plot (class1.tb)

# classify the point
class2.tb <- sits_classify_model(point.tb, samples.tb, svm_model, dist_method = sits_distances_from_data(shift = 3.0))

# plot the classification
sits_plot (class2.tb)


system.time({
    for(cname in cnames[-1]) {
        DT3[ , cname := gsub(" ", "_", DT3[[cname]]), with=FALSE]
    }
})
