library (sits)
library (mxnet)

data("samples_MT_9classes")

samples.tb <- sits_select(samples_MT_9classes, bands = c("ndvi", "evi", "nir"))


# get the labels of the data
labels <- sits_labels(samples.tb)$label

# create a named vector with integers match the class labels
int_labels <- c(1:length(labels))
names (int_labels) <- labels

# splits the data into 2 groups
samples.tb$folds <- caret::createFolds(samples.tb$label, k = 5, returnTrain = FALSE, list = FALSE)

samples_train.tb <- samples.tb[samples.tb$folds != 5,]
samples_test.tb  <- samples.tb[samples.tb$folds == 5,]

distances_train.tb <- sits_distances(samples_train.tb)
distances_test.tb  <- sits_distances(samples_test.tb)

distances_train.tb <- dplyr::sample_frac(distances_train.tb, 1.0)

train.x <- data.matrix (distances_train.tb[, -(1:2)])
train.y <- unname (int_labels [distances_train.tb[, 2]]) -1

test.x <- data.matrix (distances_test.tb[, -(1:2)])
test.y <- unname (int_labels [distances_test.tb[, 2]]) -1

# The number of hidden neurons should be between the size of the input layer and the size of the output layer.
# The number of hidden neurons should be 2/3 the size of the input layer, plus the size of the output layer.
# The number of hidden neurons should be less than twice the size of the input layer.

mx.set.seed(0)
model <- mx.mlp(train.x, train.y, hidden_node=c(400,200,100), out_node=9,
                activation = "sigmoid", out_activation="softmax", optimizer = "adam",
                num.round=5000, array.batch.size=50, learning.rate=0.001,
                eval.metric=mx.metric.accuracy,
                epoch.end.callback = mx.callback.early.stop(train.metric = .98, maximize = TRUE))

preds = predict(model, test.x)

pred.labels <- max.col (t(preds))
table (pred.labels, test.y)
