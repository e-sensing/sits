library(keras)
#install_keras()

data("prodes_226_064")

samples.tb <- prodes_226_064

train_data.tb <- sits_distances(samples.tb, adj_fun = function(x) {BBmisc::normalize(x, method = "range")})

# get the labels of the data
labels <- as.vector(unique(train_data.tb$reference))

# create a named vector with integers match the class labels
int_labels <- c(1:length(labels))
names(int_labels) <- labels
n_labels <- length(labels)

# shuflle the data
train_data.tb <- dplyr::sample_frac(train_data.tb, 1.0)

train.x <- data.matrix(train_data.tb[, -(1:2)])
train.y <- unname(int_labels[as.vector(train_data.tb[, 2])]) - 1

# prepare data for training
train.y <- keras::to_categorical(train.y, n_labels)

n_input <- NCOL(train.x)

units <- c(400, 200, 100)
activation <- "relu"
dropout_rate <- c(0.4, 0.3, 0.2)

act_vec <- vector()

for (i in 1:length(units))
    if (length(activation) == 1)
        act_vec[i] <- activation

input_tensor <- keras::layer_input(shape = c(n_input))
output_tensor <-  input_tensor

for (i in 1:length(units)) {
    output_tensor <- keras::layer_dense(output_tensor, units = units[i], activation = act_vec[i])
    output_tensor <- keras::layer_dropout(output_tensor, rate = dropout_rate[i])
}

output_tensor <- keras::layer_dense(output_tensor, units = n_labels, activation = "softmax")

model <- keras::keras_model(input_tensor, output_tensor)

summary (model)

model %>% compile(
    loss = "categorical_crossentropy",
    optimizer = optimizer_adam(),
    metrics = c("accuracy")
)

history <- model %>% fit(
    train.x, train.y,
    epochs = 100, batch_size = 128,
    validation_split = 0.2
)

plot(history)

keras::evaluate(model, train.x, train.y)

samples_test.tb <- dplyr::sample_frac(samples.tb, size = 0.1)

test_data.tb <- sits_distances(samples_test.tb, adj_fun = function(x){BBmisc::normalize(x, method = "range")})

test.x <- data.matrix(test_data.tb[, -(1:2)])
test.y <- unname(int_labels[as.vector(test_data.tb[, 2])]) - 1
test.y <- keras::to_categorical(test.y, n_labels)

pred.y <- stats::predict(model, train.x[1:20,])


keras::evaluate(model, test.x, test.y)




data("prodes_226_064")


