# Plot confusion matrix

Plot a table with informations about the confusion matrix or the
accuracy metrics

## Usage

``` r
# S3 method for class 'sits_accuracy'
plot(x, y, ..., type = "confusion_matrix")
```

## Arguments

- x:

  Object of class "plot.sits_accuracy".

- y:

  Ignored.

- ...:

  Further specifications for
  [plot](https://e-sensing.github.io/sits/reference/plot.md).

- type:

  Type of plot (either "confusion_matrix" or "metrics")

## Value

Called for side package containing color bars showing the confusion
between classes.

## Author

Gilberto Camara <gilberto.camara@inpe.br>

## Examples

``` r
if (sits_run_examples()) {
    # select a set of samples
    samples <- samples_modis_ndvi
    # index samples to split train/test
    samples[["sample_idx"]] <- 1:nrow(samples)
    # select training data
    train_data <- sits_sample(samples, frac = 0.8)
    # select test data
    sel <- !(samples[["sample_idx"]]
             %in% train_data[["sample_idx"]])
    test_data <- samples[sel, ]
    # compute a random forest model
    rfor_model <- sits_train(train_data, sits_rfor())
    # classify training points
    points_class <- sits_classify(
        data = test_data, ml_model = rfor_model
    )
    # calculate accuracy
    acc <- sits_accuracy(points_class)
    # plot accuracy
    plot(acc)
}
```
