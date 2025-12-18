# Plot confusion matrix

Plot a bar graph with informations about the confusion matrix

## Usage

``` r
# S3 method for class 'sits_accuracy'
plot(x, y, ..., title = "Confusion matrix")
```

## Arguments

- x:

  Object of class "plot.sits_accuracy".

- y:

  Ignored.

- ...:

  Further specifications for
  [plot](https://e-sensing.github.io/sits/reference/plot.md).

- title:

  Title of plot.

## Value

A plot object produced by the ggplot2 package containing color bars
showing the confusion between classes.

## Author

Gilberto Camara <gilberto.camara@inpe.br>

## Examples

``` r
if (sits_run_examples()) {
    # show accuracy for a set of samples
    train_data <- sits_sample(samples_modis_ndvi, frac = 0.5)
    test_data <- sits_sample(samples_modis_ndvi, frac = 0.5)
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
