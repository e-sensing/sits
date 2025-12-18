# Summarize accuracy matrix for training data

This is a generic function. Parameters depend on the specific type of
input.

## Usage

``` r
# S3 method for class 'sits_accuracy'
summary(object, ...)
```

## Arguments

- object:

  Object of class "sits_accuracy".

- ...:

  Further specifications for
  [summary](https://rdrr.io/r/base/summary.html).

## Value

A summary of the sample accuracy

## Author

Gilberto Camara, <gilberto.camara@inpe.br>

## Examples

``` r
if (sits_run_examples()) {
    data(cerrado_2classes)
    # split training and test data
    train_data <- sits_sample(cerrado_2classes, frac = 0.5)
    test_data <- sits_sample(cerrado_2classes, frac = 0.5)
    # train a random forest model
    rfor_model <- sits_train(train_data, sits_rfor())
    # classify test data
    points_class <- sits_classify(
        data = test_data,
        ml_model = rfor_model
    )
    # measure accuracy
    acc <- sits_accuracy(points_class)
    summary(acc)
}
```
