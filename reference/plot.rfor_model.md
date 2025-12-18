# Plot Random Forest model

Plots the important variables in a random forest model.

## Usage

``` r
# S3 method for class 'rfor_model'
plot(x, y, ...)
```

## Arguments

- x:

  Object of class "rf_model".

- y:

  Ignored.

- ...:

  Further specifications for
  [plot](https://e-sensing.github.io/sits/reference/plot.md).

## Value

A random forest object.

## Note

Please refer to the sits documentation available in
<https://e-sensing.github.io/sitsbook/> for detailed examples.

## Author

Gilberto Camara, <gilberto.camara@inpe.br>

## Examples

``` r
if (sits_run_examples()) {
    # Retrieve the samples for Mato Grosso
    # train a random forest model
    rf_model <- sits_train(samples_modis_ndvi, ml_method = sits_rfor())
    # plot the model
    plot(rf_model)
}
```
