# Plot XGB model

Plots trees in an extreme gradient boosting model.

## Usage

``` r
# S3 method for class 'xgb_model'
plot(x, ..., tree_idx = 1)
```

## Arguments

- x:

  Object of class "xgb_model".

- ...:

  Further specifications for
  [plot](https://e-sensing.github.io/sits/reference/plot.md).

- tree_idx:

  Number of tree to be plotted

## Value

A plot

## Author

Gilberto Camara, <gilberto.camara@inpe.br>

## Examples

``` r
if (sits_run_examples()) {
    # Retrieve the samples for Mato Grosso
    # train an extreme gradient boosting
    xgb_model <- sits_train(samples_modis_ndvi,
        ml_method = sits_xgboost()
    )
    plot(xgb_model)
}
```
