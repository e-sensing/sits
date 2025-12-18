# Obtain a fraction of the predictors data frame

Many machine learning algorithms (especially deep learning) use part of
the original samples as test data to adjust its hyperparameters and to
find an optimal point of convergence using gradient descent. This
function extracts a fraction of the predictors to serve as test values
for the deep learning algorithm.

## Usage

``` r
sits_pred_sample(pred, frac)
```

## Arguments

- pred:

  X-Y predictors: a data.frame with one row per sample.

- frac:

  Fraction of the X-Y predictors to be extracted

## Value

A data.frame with the chosen fraction of the X-Y predictors.

## Author

Gilberto Camara, <gilberto.camara@inpe.br>

## Examples

``` r
if (sits_run_examples()) {
    pred <- sits_predictors(samples_modis_ndvi)
    pred_frac <- sits_pred_sample(pred, frac = 0.5)
}
```
