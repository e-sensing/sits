# Normalize predictor values

Most machine learning algorithms require data to be normalized. This
applies to the "SVM" method and to all deep learning ones. To normalize
the predictors, it is required that the statistics per band for each
sample have been obtained by the "sits_stats" function.

## Usage

``` r
sits_pred_normalize(pred, stats)
```

## Arguments

- pred:

  X-Y predictors: a data.frame with one row per sample.

- stats:

  Values of time series for Q02 and Q98 of the data (list of numeric
  values with two elements)

## Value

A data.frame with normalized predictor values

## Author

Gilberto Camara, <gilberto.camara@inpe.br>

## Examples

``` r
if (sits_run_examples()) {
    stats <- sits_stats(samples_modis_ndvi)
    pred <- sits_predictors(samples_modis_ndvi)
    pred_norm <- sits_pred_normalize(pred, stats)
}
```
