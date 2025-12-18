# Obtain statistics for all sample bands

Most machine learning algorithms require data to be normalized. This
applies to the "SVM" method and to all deep learning ones. To normalize
the predictors, it is necessary to extract the statistics of each band
of the samples. This function computes the 2 of the distribution of each
band of the samples. This values are used as minimum and maximum values
in the normalization operation performed by the sits_pred_normalize()
function.

## Usage

``` r
sits_stats(samples)
```

## Arguments

- samples:

  Time series samples uses as training data.

## Value

A list with the 2 training data.

## Author

Gilberto Camara, <gilberto.camara@inpe.br>

## Examples

``` r
if (sits_run_examples()) {
    stats <- sits_stats(samples_modis_ndvi)
}
```
