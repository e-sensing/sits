# Plot time series predictions

Given a sits tibble with a set of predictions, plot them. Useful to show
multi-year predictions for a time series.

## Usage

``` r
# S3 method for class 'predicted'
plot(x, y, ..., bands = "NDVI", palette = "Harmonic")
```

## Arguments

- x:

  Object of class "predicted".

- y:

  Ignored.

- ...:

  Further specifications for
  [plot](https://e-sensing.github.io/sits/reference/plot.md).

- bands:

  Bands for visualization.

- palette:

  HCL palette used for visualization in case classes are not in the
  default sits palette.

## Value

A plot object produced by ggplot2 showing the time series and its label.

## Note

This code is reused from the dtwSat package by Victor Maus.

## Author

Victor Maus, <vwmaus1@gmail.com>

Gilberto Camara, <gilberto.camara@inpe.br>

## Examples

``` r
if (sits_run_examples()) {
    # Retrieve the samples for Mato Grosso
    # train an svm model
    ml_model <- sits_train(samples_modis_ndvi, ml_method = sits_svm)
    # classify the point
    point_ndvi <- sits_select(point_mt_6bands, bands = "NDVI")
    point_class <- sits_classify(
        data = point_ndvi, ml_model = ml_model
    )
    plot(point_class)
}
```
