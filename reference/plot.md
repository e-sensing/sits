# Plot time series and data cubes

This is a generic function. Parameters depend on the specific type of
input. See each function description for the required parameters.

- sits tibble: see `plot.sits`

- patterns: see
  [`plot.patterns`](https://e-sensing.github.io/sits/reference/plot.patterns.md)

- classified time series: see
  [`plot.predicted`](https://e-sensing.github.io/sits/reference/plot.predicted.md)

- raster cube: see
  [`plot.raster_cube`](https://e-sensing.github.io/sits/reference/plot.raster_cube.md)

- SAR cube: see
  [`plot.sar_cube`](https://e-sensing.github.io/sits/reference/plot.sar_cube.md)

- DEM cube: see
  [`plot.dem_cube`](https://e-sensing.github.io/sits/reference/plot.dem_cube.md)

- vector cube: see
  [`plot.vector_cube`](https://e-sensing.github.io/sits/reference/plot.vector_cube.md)

- classification probabilities: see
  [`plot.probs_cube`](https://e-sensing.github.io/sits/reference/plot.probs_cube.md)

- classification uncertainty: see
  [`plot.uncertainty_cube`](https://e-sensing.github.io/sits/reference/plot.uncertainty_cube.md)

- uncertainty of vector cubes: see
  [`plot.uncertainty_vector_cube`](https://e-sensing.github.io/sits/reference/plot.uncertainty_vector_cube.md)

- classified cube: see
  [`plot.class_cube`](https://e-sensing.github.io/sits/reference/plot.class_cube.md)

- classified vector cube: see
  [`plot.class_vector_cube`](https://e-sensing.github.io/sits/reference/plot.class_vector_cube.md)

- dendrogram cluster: see
  [`plot.sits_cluster`](https://e-sensing.github.io/sits/reference/plot.sits_cluster.md)

- SOM map: see
  [`plot.som_map`](https://e-sensing.github.io/sits/reference/plot.som_map.md)

- SOM evaluate cluster: see
  [`plot.som_evaluate_cluster`](https://e-sensing.github.io/sits/reference/plot.som_evaluate_cluster.md)

- geo-distances: see
  [`plot.geo_distances`](https://e-sensing.github.io/sits/reference/plot.geo_distances.md)

- random forest model: see
  [`plot.rfor_model`](https://e-sensing.github.io/sits/reference/plot.rfor_model.md)

- xgboost model: see
  [`plot.xgb_model`](https://e-sensing.github.io/sits/reference/plot.xgb_model.md)

- torch ML model: see
  [`plot.torch_model`](https://e-sensing.github.io/sits/reference/plot.torch_model.md)

Plots the time series to be used for classification

## Usage

``` r
# S3 method for class 'sits'
plot(x, y, ..., together = TRUE)
```

## Arguments

- x:

  Object of class "sits".

- y:

  Ignored.

- ...:

  Further specifications for plot.

- together:

  A logical value indicating whether the samples should be plotted
  together.

## Value

A series of plot objects produced by ggplot2 showing all time series
associated to each combination of band and label, and including the
median, and first and third quartile ranges.

## Author

Gilberto Camara, <gilberto.camara@inpe.br>

## Examples

``` r
if (sits_run_examples()) {
    # plot sets of time series
    plot(cerrado_2classes)
}
```
