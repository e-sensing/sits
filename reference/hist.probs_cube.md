# histogram of prob cubes

This is a generic function. Parameters depend on the specific type of
input.

## Usage

``` r
# S3 method for class 'probs_cube'
hist(x, ..., tile = x[["tile"]][[1L]], label = NULL, size = 100000L)
```

## Arguments

- x:

  Object of classes "raster_cube".

- ...:

  Further specifications for
  [summary](https://rdrr.io/r/base/summary.html).

- tile:

  Tile to be shown

- label:

  Label to be shown

- size:

  Number of cells to be sampled

## Value

A histogram of one label of a probability cube.

## Author

Gilberto Camara, <gilberto.camara@inpe.br>

## Examples

``` r
if (sits_run_examples()) {
    # create a data cube from local files
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    modis_cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir
    )
    rfor_model <- sits_train(samples_modis_ndvi, sits_rfor())
    probs_cube <- sits_classify(
        data = modis_cube,
        ml_model = rfor_model,
        output_dir = tempdir()
    )
    hist(probs_cube, label = "Forest")
}
```
