# Histogram uncertainty cubes

This is a generic function. Parameters depend on the specific type of
input.

## Usage

``` r
# S3 method for class 'uncertainty_cube'
hist(x, ..., tile = x[["tile"]][[1L]], size = 100000L)
```

## Arguments

- x:

  Object of class "variance_cube"

- ...:

  Further specifications for
  [hist](https://rdrr.io/r/graphics/hist.html).

- tile:

  Tile to be summarized

- size:

  Sample size

## Value

A histogram of a uncertainty cube

## Author

Gilberto Camara, <gilberto.camara@inpe.br>

## Examples

``` r
if (sits_run_examples()) {
    # create a data cube from local files
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir
    )
    # create a random forest model
    rfor_model <- sits_train(samples_modis_ndvi, sits_rfor())
    # classify a data cube
    probs_cube <- sits_classify(
        data = cube, ml_model = rfor_model, output_dir = tempdir()
    )
    uncert_cube <- sits_uncertainty(
        cube = probs_cube,
        output_dir = tempdir()
    )
    hist(uncert_cube)
}
```
