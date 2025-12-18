# Summarize variance cubes

This is a generic function. Parameters depend on the specific type of
input.

## Usage

``` r
# S3 method for class 'variance_cube'
summary(
  object,
  ...,
  intervals = 0.05,
  sample_size = 10000L,
  multicores = 2L,
  memsize = 2L,
  quantiles = c("75%", "80%", "85%", "90%", "95%", "100%")
)
```

## Arguments

- object:

  Object of class "class_cube"

- ...:

  Further specifications for
  [summary](https://rdrr.io/r/base/summary.html).

- intervals:

  Intervals to calculate the quantiles

- sample_size:

  The approximate size of samples will be extracted from the variance
  cube (by tile).

- multicores:

  Number of cores to summarize data (integer, min = 1, max = 2048).

- memsize:

  Memory in GB available to summarize data (integer, min = 1, max =
  16384).

- quantiles:

  Quantiles to be shown

## Value

A summary of a variance cube

## Author

Gilberto Camara, <gilberto.camara@inpe.br>

Felipe Carlos, <efelipecarlos@gmail.com>

Felipe Souza, <lipecaso@gmail.com>

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
    variance_cube <- sits_variance(
        data = probs_cube,
        output_dir = tempdir()
    )
    summary(variance_cube)
}
```
