# histogram of data cubes

This is a generic function. Parameters depend on the specific type of
input.

## Usage

``` r
# S3 method for class 'raster_cube'
hist(
  x,
  ...,
  tile = x[["tile"]][[1L]],
  date = NULL,
  band = NULL,
  size = 100000L
)
```

## Arguments

- x:

  Object of classes "raster_cube".

- ...:

  Further specifications for
  [summary](https://rdrr.io/r/base/summary.html).

- tile:

  Tile to be shown

- date:

  Date to be shown

- band:

  Band to be shown

- size:

  Number of cells to be sampled

## Value

A histogram of one band of data cube.

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
    hist(cube)
}
```
