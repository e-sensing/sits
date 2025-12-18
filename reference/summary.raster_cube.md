# Summarize data cubes

This is a generic function. Parameters depend on the specific type of
input.

## Usage

``` r
# S3 method for class 'raster_cube'
summary(object, ..., tile = NULL, date = NULL)
```

## Arguments

- object:

  Object of classes "raster_cube".

- ...:

  Further specifications for
  [summary](https://rdrr.io/r/base/summary.html).

- tile:

  Tile to be summarized

- date:

  Date to be summarized

## Value

A summary of the data cube.

## Author

Gilberto Camara, <gilberto.camara@inpe.br>

Felipe Souza, <felipe.souza@inpe.br>

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
    summary(cube)
}
```
