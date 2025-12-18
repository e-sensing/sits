# Histogram

This is a generic function. Parameters depend on the specific type of
input.

## Usage

``` r
# S3 method for class 'sits'
hist(x, ...)
```

## Arguments

- x:

  Object of classes "sits".

- ...:

  Further specifications for
  [hist](https://rdrr.io/r/graphics/hist.html).

## Value

A summary of the sits tibble.

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
