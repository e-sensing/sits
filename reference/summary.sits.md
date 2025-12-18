# Summarize sits

This is a generic function. Parameters depend on the specific type of
input.

## Usage

``` r
# S3 method for class 'sits'
summary(object, ...)
```

## Arguments

- object:

  Object of class "sits".

- ...:

  Further specifications for
  [summary](https://rdrr.io/r/base/summary.html).

## Value

A summary of the sits tibble.

## Author

Gilberto Camara, <gilberto.camara@inpe.br>

Felipe Carvalho, <felipe.carvalho@inpe.br>

## Examples

``` r
if (sits_run_examples()) {
    summary(samples_modis_ndvi)
}
```
