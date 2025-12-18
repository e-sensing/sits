# Plot patterns that describe classes

Plots the patterns (one plot per band/class combination) Useful to
understand the trends of time series.

## Usage

``` r
# S3 method for class 'patterns'
plot(x, y, ..., bands = NULL, year_grid = FALSE)
```

## Arguments

- x:

  Object of class "patterns".

- y:

  Ignored.

- ...:

  Further specifications for
  [plot](https://e-sensing.github.io/sits/reference/plot.md).

- bands:

  Bands to be viewed (optional).

- year_grid:

  Plot a grid of panels using labels as columns and years as rows.
  Default is FALSE.

## Value

A plot object produced by ggplot2 with one average pattern per label.

## Note

This code is reused from the dtwSat package by Victor Maus.

## Author

Gilberto Camara, <gilberto.camara@inpe.br>

Victor Maus, <vwmaus1@gmail.com>

## Examples

``` r
if (sits_run_examples()) {
    # plot patterns
    plot(sits_patterns(cerrado_2classes))
}
```
