# Change the labels of a set of time series

Given a sits tibble with a set of labels, renames the labels to the
specified in value.

## Usage

``` r
sits_labels(data) <- value
```

## Arguments

- data:

  Data cube or time series.

- value:

  A character vector used to convert labels. Labels will be renamed to
  the respective value positioned at the labels order returned by
  [`sits_labels`](https://e-sensing.github.io/sits/reference/sits_labels.md).

## Value

A sits tibble or data cube with modified labels.

## Author

Rolf Simoes, <rolfsimoes@gmail.com>

## Examples

``` r
# show original samples ("Cerrado" and "Pasture")
sits_labels(cerrado_2classes)
#> [1] "Cerrado" "Pasture"
# rename label samples to "Savanna" and "Grasslands"
sits_labels(cerrado_2classes) <- c("Savanna", "Grasslands")
# see the change
sits_labels(cerrado_2classes)
#> [1] "Grasslands" "Savanna"   
```
