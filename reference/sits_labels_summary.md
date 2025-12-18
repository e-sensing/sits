# Inform label distribution of a set of time series

Describes labels in a sits tibble

## Usage

``` r
sits_labels_summary(data)

# S3 method for class 'sits'
sits_labels_summary(data)
```

## Arguments

- data:

  Data.frame - Valid sits tibble

## Value

A tibble with the frequency of each label.

## Author

Rolf Simoes, <rolfsimoes@gmail.com>

## Examples

``` r
# read a tibble with 400 samples of Cerrado and 346 samples of Pasture
data(cerrado_2classes)
# print the labels
sits_labels_summary(cerrado_2classes)
#> Warning: this function is deprecated; please use summary()
#> # A tibble: 2 × 3
#>   label   count  prop
#>   <chr>   <int> <dbl>
#> 1 Cerrado   400 0.536
#> 2 Pasture   346 0.464
```
