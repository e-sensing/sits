# Sample a percentage of a time series

Takes a sits tibble with different labels and returns a new tibble. For
a given field as a group criterion, this new tibble contains a
percentage of the total number of samples per group. If frac \> 1 , all
sampling will be done with replacement.

## Usage

``` r
sits_sample(data, frac = 0.2, oversample = TRUE)
```

## Arguments

- data:

  Sits time series tibble

- frac:

  Percentage of samples to extract (range: 0.0 to 2.0, default = 0.2)

- oversample:

  Logical: oversample classes with small number of samples? (TRUE/FALSE)

## Value

A sits tibble with a fixed quantity of samples.

## Author

Rolf Simoes, <rolfsimoes@gmail.com>

## Examples

``` r
# Retrieve a set of time series with 2 classes
data(cerrado_2classes)
# Print the labels of the resulting tibble
summary(cerrado_2classes)
#> # A tibble: 2 × 3
#>   label   count  prop
#>   <chr>   <int> <dbl>
#> 1 Cerrado   400 0.536
#> 2 Pasture   346 0.464
# Sample by fraction
data_02 <- sits_sample(cerrado_2classes, frac = 0.2)
# Print the labels
summary(data_02)
#> # A tibble: 2 × 3
#>   label   count  prop
#>   <chr>   <int> <dbl>
#> 1 Cerrado    80 0.537
#> 2 Pasture    69 0.463
```
