# Remove NA using weighted moving average

Remove NA using weighted moving average

## Usage

``` r
impute_mean_window(data = NULL, k = 2, weighting = "simple")
```

## Arguments

- data:

  A time series vector or matrix

- k:

  A integer width of the moving average window. Expands to both sides of
  the center element e.g. k = 2 means 4 observations (2 left, 2 right)
  are taken into account. If all observations in the current window are
  NA, the window size is automatically increased until there are at
  least 2 non-NA values present

- weighting:

  A string with the weighting strategy to be used. More details below
  (default is "simple").

## Value

A set of filtered time series using the imputation function.

## Note

The `weighting` parameter defines the weighting strategy used in the
moving window. The strategies available are:

- `simple` - Simple Moving Average (SMA) (default option)

- `linear` - Linear Weighted Moving Average (LWMA)

- `exponential` - Exponential Weighted Moving Average (EWMA)

## References

The implementation of this function was adapted from the `imputeTS` R
Package. The code is open-source, under the GPL license, and is
available on GitHub <https://github.com/SteffenMoritz/imputeTS>.

## Author

Felipe Carlos, <efelipecarlos@gmail.com>
