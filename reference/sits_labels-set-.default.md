# Change the labels of other data structures

Change the labels of other data structures

## Usage

``` r
# Default S3 method
sits_labels(data) <- value
```

## Arguments

- data:

  Data cube or time series.

- value:

  A character vector used to convert labels. Labels will be renamed to
  the respective value positioned at the labels order returned by
  [`sits_labels`](https://e-sensing.github.io/sits/reference/sits_labels.md).
