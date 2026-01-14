# Change the labels of a probs raster cube

Change the labels of a probs raster cube

## Usage

``` r
# S3 method for class 'probs_cube'
sits_labels(data) <- value
```

## Arguments

- data:

  Raster cube with probability values.

- value:

  A character vector used to convert labels. Labels will be renamed to
  the respective value positioned at the labels order returned by
  [`sits_labels`](https://e-sensing.github.io/sits/reference/sits_labels.md).
