# Change the labels of a classified raster cube

Change the labels of a classified raster cube

## Usage

``` r
# S3 method for class 'class_cube'
sits_labels(data) <- value
```

## Arguments

- data:

  Classified raster data cube.

- value:

  A character vector used to convert labels. Labels will be renamed to
  the respective value positioned at the labels order returned by
  [`sits_labels`](https://e-sensing.github.io/sits/reference/sits_labels.md).
