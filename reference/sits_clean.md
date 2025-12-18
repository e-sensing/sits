# Cleans a classified map using a local window

Applies a modal function to clean up possible noisy pixels keeping the
most frequently values within the neighborhood. In a tie, the first
value of the vector is considered. Modal functions applied to classified
cubes are useful to remove salt-and-pepper noise in the result.

## Usage

``` r
sits_clean(cube, ...)

# S3 method for class 'class_cube'
sits_clean(
  cube,
  ...,
  window_size = 5L,
  memsize = 4L,
  multicores = 2L,
  output_dir,
  version = "v1-clean",
  progress = TRUE
)

# S3 method for class 'raster_cube'
sits_clean(cube, ...)

# S3 method for class 'derived_cube'
sits_clean(cube, ...)

# Default S3 method
sits_clean(cube, ...)
```

## Arguments

- cube:

  Classified data cube (tibble of class "class_cube").

- ...:

  Specific parameters for specialised functions

- window_size:

  An odd integer representing the size of the sliding window of the
  modal function (min = 1, max = 15).

- memsize:

  Memory available for classification in GB (integer, min = 1, max =
  16384).

- multicores:

  Number of cores to be used for classification (integer, min = 1, max =
  2048).

- output_dir:

  Valid directory for output file. (character vector of length 1).

- version:

  Version of the output file (character vector of length 1)

- progress:

  Logical: Show progress bar?

## Value

A tibble with an classified map (class = "class_cube").

## Note

The `sits_clean` function is useful to further remove classification
noise which has not been detected by
[`sits_smooth`](https://e-sensing.github.io/sits/reference/sits_smooth.md).
It improves the spatial consistency of the classified maps.

## Author

Felipe Carvalho, <felipe.carvalho@inpe.br>

## Examples

``` r
if (sits_run_examples()) {
    rf_model <- sits_train(samples_modis_ndvi, ml_method = sits_rfor)
    # create a data cube from local files
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir
    )
    # classify a data cube
    probs_cube <- sits_classify(
        data = cube,
        ml_model = rf_model,
        output_dir = tempdir()
    )
    # label the probability cube
    label_cube <- sits_label_classification(
        probs_cube,
        output_dir = tempdir()
    )
    # apply a mode function in the labelled cube
    clean_cube <- sits_clean(
        cube = label_cube,
        window_size = 5,
        output_dir = tempdir(),
        multicores = 1
    )
}
```
