# Get labels associated to a data set

Finds labels in a sits tibble or data cube

## Usage

``` r
sits_labels(data)

# S3 method for class 'sits'
sits_labels(data)

# S3 method for class 'derived_cube'
sits_labels(data)

# S3 method for class 'derived_vector_cube'
sits_labels(data)

# S3 method for class 'raster_cube'
sits_labels(data)

# S3 method for class 'patterns'
sits_labels(data)

# S3 method for class 'sits_model'
sits_labels(data)

# Default S3 method
sits_labels(data)
```

## Arguments

- data:

  Time series (tibble of class "sits"), patterns (tibble of class
  "patterns"), data cube (tibble of class "raster_cube"), or model
  (closure of class "sits_model").

## Value

The labels of the input data (character vector).

## Author

Rolf Simoes, <rolfsimoes@gmail.com>

## Examples

``` r
if (sits_run_examples()) {
    # get the labels for a time series set
    labels_ts <- sits_labels(samples_modis_ndvi)
    # get labels for a set of patterns
    labels_pat <- sits_labels(sits_patterns(samples_modis_ndvi))
    # create a random forest model
    rfor_model <- sits_train(samples_modis_ndvi, sits_rfor())
    # get lables for the model
    labels_mod <- sits_labels(rfor_model)
    # create a data cube from local files
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir
    )
    # classify a data cube
    probs_cube <- sits_classify(
        data = cube, ml_model = rfor_model, output_dir = tempdir()
    )
    # get the labels for a probs cube
    labels_probs <- sits_labels(probs_cube)
}
```
