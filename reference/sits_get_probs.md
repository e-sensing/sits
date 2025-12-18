# Get values from probability maps

Given a set of lat/long locations and a probability cube, retrieve the
prob values of each point. This function is useful to estimate
probability distributions and to assess the differences between
classifiers.

## Usage

``` r
sits_get_probs(cube, samples, window_size = NULL)

# S3 method for class 'csv'
sits_get_probs(cube, samples, window_size = NULL)

# S3 method for class 'shp'
sits_get_probs(cube, samples, window_size = NULL)

# S3 method for class 'sf'
sits_get_probs(cube, samples, window_size = NULL)

# S3 method for class 'sits'
sits_get_probs(cube, samples, window_size = NULL)

# S3 method for class 'data.frame'
sits_get_probs(cube, samples, window_size = NULL)

# Default S3 method
sits_get_probs(cube, samples, window_size = NULL)
```

## Arguments

- cube:

  Probability data cube.

- samples:

  Location of the samples to be retrieved. Either a tibble of class
  "sits", an "sf" object with POINT geometry, the location of a POINT
  shapefile, the location of csv file with columns "longitude" and
  "latitude", or a data.frame with columns "longitude" and "latitude"

- window_size:

  Size of window around pixel (optional)

## Value

A tibble of with columns \<longitude, latitude, values\> in case no
windows are requested and \<longitude, latitude, neighbors\> in case
windows are requested

## Note

There are four ways of specifying data to be retrieved using the
`samples` parameter:

- CSV: a CSV file with columns `longitude`, `latitude`.

- SHP: a shapefile in POINT geometry.

- sf object: An `link[sf]{sf}` object with POINT geometry.

- sits object: A valid tibble with `sits` timeseries.

- data.frame: A data.frame with `longitude` and `latitude`.

## Author

Gilberto Camara, <gilberto.camara@inpe.br>

## Examples

``` r
if (sits_run_examples()) {
    # create a random forest model
    rfor_model <- sits_train(samples_modis_ndvi, sits_rfor())
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
    # obtain the a set of points for sampling
    ground_truth <- system.file("extdata/samples/samples_sinop_crop.csv",
        package = "sits"
    )
    # get the classification values for a selected set of locations
    probs_samples <- sits_get_probs(probs_cube, ground_truth)
}
```
