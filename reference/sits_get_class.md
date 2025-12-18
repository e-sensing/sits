# Get values from classified maps

Given a set of lat/long locations and a classified cube, retrieve the
class of each point. This function is useful to obtain values from
classified cubes for accuracy estimates.

## Usage

``` r
sits_get_class(cube, samples)

# Default S3 method
sits_get_class(cube, samples)

# S3 method for class 'csv'
sits_get_class(cube, samples)

# S3 method for class 'shp'
sits_get_class(cube, samples)

# S3 method for class 'sf'
sits_get_class(cube, samples)

# S3 method for class 'sits'
sits_get_class(cube, samples)

# S3 method for class 'data.frame'
sits_get_class(cube, samples)
```

## Arguments

- cube:

  Classified data cube.

- samples:

  Location of the samples to be retrieved. Either a tibble of class
  "sits", an "sf" object, the name of a shapefile or csv file, or a
  data.frame with columns "longitude" and "latitude"

## Value

A tibble of with columns \<longitude, latitude, start_date, end_date,
label\>.

## Note

There are four ways of specifying data to be retrieved using the
`samples` parameter: (a) CSV file: a CSV file with columns `longitude`,
`latitude`; (b) SHP file: a shapefile in POINT geometry; (c) sits
object: A sits tibble; (d) sf object: An `link[sf]{sf}` object with
POINT or geometry; (e) data.frame: A data.frame with `longitude` and
`latitude`.

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
    # plot the probability cube
    plot(probs_cube)
    # smooth the probability cube using Bayesian statistics
    bayes_cube <- sits_smooth(probs_cube, output_dir = tempdir())
    # plot the smoothed cube
    plot(bayes_cube)
    # label the probability cube
    label_cube <- sits_label_classification(
        bayes_cube,
        output_dir = tempdir()
    )
    # obtain the a set of points for sampling
    ground_truth <- system.file("extdata/samples/samples_sinop_crop.csv",
        package = "sits"
    )
    # get the classification values for a selected set of locations
    labels_samples <- sits_get_class(label_cube, ground_truth)
}
```
