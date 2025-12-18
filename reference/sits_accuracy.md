# Assess classification accuracy

This function calculates the accuracy of the classification result. The
input is either a set of classified time series or a classified data
cube. Classified time series are produced by
[`sits_classify`](https://e-sensing.github.io/sits/reference/sits_classify.md).
Classified images are generated using
[`sits_classify`](https://e-sensing.github.io/sits/reference/sits_classify.md)
followed by
[`sits_label_classification`](https://e-sensing.github.io/sits/reference/sits_label_classification.md).

For a set of time series, `sits_accuracy` creates a confusion matrix and
calculates the resulting statistics using package `caret`. For a
classified image, the function uses an area-weighted technique proposed
by Olofsson et al. according to references \[1-3\] to produce reliable
accuracy estimates at 95% confidence level. In both cases, it provides
an accuracy assessment of the classified, including Overall Accuracy,
Kappa, User's Accuracy, Producer's Accuracy and error matrix (confusion
matrix).

## Usage

``` r
sits_accuracy(data, ...)

# S3 method for class 'sits'
sits_accuracy(data, ...)

# S3 method for class 'class_vector_cube'
sits_accuracy(data, ..., prediction_attr, reference_attr)

# S3 method for class 'class_cube'
sits_accuracy(data, ..., validation, method = "olofsson")

# S3 method for class 'raster_cube'
sits_accuracy(data, ...)

# S3 method for class 'derived_cube'
sits_accuracy(data, ...)

# S3 method for class 'tbl_df'
sits_accuracy(data, ...)

# Default S3 method
sits_accuracy(data, ...)
```

## Arguments

- data:

  Either a data cube with classified images or a set of time series

- ...:

  Specific parameters

- prediction_attr:

  Name of the column of the segments object that contains the predicted
  values (only for vector class cubes)

- reference_attr:

  Name of the column of the segments object that contains the reference
  values (only for vector class cubes)

- validation:

  Samples for validation (see below) Only required when data is a raster
  class cube.

- method:

  A character with 'olofsson' or 'pixel' to compute accuracy (only for
  raster class cubes)

## Value

A list of lists: The error_matrix, the class_areas, the unbiased
estimated areas, the standard error areas, confidence interval 95 and
the accuracy (user, producer, and overall), or NULL if the data is
empty. The result is assigned to class "sits_accuracy" and can be
visualized directly on the screen.

## Note

The \`validation\` data needs to contain the following columns:
"latitude", "longitude", "start_date", "end_date", and "label". It can
be either a path to a CSV file, a sits tibble, a data frame, or an sf
object.

When \`validation\` is an sf object, the columns "latitude" and
"longitude" are not required as the locations are extracted from the
geometry column. The \`centroid\` is calculated before extracting the
location values for any geometry type.

## References

\[1\] Olofsson, P., Foody, G.M., Stehman, S.V., Woodcock, C.E. (2013).
Making better use of accuracy data in land change studies: Estimating
accuracy and area and quantifying uncertainty using stratified
estimation. Remote Sensing of Environment, 129, pp.122-131.

\[2\] Olofsson, P., Foody G.M., Herold M., Stehman, S.V., Woodcock,
C.E., Wulder, M.A. (2014) Good practices for estimating area and
assessing accuracy of land change. Remote Sensing of Environment, 148,
pp. 42-57.

\[3\] FAO, Map Accuracy Assessment and Area Estimation: A Practical
Guide. National forest monitoring assessment working paper No.46/E,
2016.

## Author

Rolf Simoes, <rolfsimoes@gmail.com>

Alber Sanchez, <alber.ipia@inpe.br>

## Examples

``` r
if (sits_run_examples()) {
    # show accuracy for a set of samples
    train_data <- sits_sample(samples_modis_ndvi, frac = 0.5)
    test_data <- sits_sample(samples_modis_ndvi, frac = 0.5)
    rfor_model <- sits_train(train_data, sits_rfor())
    points_class <- sits_classify(
        data = test_data, ml_model = rfor_model
    )
    acc <- sits_accuracy(points_class)

    # show accuracy for a data cube classification
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
    # label the probability cube
    label_cube <- sits_label_classification(
        probs_cube,
        output_dir = tempdir()
    )
    # obtain the ground truth for accuracy assessment
    ground_truth <- system.file("extdata/samples/samples_sinop_crop.csv",
        package = "sits"
    )
    # make accuracy assessment
    as <- sits_accuracy(label_cube, validation = ground_truth)
}
```
