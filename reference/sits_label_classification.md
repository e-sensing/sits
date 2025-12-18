# Build a labelled image from a probability cube

Takes a set of classified raster layers with probabilities, and labels
them based on the maximum probability for each pixel. This function is
the final step of main the land classification workflow.

## Usage

``` r
sits_label_classification(cube, ...)

# S3 method for class 'probs_cube'
sits_label_classification(
  cube,
  ...,
  memsize = 4L,
  multicores = 2L,
  output_dir,
  version = "v1",
  progress = TRUE
)

# S3 method for class 'probs_vector_cube'
sits_label_classification(
  cube,
  ...,
  output_dir,
  version = "v1",
  progress = TRUE
)

# S3 method for class 'raster_cube'
sits_label_classification(cube, ...)

# S3 method for class 'derived_cube'
sits_label_classification(cube, ...)

# Default S3 method
sits_label_classification(cube, ...)
```

## Arguments

- cube:

  Classified image data cube.

- ...:

  Other parameters for specific functions.

- memsize:

  maximum overall memory (in GB) to label the classification.

- multicores:

  Number of workers to label the classification in parallel.

- output_dir:

  Output directory for classified files.

- version:

  Version of resulting image (in the case of multiple runs).

- progress:

  Show progress bar?

## Value

A data cube with an image with the classified map.

## Note

The main `sits` classification workflow has the following steps:

1.  [`sits_cube`](https://e-sensing.github.io/sits/reference/sits_cube.md):
    selects a ARD image collection from a cloud provider.

2.  [`sits_cube_copy`](https://e-sensing.github.io/sits/reference/sits_cube_copy.md):
    copies an ARD image collection from a cloud provider to a local
    directory for faster processing.

3.  [`sits_regularize`](https://e-sensing.github.io/sits/reference/sits_regularize.md):
    create a regular data cube from an ARD image collection.

4.  [`sits_apply`](https://e-sensing.github.io/sits/reference/sits_apply.md):
    create new indices by combining bands of a regular data cube
    (optional).

5.  [`sits_get_data`](https://e-sensing.github.io/sits/reference/sits_get_data.md):
    extract time series from a regular data cube based on user-provided
    labelled samples.

6.  [`sits_train`](https://e-sensing.github.io/sits/reference/sits_train.md):
    train a machine learning model based on image time series.

7.  [`sits_classify`](https://e-sensing.github.io/sits/reference/sits_classify.md):
    classify a data cube using a machine learning model and obtain a
    probability cube.

8.  [`sits_smooth`](https://e-sensing.github.io/sits/reference/sits_smooth.md):
    post-process a probability cube using a spatial smoother to remove
    outliers and increase spatial consistency.

9.  `sits_label_classification`: produce a classified map by selecting
    the label with the highest probability from a smoothed cube.

Please refer to the sits documentation available in
<https://e-sensing.github.io/sitsbook/> for detailed examples.

## Author

Rolf Simoes, <rolfsimoes@gmail.com>

Felipe Souza, <felipe.souza@inpe.br>

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
    # plot the labelled cube
    plot(label_cube)
}
```
