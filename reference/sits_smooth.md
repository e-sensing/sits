# Smooth probability cubes with spatial predictors

Takes a set of classified raster layers with probabilities, whose
metadata is created by
[`sits_cube`](https://e-sensing.github.io/sits/reference/sits_cube.md),
and applies a Bayesian smoothing function.

## Usage

``` r
sits_smooth(cube, ...)

# S3 method for class 'probs_cube'
sits_smooth(
  cube,
  ...,
  window_size = 9L,
  neigh_fraction = 0.5,
  smoothness = 20,
  exclusion_mask = NULL,
  memsize = 4L,
  multicores = 2L,
  output_dir,
  version = "v1",
  progress = TRUE
)

# S3 method for class 'probs_vector_cube'
sits_smooth(cube, ...)

# S3 method for class 'raster_cube'
sits_smooth(cube, ...)

# S3 method for class 'derived_cube'
sits_smooth(cube, ...)

# Default S3 method
sits_smooth(cube, ...)
```

## Arguments

- cube:

  Probability data cube.

- ...:

  Other parameters for specific functions.

- window_size:

  Size of the neighborhood (integer, min = 3, max = 21)

- neigh_fraction:

  Fraction of neighbors with high probabilities to be used in Bayesian
  inference. (numeric, min = 0.1, max = 1.0)

- smoothness:

  Estimated variance of logit of class probabilities (Bayesian smoothing
  parameter) (integer vector or scalar, min = 1, max = 200).

- exclusion_mask:

  Areas to be excluded from the classification process. It can be
  defined as a sf object or a shapefile.

- memsize:

  Memory available for classification in GB (integer, min = 1, max =
  16384).

- multicores:

  Number of cores to be used for classification (integer, min = 1, max =
  2048).

- output_dir:

  Valid directory for output file. (character vector of length 1).

- version:

  Version of the output (character vector of length 1).

- progress:

  Check progress bar?

## Value

A data cube.

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

8.  `sits_smooth`: post-process a probability cube using a spatial
    smoother to remove outliers and increase spatial consistency.

9.  [`sits_label_classification`](https://e-sensing.github.io/sits/reference/sits_label_classification.md):
    produce a classified map by selecting the label with the highest
    probability from a smoothed cube.

Machine learning algorithms rely on training samples that are derived
from “pure” pixels, hand-picked by users to represent the desired output
classes. Given the presence of mixed pixels in images regardless of
resolution, and the considerable data variability within each class,
these classifiers often produce results with misclassified pixels.

Post-processing the results of
[`sits_classify`](https://e-sensing.github.io/sits/reference/sits_classify.md)
using `sits_smooth` reduces salt-and-pepper and border effects. By
minimizing noise, `sits_smooth` brings a significant gain in the overall
accuracy and interpretability of the final output.

## References

Gilberto Camara, Renato Assunção, Alexandre Carvalho, Rolf Simões,
Felipe Souza, Felipe Carlos, Anielli Souza, Ana Rorato, Ana Paula
Dal’Asta, “Bayesian inference for post-processing of remote sensing
image classification”. Remote Sensing, 16(23), 4572, 2024.
[doi:10.3390/rs16234572](https://doi.org/10.3390/rs16234572) .

## Author

Gilberto Camara, <gilberto.camara@inpe.br>

Rolf Simoes, <rolfsimoes@gmail.com>

## Examples

``` r
if (sits_run_examples()) {
    # create am xgboost model
    xgb_model <- sits_train(samples_modis_ndvi, sits_xgboost())
    # create a data cube from local files
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir
    )
    # classify a data cube
    probs_cube <- sits_classify(
        data = cube, ml_model = xgb_model, output_dir = tempdir()
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
