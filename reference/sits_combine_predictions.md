# Estimate ensemble prediction based on list of probs cubes

Calculate an ensemble predictor based a list of probability cubes. The
function combines the output of two or more models to derive a weighted
average. The supported types of ensemble predictors are 'average' and
'uncertainty'. In the latter case, the uncertainty cubes need to be
provided using param `uncert_cubes`.

## Usage

``` r
sits_combine_predictions(cubes, type = "average", ...)

# S3 method for class 'average'
sits_combine_predictions(
  cubes,
  type = "average",
  ...,
  weights = NULL,
  memsize = 8L,
  multicores = 2L,
  output_dir,
  version = "v1",
  progress = FALSE
)

# S3 method for class 'uncertainty'
sits_combine_predictions(
  cubes,
  type = "uncertainty",
  ...,
  uncert_cubes,
  memsize = 8L,
  multicores = 2L,
  output_dir,
  version = "v1",
  progress = FALSE
)

# Default S3 method
sits_combine_predictions(cubes, type, ...)
```

## Arguments

- cubes:

  List of probability data cubes (class "probs_cube")

- type:

  Method to measure uncertainty. One of "average" or "uncertainty"

- ...:

  Parameters for specific functions.

- weights:

  Weights for averaging (numeric vector).

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

  Set progress bar?

- uncert_cubes:

  Uncertainty cubes to be used as local weights when type =
  "uncertainty" is selected (list of tibbles with class
  "uncertainty_cube")

## Value

A combined probability cube (tibble of class "probs_cube").

## Note

The distribution of class probabilities produced by machine learning
models such as random forest is quite different from that produced by
deep learning models such as temporal CNN. Combining the result of two
different models is recommended to remove possible bias induced by a
single model.

By default, the function takes the average of the class probabilities of
two or more model results. If desired, users can use the uncertainty
estimates for each results to compute the weights for each pixel. In
this case, the uncertainties produced by the models for each pixel are
used to compute the weights for producing the combined result.

## Author

Gilberto Camara, <gilberto.camara@inpe.br>

Rolf Simoes, <rolfsimoes@gmail.com>

## Examples

``` r
if (sits_run_examples()) {
    # create a data cube from local files
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir
    )
    # create a random forest model
    rfor_model <- sits_train(samples_modis_ndvi, sits_rfor())
    # classify a data cube using rfor model
    probs_rfor_cube <- sits_classify(
        data = cube, ml_model = rfor_model, output_dir = tempdir(),
        version = "rfor"
    )
    # create an SVM model
    svm_model <- sits_train(samples_modis_ndvi, sits_svm())
    # classify a data cube using SVM model
    probs_svm_cube <- sits_classify(
        data = cube, ml_model = svm_model, output_dir = tempdir(),
        version = "svm"
    )
    # create a list of predictions to be combined
    pred_cubes <- list(probs_rfor_cube, probs_svm_cube)
    # combine predictions
    comb_probs_cube <- sits_combine_predictions(
        pred_cubes,
        output_dir = tempdir()
    )
    # plot the resulting combined prediction cube
    plot(comb_probs_cube)
}
```
