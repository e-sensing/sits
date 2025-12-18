# Calculate the variance of a probability cube

Takes a probability cube and estimate the local variance of the logit of
the probability, to support the choice of parameters for Bayesian
smoothing.

## Usage

``` r
sits_variance(cube, ...)

# S3 method for class 'probs_cube'
sits_variance(
  cube,
  ...,
  window_size = 9L,
  neigh_fraction = 0.5,
  memsize = 4L,
  multicores = 2L,
  output_dir,
  version = "v1",
  progress = TRUE
)

# S3 method for class 'raster_cube'
sits_variance(cube, ...)

# S3 method for class 'derived_cube'
sits_variance(cube, ...)

# Default S3 method
sits_variance(cube, ...)
```

## Arguments

- cube:

  Probability data cube (class "probs_cube")

- ...:

  Parameters for specific functions

- window_size:

  Size of the neighborhood (odd integer)

- neigh_fraction:

  Fraction of neighbors with highest probability for Bayesian inference
  (numeric from 0.0 to 1.0)

- memsize:

  Maximum overall memory (in GB) to run the smoothing (integer, min = 1,
  max = 16384)

- multicores:

  Number of cores to run the smoothing function (integer, min = 1, max =
  2048)

- output_dir:

  Output directory for image files (character vector of length 1)

- version:

  Version of resulting image (character vector of length 1)

- progress:

  Check progress bar?

## Value

A variance data cube.

## Author

Gilberto Camara, <gilberto.camara@inpe.br>

Rolf Simoes, <rolfsimoes@gmail.com>

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
    var_cube <- sits_variance(probs_cube, output_dir = tempdir())
    # plot the variance cube
    plot(var_cube)
}
```
