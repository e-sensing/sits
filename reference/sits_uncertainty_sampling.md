# Suggest samples for enhancing classification accuracy

Suggest samples for regions of high uncertainty as predicted by the
model. The function selects data points that have confused an algorithm.
These points don't have labels and need be manually labelled by experts
and then used to increase the classification's training set.

This function is best used in the following context: 1. Select an
initial set of samples. 2. Train a machine learning model. 3. Build a
data cube and classify it using the model. 4. Run a Bayesian smoothing
in the resulting probability cube. 5. Create an uncertainty cube. 6.
Perform uncertainty sampling.

The Bayesian smoothing procedure will reduce the classification outliers
and thus increase the likelihood that the resulting pixels with high
uncertainty have meaningful information.

## Usage

``` r
sits_uncertainty_sampling(
  uncert_cube,
  n = 100L,
  min_uncert = 0.4,
  sampling_window = 10L,
  multicores = 2L,
  memsize = 4L
)
```

## Arguments

- uncert_cube:

  An uncertainty cube. See
  [`sits_uncertainty`](https://e-sensing.github.io/sits/reference/sits_uncertainty.md).

- n:

  Number of suggested points to be sampled per tile.

- min_uncert:

  Minimum uncertainty value to select a sample.

- sampling_window:

  Window size for collecting points (in pixels). The minimum window size
  is 10.

- multicores:

  Number of workers for parallel processing (integer, min = 1, max =
  2048).

- memsize:

  Maximum overall memory (in GB) to run the function.

## Value

A tibble with longitude and latitude in WGS84 with locations which have
high uncertainty and meet the minimum distance criteria.

## References

Robert Monarch, "Human-in-the-Loop Machine Learning: Active learning and
annotation for human-centered AI". Manning Publications, 2021.

## Author

Alber Sanchez, <alber.ipia@inpe.br>

Rolf Simoes, <rolfsimoes@gmail.com>

Felipe Carvalho, <felipe.carvalho@inpe.br>

Gilberto Camara, <gilberto.camara@inpe.br>

## Examples

``` r
if (sits_run_examples()) {
    # create a data cube
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir
    )
    # build a random forest model
    rfor_model <- sits_train(samples_modis_ndvi, ml_method = sits_rfor())
    # classify the cube
    probs_cube <- sits_classify(
        data = cube, ml_model = rfor_model, output_dir = tempdir()
    )
    # create an uncertainty cube
    uncert_cube <- sits_uncertainty(probs_cube,
        type = "entropy",
        output_dir = tempdir()
    )
    # obtain a new set of samples for active learning
    # the samples are located in uncertain places
    new_samples <- sits_uncertainty_sampling(
        uncert_cube,
        n = 10, min_uncert = 0.4
    )
}
```
