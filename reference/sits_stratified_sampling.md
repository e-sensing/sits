# Allocation of sample size to strata

Takes a class cube with different labels and a sampling design with a
number of samples per class and allocates a set of locations for each
class

## Usage

``` r
sits_stratified_sampling(
  cube,
  sampling_design,
  alloc = "alloc_prop",
  overhead = 1.2,
  multicores = 2L,
  memsize = 2L,
  shp_file = NULL,
  progress = TRUE
)
```

## Arguments

- cube:

  Classified cube

- sampling_design:

  Result of sits_sampling_design

- alloc:

  Allocation method chosen

- overhead:

  Additional percentage to account for border points

- multicores:

  Number of cores that will be used to sample the images in parallel.

- memsize:

  Memory available for sampling.

- shp_file:

  Name of shapefile to be saved (optional)

- progress:

  Show progress bar? Default is TRUE.

## Value

samples Point sf object with required samples

## Author

Gilberto Camara, <gilberto.camara@inpe.br>

Felipe Carlos, <efelipecarlos@gmail.com>

Felipe Carvalho, <felipe.carvalho@inpe.br>

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
    # label the probability cube
    label_cube <- sits_label_classification(
        probs_cube,
        output_dir = tempdir()
    )
    # estimated UA for classes
    expected_ua <- c(
        Cerrado = 0.95, Forest = 0.95,
        Pasture = 0.95, Soy_Corn = 0.95
    )
    # design sampling
    sampling_design <- sits_sampling_design(label_cube, expected_ua)
    # select samples
    samples <- sits_stratified_sampling(
        label_cube,
        sampling_design, "alloc_prop"
    )
}
```
