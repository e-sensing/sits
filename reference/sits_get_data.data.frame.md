# Get time series using sits objects

Retrieve a set of time series from a data cube and and put the result in
a `sits tibble`. The `samples` parameter should be a `data.frame` which
which contains mandatory columns `longitude` and `latitude`, and
optional columns `start_date`, `end_date` and `label` for each sample.

## Usage

``` r
# S3 method for class 'data.frame'
sits_get_data(
  cube,
  samples,
  ...,
  start_date = NULL,
  end_date = NULL,
  bands = NULL,
  impute_fn = impute_linear(),
  label = "NoClass",
  crs = "EPSG:4326",
  multicores = 2,
  progress = FALSE
)
```

## Arguments

- cube:

  Data cube from where data is to be retrieved. (tibble of class
  "raster_cube").

- samples:

  A data.frame with mandatory columns `longitude`, and `latitude`, and
  optional columns `start_date`, `end_date`, `label`.

- ...:

  Specific parameters for specific cases.

- start_date:

  Start of the interval for the time series - optional (Date in
  "YYYY-MM-DD" format).

- end_date:

  End of the interval for the time series - optional (Date in
  "YYYY-MM-DD" format).

- bands:

  Bands to be retrieved - optional.

- impute_fn:

  Imputation function to remove NA.

- label:

  Label to be assigned to all time series if column `label` is not
  provided in the data.frame.

- crs:

  A character with the samples crs. Default is "EPSG:4326".

- multicores:

  Number of threads to process the time series (integer, with min = 1
  and max = 2048).

- progress:

  Logical: show progress bar?

## Value

A tibble of class "sits" with set of time series \<longitude, latitude,
start_date, end_date, label\>.

## Examples

``` r
if (sits_run_examples()) {
    # create a cube from local files
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    raster_cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir
    )
    # read a lat/long from a local cube
    samples <- data.frame(longitude = -55.66738, latitude = -11.76990)
    point_ndvi <- sits_get_data(raster_cube, samples)
}
```
