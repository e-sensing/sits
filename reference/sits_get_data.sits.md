# Get time series using sits objects

Retrieve a set of time series from a data cube and and put the result in
a `sits tibble`. The `samples` parameter should be a valid `sits tibble`
which which contains columns `longitude`, `latitude`, `start_date`,
`end_date` and `label` for each sample.

## Usage

``` r
# S3 method for class 'sits'
sits_get_data(
  cube,
  samples,
  ...,
  bands = NULL,
  crs = "EPSG:4326",
  impute_fn = impute_linear(),
  multicores = 2L,
  progress = FALSE
)
```

## Arguments

- cube:

  Data cube from where data is to be retrieved. (tibble of class
  "raster_cube").

- samples:

  Location of the samples to be retrieved. Either a tibble of class
  "sits", an "sf" object, the name of a shapefile or csv file, or a
  data.frame with columns "longitude" and "latitude".

- ...:

  Specific parameters for specific cases.

- bands:

  Bands to be retrieved - optional.

- crs:

  A character with the samples crs. Default is "EPSG:4326".

- impute_fn:

  Imputation function to remove NA.

- multicores:

  Number of threads to process the time series (integer, with min = 1
  and max = 2048).

- progress:

  Logical: show progress bar?

## Value

A tibble of class "sits" with set of time series \<longitude, latitude,
start_date, end_date, label\>.
