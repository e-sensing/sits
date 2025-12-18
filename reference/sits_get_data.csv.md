# Get time series using CSV files

Retrieve a set of time series from a data cube and and put the result in
a "sits tibble", which contains both the satellite image time series and
their metadata. The `samples` parameter must point to a file with
extension ".csv", with mandatory columns `longitude`, `latitude`,
`label`, `start_date` and `end_date`.

## Usage

``` r
# S3 method for class 'csv'
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

  Location of a csv file.

- ...:

  Specific parameters for each kind of input.

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

A tibble of class "sits" with set of time series and metadata with
\<longitude, latitude, start_date, end_date, label, time_series\>.

## Examples

``` r
if (sits_run_examples()) {
    # reading a lat/long from a local cube
    # create a cube from local files
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    raster_cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir
    )
    # reading samples from a cube based on a  CSV file
    csv_file <- system.file("extdata/samples/samples_sinop_crop.csv",
        package = "sits"
    )
    points <- sits_get_data(cube = raster_cube, samples = csv_file)
}
```
