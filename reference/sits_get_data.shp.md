# Get time series using shapefiles

Retrieve a set of time series from a data cube and and put the result in
a `sits tibble`, which contains both the satellite image time series and
their metadata. The `samples` parameter must point to a file with
extension ".shp" which should be a valid shapefile in POINT or POLYGON
geometry. If `start_date` and `end_date` are not informed, the function
uses these data from the cube.

## Usage

``` r
# S3 method for class 'shp'
sits_get_data(
  cube,
  samples,
  ...,
  start_date = NULL,
  end_date = NULL,
  bands = NULL,
  impute_fn = impute_linear(),
  label = "NoClass",
  label_attr = NULL,
  n_sam_pol = 30L,
  pol_avg = FALSE,
  sampling_type = "random",
  multicores = 2L,
  progress = FALSE
)
```

## Arguments

- cube:

  Data cube from where data is to be retrieved. (tibble of class
  "raster_cube").

- samples:

  The name of a shapefile.

- ...:

  Specific parameters for specific cases.

- start_date:

  Start of the interval for the time series - optional (Date in
  "YYYY-MM-DD" format).

- end_date:

  End of the interval for the time series - optional (Date in
  "YYYY-MM-DD" format).

- bands:

  Bands to be retrieved - optional

- impute_fn:

  Imputation function to remove NA.

- label:

  Label to be assigned to all time series - optional

- label_attr:

  Attribute in the shapefile to be used as a polygon label.

- n_sam_pol:

  Number of samples per polygon to be read for POLYGON or MULTIPOLYGON
  shapefiles.

- pol_avg:

  Logical: summarize samples for each polygon?

- sampling_type:

  Spatial sampling type: random, hexagonal, regular, or Fibonacci.

- multicores:

  Number of threads to process the time series (integer, with min = 1
  and max = 2048).

- progress:

  Logical: show progress bar?

## Value

A tibble of class "sits" with set of time series and metadata
\<longitude, latitude, start_date, end_date, label, time_series\>.

## Note

For shapefiles, the following parameters are relevant:

- `label`: label to be assigned to the samples. Should only be used if
  all geometries have a single label.

- `label_attr`: defines which attribute should be used as a label,
  required for POINT and POLYGON geometries if `label` has not been set.

- `n_sam_pol`: indicates how many points are extracted from each
  polygon, required for POLYGON geometry (default = 15).

- `sampling_type`: defines how sampling is done, required for POLYGON
  geometry (default = "random").

- `pol_avg`: indicates if average of values for POLYGON geometry should
  be computed (default = "FALSE").

## Examples

``` r
if (sits_run_examples()) {
    # reading a shapefile from BDC (Brazil Data Cube)
    bdc_cube <- sits_cube(
        source = "BDC",
        collection = "CBERS-WFI-16D",
        bands = c("NDVI", "EVI"),
        tiles = c("007004", "007005"),
        start_date = "2018-09-01",
        end_date = "2018-10-28"
    )
    # define a shapefile to be read from the cube
    shp_file <- system.file("extdata/shapefiles/bdc-test/samples.shp",
        package = "sits"
    )
    # get samples from the BDC based on the shapefile
    time_series_bdc <- sits_get_data(
        cube = bdc_cube,
        samples = shp_file
    )
}
```
