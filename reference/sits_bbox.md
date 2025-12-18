# Get the bounding box of the data

Obtain a vector of limits (either on lat/long for time series or in
projection coordinates in the case of cubes)

## Usage

``` r
sits_bbox(data, ..., crs = "EPSG:4326", as_crs = NULL)

# S3 method for class 'sits'
sits_bbox(data, ..., crs = "EPSG:4326", as_crs = NULL)

# S3 method for class 'raster_cube'
sits_bbox(data, ..., as_crs = NULL)

# S3 method for class 'tbl_df'
sits_bbox(data, ..., crs = "EPSG:4326", as_crs = NULL)

# Default S3 method
sits_bbox(data, ..., crs = "EPSG:4326", as_crs = NULL)
```

## Arguments

- data:

  samples (class "sits") or `cube`.

- ...:

  parameters for specific types

- crs:

  CRS of the time series.

- as_crs:

  CRS to project the resulting `bbox`.

## Value

A `bbox`.

## Note

Time series in `sits` are associated with lat/long values in WGS84,
while each data cubes is associated to a cartographic projection. To
obtain the bounding box of a data cube in a different projection than
the original, use the `as_crs` parameter.

## Author

Gilberto Camara, <gilberto.camara@inpe.br>

Rolf Simoes, <rolfsimoes@gmail.com>

## Examples

``` r
if (sits_run_examples()) {
    # get the bbox of a set of samples
    sits_bbox(samples_modis_ndvi)
    # get the bbox of a cube in WGS84
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir
    )
    sits_bbox(cube, as_crs = "EPSG:4326")
}
```
