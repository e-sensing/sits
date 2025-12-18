# Return a sits_tibble or raster_cube as an sf object.

Converts a sits_tibble or raster_cube as an sf object.

## Usage

``` r
sits_as_sf(data, ...)

# S3 method for class 'sits'
sits_as_sf(data, ..., crs = "EPSG:4326", as_crs = NULL)

# S3 method for class 'raster_cube'
sits_as_sf(data, ..., as_crs = NULL)

# S3 method for class 'vector_cube'
sits_as_sf(data, ..., as_crs = NULL)

# Default S3 method
sits_as_sf(data, ...)
```

## Arguments

- data:

  A sits tibble or sits cube.

- ...:

  Additional parameters.

- crs:

  Input coordinate reference system.

- as_crs:

  Output coordinate reference system.

## Value

An sf object of point or polygon geometry.

## Author

Felipe Carvalho, <felipe.carvalho@inpe.br>

Alber Sanchez, <alber.ipia@inpe.br>

## Examples

``` r
if (sits_run_examples()) {
    # convert sits tibble to an sf object (point)
    sf_object <- sits_as_sf(cerrado_2classes)

    # convert sits cube to an sf object (polygon)
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir
    )
    sf_object <- sits_as_sf(cube)
}
```
