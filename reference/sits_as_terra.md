# Convert a data cube into a Spatial Raster object from terra

Uses the information about files, bands and dates in a data cube to
produce an object of class `terra`. User has to select a tile and a date
from the data cube. By default, all bands are included in the `terra`
object. Users can select bands.

## Usage

``` r
sits_as_terra(cube, tile = cube[1L, ]$tile, ...)

# S3 method for class 'raster_cube'
sits_as_terra(cube, tile = cube[1L, ]$tile, ..., bands = NULL, date = NULL)

# S3 method for class 'probs_cube'
sits_as_terra(cube, tile = cube[1L, ]$tile, ...)

# S3 method for class 'class_cube'
sits_as_terra(cube, tile = cube[1L, ]$tile, ...)

# S3 method for class 'variance_cube'
sits_as_terra(cube, tile = cube[1L, ]$tile, ...)

# S3 method for class 'uncertainty_cube'
sits_as_terra(cube, tile = cube[1L, ]$tile, ...)
```

## Arguments

- cube:

  A sits cube.

- tile:

  Tile of the data cube.

- ...:

  Other parameters for specific types of data cubes.

- bands:

  Bands of the data cube to be part of `terra` object.

- date:

  Date of the data cube to be part of `terra` object.

## Value

An Spatial Raster object from `terra`.

## Author

Gilberto Camara, <gilberto.camara.inpe@gmail.com>

## Examples

``` r
if (sits_run_examples()) {
    # convert sits cube to an sf object (polygon)
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir
    )
    spat_raster <- sits_as_terra(cube)
}
```
