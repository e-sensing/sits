# Convert a data cube into a stars object

Uses the information about files, bands and dates in a data cube to
produce an object of class `stars`. User has to select a tile from the
data cube. By default, all bands and dates are included in the `stars`
object. Users can select bands and dates.

## Usage

``` r
sits_as_stars(
  cube,
  tile = cube[1L, ]$tile,
  bands = NULL,
  dates = NULL,
  proxy = FALSE
)
```

## Arguments

- cube:

  A sits cube.

- tile:

  Tile of the data cube.

- bands:

  Bands of the data cube to be part of `stars` object.

- dates:

  Dates of the data cube to be part of `stars` object.

- proxy:

  Produce a stars proxy object.

## Value

An space-time stars object.

## Note

By default, the `stars` object will be loaded in memory. This can result
in heavy memory usage. To produce a `stars.proxy` object, uses have to
select a single date, since `stars` does not allow proxy objects to be
created with two dimensions.

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
    stars_object <- sits_as_stars(cube)
}
```
