# Given a ROI, find MGRS tiles intersecting it.

Takes a a ROI and produces a list of MGRS tiles intersecting it

## Usage

``` r
sits_roi_to_mgrs(roi)
```

## Arguments

- roi:

  Valid ROI to use in other SITS functions

## Value

tiles Character vector with names of MGRS tiles

## Note

To define a `roi` use one of:

- A path to a shapefile with polygons;

- A `sfc` or `sf` object from `sf` package;

- A `SpatExtent` object from `terra` package;

- A named `vector` (`"lon_min"`, `"lat_min"`, `"lon_max"`, `"lat_max"`)
  in WGS84;

- A named `vector` (`"xmin"`, `"xmax"`, `"ymin"`, `"ymax"`) with XY
  coordinates.

Defining a region of interest using `SpatExtent` or XY values not in
WGS84 requires the `crs` parameter to be specified.

## Author

Felipe Carvalho, <felipe.carvalho@inpe.br>

Felipe Carlos, <efelipecarlos@gmail.com>

## Examples

``` r
if (sits_run_examples()) {
# Defining a ROI
roi <- c(
  lon_min = -64.037,
  lat_min = -9.644,
  lon_max = -63.886,
  lat_max = -9.389
)
# Finding tiles
tiles <- sits_roi_to_mgrs(roi)
}
```
