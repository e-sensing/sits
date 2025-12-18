# Find tiles of a given ROI and Grid System

Given an ROI and grid system, this function finds the intersected tiles
and returns them as an SF object.

## Usage

``` r
sits_roi_to_tiles(roi, crs = NULL, grid_system = "MGRS")
```

## Arguments

- roi:

  Region of interest (see notes below).

- crs:

  Coordinate Reference System (CRS) of the roi. (see details below).

- grid_system:

  Grid system to be used for the output images. (Default is "MGRS")

## Value

A `sf` object with the intersect tiles with three columns tile_id, epsg,
and the percentage of coverage area.

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

The `grid_system` parameter allows the user to reproject the files to a
grid system which is different from that used in the ARD image
collection of the could provider. Currently, the package supports the
use of MGRS grid system and those used by the Brazil Data Cube
("BDC_LG_V2" "BDC_MD_V2" "BDC_SM_V2").

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
tiles <- sits_roi_to_tiles(roi, grid_system = "MGRS")
}
```
