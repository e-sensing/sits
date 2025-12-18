# Build a regular data cube from an irregular one

Produces regular data cubes for analysis-ready data (ARD) image
collections. Analysis-ready data (ARD) collections available in AWS,
MPC, USGS and DEAfrica are not regular in space and time. Bands may have
different resolutions, images may not cover the entire time, and time
intervals are not regular. For this reason, subsets of these collection
need to be converted to regular data cubes before further processing and
data analysis. This function requires users to include the cloud band in
their ARD-based data cubes. This function uses the `gdalcubes` package.

## Usage

``` r
sits_regularize(cube, ...)

# S3 method for class 'raster_cube'
sits_regularize(
  cube,
  ...,
  period,
  res,
  output_dir,
  timeline = NULL,
  roi = NULL,
  crs = NULL,
  tiles = NULL,
  grid_system = NULL,
  multicores = 2L,
  progress = TRUE
)

# S3 method for class 'sar_cube'
sits_regularize(
  cube,
  ...,
  period,
  res,
  output_dir,
  timeline = NULL,
  grid_system = "MGRS",
  roi = NULL,
  crs = NULL,
  tiles = NULL,
  multicores = 2L,
  progress = TRUE
)

# S3 method for class 'combined_cube'
sits_regularize(
  cube,
  ...,
  period,
  res,
  output_dir,
  grid_system = NULL,
  roi = NULL,
  crs = NULL,
  tiles = NULL,
  multicores = 2L,
  progress = TRUE
)

# S3 method for class 'rainfall_cube'
sits_regularize(
  cube,
  ...,
  period,
  res,
  output_dir,
  timeline = NULL,
  grid_system = "MGRS",
  roi = NULL,
  crs = NULL,
  tiles = NULL,
  multicores = 2L,
  progress = TRUE
)

# S3 method for class 'dem_cube'
sits_regularize(
  cube,
  ...,
  res,
  output_dir,
  grid_system = "MGRS",
  roi = NULL,
  crs = NULL,
  tiles = NULL,
  multicores = 2L,
  progress = TRUE
)

# S3 method for class 'ogh_cube'
sits_regularize(
  cube,
  ...,
  period,
  res,
  output_dir,
  timeline = NULL,
  grid_system = "MGRS",
  roi = NULL,
  crs = NULL,
  tiles = NULL,
  multicores = 2L,
  progress = TRUE
)

# S3 method for class 'derived_cube'
sits_regularize(cube, ...)

# Default S3 method
sits_regularize(cube, ...)
```

## Arguments

- cube:

  `raster_cube` object whose observation period and/or spatial
  resolution is not constant.

- ...:

  Additional parameters.

- period:

  ISO8601-compliant time period for regular data cubes, with number and
  unit, where "D", "M" and "Y" stand for days, month and year; e.g.,
  "P16D" for 16 days.

- res:

  Spatial resolution of regularized images (in meters).

- output_dir:

  Valid directory for storing regularized images.

- timeline:

  User-defined timeline for regularized cube.

- roi:

  Region of interest (see notes below).

- crs:

  Coordinate Reference System (CRS) of the roi. (see details below).

- tiles:

  Tiles to be produced.

- grid_system:

  Grid system to be used for the output images.

- multicores:

  Number of cores used for regularization; used for parallel processing
  of input (integer)

- progress:

  show progress bar?

## Value

A `raster_cube` object with aggregated images.

## Note

The main `sits` classification workflow has the following steps:

1.  [`sits_cube`](https://e-sensing.github.io/sits/reference/sits_cube.md):
    selects a ARD image collection from a cloud provider.

2.  [`sits_cube_copy`](https://e-sensing.github.io/sits/reference/sits_cube_copy.md):
    copies an ARD image collection from a cloud provider to a local
    directory for faster processing.

3.  `sits_regularize`: create a regular data cube from an ARD image
    collection.

4.  [`sits_apply`](https://e-sensing.github.io/sits/reference/sits_apply.md):
    create new indices by combining bands of a regular data cube
    (optional).

5.  [`sits_get_data`](https://e-sensing.github.io/sits/reference/sits_get_data.md):
    extract time series from a regular data cube based on user-provided
    labelled samples.

6.  [`sits_train`](https://e-sensing.github.io/sits/reference/sits_train.md):
    train a machine learning model based on image time series.

7.  [`sits_classify`](https://e-sensing.github.io/sits/reference/sits_classify.md):
    classify a data cube using a machine learning model and obtain a
    probability cube.

8.  [`sits_smooth`](https://e-sensing.github.io/sits/reference/sits_smooth.md):
    post-process a probability cube using a spatial smoother to remove
    outliers and increase spatial consistency.

9.  [`sits_label_classification`](https://e-sensing.github.io/sits/reference/sits_label_classification.md):
    produce a classified map by selecting the label with the highest
    probability from a smoothed cube.

The regularization operation converts subsets of image collections
available in cloud providers into regular data cubes. It is an essential
part of the `sits` workflow. The input to `sits_regularize` should be an
ARD cube which includes the cloud band. The aggregation method used in
`sits_regularize` sorts the images based on cloud cover, putting images
with the least clouds at the top of the stack. Once the stack of images
is sorted, the method uses the first valid value to create the temporal
aggregation.

The "period" parameter is mandatory, and defines the time interval
between two images of the regularized cube. When combining Sentinel-1A
and Sentinel-1B images, experiments show that a 16-day period ("P16D")
are a good default. Landsat images require a longer period of one to
three months.

By default, the date of the first image of the input cube is taken as
the starting date for the regular cube. In many situations, users may
want to pre-define the required times using the "timeline" parameter.
The "timeline" parameter, if used, must contain a set of dates which are
compatible with the input cube.

To define a `roi` use one of:

- A path to a shapefile with polygons;

- A `sfc` or `sf` object from `sf` package;

- A `SpatExtent` object from `terra` package;

- A named `vector` (`"lon_min"`, `"lat_min"`, `"lon_max"`, `"lat_max"`)
  in WGS84;

- A named `vector` (`"xmin"`, `"xmax"`, `"ymin"`, `"ymax"`) with XY
  coordinates.

Defining a region of interest using `SpatExtent` or XY values not in
WGS84 requires the `crs` parameter to be specified. `sits_regularize()`
function will crop the images that contain the region of interest().

The optional `tiles` parameter indicates which tiles of the input cube
will be used for regularization.

The `grid_system` parameter allows the user to reproject the files to a
grid system which is different from that used in the ARD image
collection of the could provider. Currently, the package supports the
use of MGRS grid system and those used by the Brazil Data Cube
("BDC_LG_V2" "BDC_MD_V2" "BDC_SM_V2").

## References

Appel, Marius; Pebesma, Edzer. On-demand processing of data cubes from
satellite image collections with the gdalcubes library. Data, v. 4, n.
3, p. 92, 2019.
[doi:10.3390/data4030092](https://doi.org/10.3390/data4030092) .

## Author

Felipe Carvalho, <felipe.carvalho@inpe.br>

Rolf Simoes, <rolfsimoes@gmail.com>

## Examples

``` r
if (sits_run_examples()) {
    # define a non-regular Sentinel-2 cube in AWS
    s2_cube_open <- sits_cube(
        source = "AWS",
        collection = "SENTINEL-2-L2A",
        tiles = c("20LKP", "20LLP"),
        bands = c("B8A", "CLOUD"),
        start_date = "2018-10-01",
        end_date = "2018-11-01"
    )
    # regularize the cube
    rg_cube <- sits_regularize(
        cube = s2_cube_open,
        period = "P16D",
        res = 60,
        multicores = 2,
        output_dir = tempdir()
    )

    ## Sentinel-1 SAR
    roi <- c(
        "lon_min" = -50.410, "lon_max" = -50.379,
        "lat_min" = -10.1910, "lat_max" = -10.1573
    )
    s1_cube_open <- sits_cube(
        source = "MPC",
        collection = "SENTINEL-1-GRD",
        bands = c("VV", "VH"),
        orbit = "descending",
        roi = roi,
        start_date = "2020-06-01",
        end_date = "2020-09-28"
    )
    # regularize the cube
    rg_cube <- sits_regularize(
        cube = s1_cube_open,
        period = "P12D",
        res = 60,
        roi = roi,
        multicores = 2,
        output_dir = tempdir()
    )
}
```
