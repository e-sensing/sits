# Copy the images of a cube to a local directory

This function downloads the images of a cube in parallel. A region of
interest (`roi`) can be provided to crop the images and a resolution
(`res`) to resample the bands. `sits_cube_copy` is useful to improve
processing time in the regularization operation.

## Usage

``` r
sits_cube_copy(
  cube,
  roi = NULL,
  res = NULL,
  crs = NULL,
  n_tries = 3L,
  multicores = 2L,
  output_dir,
  progress = TRUE
)
```

## Arguments

- cube:

  A data cube (class "raster_cube")

- roi:

  Region of interest. Either:

  1.  A path to a shapefile with polygons;

  2.  A `sf` object from `sf` package;

  3.  A named `vector` (`"lon_min"`, `"lat_min"`, `"lon_max"`,
      `"lat_max"`) in WGS84;

  4.  A named `vector` (`"xmin"`, `"xmax"`, `"ymin"`, `"ymax"`) with XY
      coordinates in WGS84.

- res:

  An integer value corresponds to the output spatial resolution of the
  images. Default is NULL.

- crs:

  The Coordinate Reference System (CRS) of the roi. (see details below).

- n_tries:

  Number of attempts to download the same image. Default is 3.

- multicores:

  Number of cores for parallel downloading (integer, min = 1, max =
  2048).

- output_dir:

  Output directory where images will be saved. (character vector of
  length 1).

- progress:

  Logical: show progress bar?

## Value

Copy of input data cube (class "raster cube").

The main `sits` classification workflow has the following steps:

1.  [`sits_cube`](https://e-sensing.github.io/sits/reference/sits_cube.md):
    selects a ARD image collection from a cloud provider.

2.  `sits_cube_copy`: copies an ARD image collection from a cloud
    provider to a local directory for faster processing.

3.  [`sits_regularize`](https://e-sensing.github.io/sits/reference/sits_regularize.md):
    create a regular data cube from an ARD image collection.

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

The `roi` parameter is used to crop cube images. To define a `roi` use
one of:

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

Felipe Carlos, <efelipecarlos@gmail.com>

Felipe Carvalho, <felipe.carvalho@inpe.br>

## Examples

``` r
if (sits_run_examples()) {
    # Creating a sits cube from BDC
    bdc_cube <- sits_cube(
        source = "BDC",
        collection = "CBERS-WFI-16D",
        tiles = c("007004", "007005"),
        bands = c("B15", "CLOUD"),
        start_date = "2018-01-01",
        end_date = "2018-01-12"
    )
    # Downloading images to a temporary directory
    cube_local <- sits_cube_copy(
        cube = bdc_cube,
        output_dir = tempdir(),
        roi = c(
            lon_min = -46.5,
            lat_min = -45.5,
            lon_max = -15.5,
            lat_max = -14.6
        ),
        multicores = 2L,
        res = 250
    )
}
```
