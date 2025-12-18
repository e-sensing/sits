# Mosaic classified cubes

Creates a mosaic of all tiles of a sits cube. Mosaics can be created
from both regularized ARD images or from classified maps. In the case of
ARD images, a mosaic will be produce for each band/date combination. It
is better to first regularize the data cubes and then use `sits_mosaic`.

## Usage

``` r
sits_mosaic(
  cube,
  crs = "EPSG:3857",
  roi = NULL,
  multicores = 2L,
  output_dir,
  res = NULL,
  version = "v1",
  progress = TRUE
)
```

## Arguments

- cube:

  A sits data cube.

- crs:

  A target coordinate reference system of raster mosaic. The provided
  crs could be a string (e.g, "EPSG:4326" or a proj4string), or an EPSG
  code number (e.g. 4326). Default is "EPSG:3857" - WGS 84 /
  Pseudo-Mercator.

- roi:

  Region of interest (see below).

- multicores:

  Number of cores that will be used to crop the images in parallel.

- output_dir:

  Directory for output images.

- res:

  Spatial resolution of the mosaic. Default is NULL.

- version:

  Version of resulting image (in the case of multiple tests)

- progress:

  Show progress bar? Default is TRUE.

## Value

a sits cube with only one tile.

## Note

To define a `roi` use one of:

- A path to a shapefile with polygons;

- A `sfc` or `sf` object from `sf` package;

- A `SpatExtent` object from `terra` package;

- A named `vector` (`"lon_min"`, `"lat_min"`, `"lon_max"`, `"lat_max"`)
  in WGS84;

- A named `vector` (`"xmin"`, `"xmax"`, `"ymin"`, `"ymax"`) with XY
  coordinates.

The user should specify the CRS of the mosaic. We use "EPSG:3857"
(Pseudo-Mercator) as the default.

## Author

Felipe Carvalho, <felipe.carvalho@inpe.br>

Rolf Simoes, <rolfsimoes@gmail.com>

Felipe Carlos, <efelipecarlos@gmail.com>

## Examples

``` r
if (sits_run_examples()) {
    # create a random forest model
    rfor_model <- sits_train(samples_modis_ndvi, sits_rfor())
    # create a data cube from local files
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir
    )
    # classify a data cube
    probs_cube <- sits_classify(
        data = cube, ml_model = rfor_model, output_dir = tempdir()
    )
    # smooth the probability cube using Bayesian statistics
    bayes_cube <- sits_smooth(probs_cube, output_dir = tempdir())
    # label the probability cube
    label_cube <- sits_label_classification(
        bayes_cube,
        output_dir = tempdir()
    )
    # create roi
    roi <- sf::st_sfc(
        sf::st_polygon(
            list(rbind(
                c(-55.64768, -11.68649),
                c(-55.69654, -11.66455),
                c(-55.62973, -11.61519),
                c(-55.64768, -11.68649)
            ))
        ),
        crs = "EPSG:4326"
    )
    # crop and mosaic classified image
    mosaic_cube <- sits_mosaic(
        cube = label_cube,
        roi = roi,
        crs = "EPSG:4326",
        output_dir = tempdir()
    )
}
```
