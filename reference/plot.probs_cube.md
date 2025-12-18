# Plot probability cubes

plots a probability cube

## Usage

``` r
# S3 method for class 'probs_cube'
plot(
  x,
  ...,
  tile = x[["tile"]][[1L]],
  roi = NULL,
  labels = NULL,
  palette = "YlGn",
  rev = FALSE,
  quantile = NULL,
  scale = 1,
  max_cog_size = 512L,
  legend_position = "outside",
  legend_title = "probs"
)
```

## Arguments

- x:

  Object of class "probs_cube".

- ...:

  Further specifications for
  [plot](https://e-sensing.github.io/sits/reference/plot.md).

- tile:

  Tile to be plotted.

- roi:

  Spatial extent to plot in WGS 84 - named vector (see notes below)

- labels:

  Labels to plot.

- palette:

  RColorBrewer palette

- rev:

  Reverse order of colors in palette?

- quantile:

  Minimum quantile to plot

- scale:

  Scale to plot map (0.4 to 1.0)

- max_cog_size:

  Maximum size of COG overviews (lines or columns)

- legend_position:

  Where to place the legend (default = "outside")

- legend_title:

  Title of legend (default = "probs")

## Value

A plot containing probabilities associated to each class for each pixel.

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
[`sits_regularize()`](https://e-sensing.github.io/sits/reference/sits_regularize.md)
function will crop the images that contain the region of interest().

## Author

Gilberto Camara, <gilberto.camara@inpe.br>

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
    # plot the resulting probability cube
    plot(probs_cube)
}
```
