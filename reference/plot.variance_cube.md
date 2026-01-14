# Plot variance cubes

Plots a variance cube, useful to understand how local smoothing will
work.

## Usage

``` r
# S3 method for class 'variance_cube'
plot(
  x,
  ...,
  tile = x[["tile"]][[1L]],
  roi = NULL,
  labels = NULL,
  palette = "YlGnBu",
  rev = FALSE,
  type = "map",
  quantile = 0.75,
  scale = 1,
  max_cog_size = 1024L,
  legend_position = "inside",
  legend_title = "logvar"
)
```

## Arguments

- x:

  Object of class "variance_cube".

- ...:

  Further specifications for
  [plot](https://e-sensing.github.io/sits/reference/plot.md).

- tile:

  Tile to be plotted.

- roi:

  Spatial extent to plot (see notes)

- labels:

  Labels to plot.

- palette:

  RColorBrewer or "cols4all" palette

- rev:

  Reverse order of colors in palette?

- type:

  Type of plot ("map" or "hist")

- quantile:

  Minimum quantile to plot

- scale:

  Scale to plot map (0.4 to 1.0)

- max_cog_size:

  Maximum size of COG overviews (lines or columns)

- legend_position:

  Where to place the legend (default = "inside")

- legend_title:

  Title of legend (default = "probs")

## Value

A plot containing local variances associated to the logit probability
for each pixel and each class.

## Note

To see which color palettes are supported, please run
cols4all::c4a_gui(). To define a `roi` use one of:

- A path to a shapefile with polygons;

- A `sfc` or `sf` object from `sf` package;

- A `SpatExtent` object from `terra` package;

- A named `vector` (`"lon_min"`, `"lat_min"`, `"lon_max"`, `"lat_max"`)
  in WGS84;

- A named `vector` (`"xmin"`, `"xmax"`, `"ymin"`, `"ymax"`) with XY
  coordinates.

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
    # obtain a variance cube
    var_cube <- sits_variance(probs_cube, output_dir = tempdir())
    # plot the variance cube
    plot(var_cube)
}
```
