# Plot classified images

plots a classified raster using tmap.

## Usage

``` r
# S3 method for class 'class_cube'
plot(
  x,
  y,
  ...,
  tile = x[["tile"]][[1L]],
  roi = NULL,
  legend = NULL,
  palette = "Spectral",
  scale = 1,
  max_cog_size = 1024L,
  legend_position = "outside"
)
```

## Arguments

- x:

  Object of class "class_cube".

- y:

  Ignored.

- ...:

  Further specifications for
  [plot](https://e-sensing.github.io/sits/reference/plot.md).

- tile:

  Tile to be plotted.

- roi:

  Spatial extent to plot (see note)

- legend:

  Named vector that associates labels to colors.

- palette:

  A RColorBrewer or "cols4all" palette

- scale:

  Relative scale (0.4 to 1.0) of plot text

- max_cog_size:

  Maximum size of COG overviews (lines or columns)

- legend_position:

  Where to place the legend (default = "outside")

## Value

A color map, where each pixel has the color associated to a label, as
defined by the legend parameter.

## Note

To see which color palettes are supported, please run
cols4all::c4a_gui(). The following optional parameters are available to
allow for detailed control over the plot output:

- `graticules_labels_size`: size of coord labels (default = 0.8)

- `legend_title_size`: relative size of legend title (default = 1.0)

- `legend_text_size`: relative size of legend text (default = 1.0)

- `legend_bg_color`: color of legend background (default = "white")

- `legend_bg_alpha`: legend opacity (default = 0.5)

\#' To define a `roi` use one of:

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
    # label cube with the most likely class
    label_cube <- sits_label_classification(
        probs_cube,
        output_dir = tempdir()
    )
    # plot the resulting classified image
    plot(label_cube)
}
```
