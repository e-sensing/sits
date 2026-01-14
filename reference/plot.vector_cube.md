# Plot RGB vector data cubes

Plot vector data cube with segments on top of raster image. Vector cubes
have both a vector and a raster component. The vector part are the
segments produced by
[`sits_segment`](https://e-sensing.github.io/sits/reference/sits_segment.md).
Their visual output is controlled by "seg_color" and "line_width"
parameters. The raster output works in the same way as the false color
and RGB plots.

## Usage

``` r
# S3 method for class 'vector_cube'
plot(
  x,
  ...,
  band = NULL,
  red = NULL,
  green = NULL,
  blue = NULL,
  tile = x[["tile"]][[1L]],
  dates = NULL,
  roi = NULL,
  seg_color = "black",
  line_width = 0.2,
  palette = "RdYlGn",
  rev = FALSE,
  scale = 1,
  first_quantile = 0.02,
  last_quantile = 0.98,
  max_cog_size = 1024L,
  legend_position = "inside"
)
```

## Arguments

- x:

  Object of class "raster_cube".

- ...:

  Further specifications for
  [plot](https://e-sensing.github.io/sits/reference/plot.md).

- band:

  Band for plotting grey images.

- red:

  Band for red color.

- green:

  Band for green color.

- blue:

  Band for blue color.

- tile:

  Tile to be plotted.

- dates:

  Dates to be plotted.

- roi:

  Spatial extent to plot in WGS 84 - (see notes)

- seg_color:

  Color to show the segment boundaries

- line_width:

  Line width to plot the segments boundary (in pixels)

- palette:

  An RColorBrewer or "cols4all" palette

- rev:

  Reverse the color order in the palette?

- scale:

  Scale to plot map (0.4 to 1.5)

- first_quantile:

  First quantile for stretching images

- last_quantile:

  Last quantile for stretching images

- max_cog_size:

  Maximum size of COG overviews (lines or columns)

- legend_position:

  Where to place the legend (default = "inside")

## Value

A plot object with an RGB image or a B/W image on a color scale using
the palette

## Note

To see which color palettes are supported, please run
cols4all::c4a_gui(). The following optional parameters are available to
allow for detailed control over the plot output:

- `graticules_labels_size`: size of coord labels (default = 0.7)

- `legend_title_size`: relative size of legend title (default = 0.7)

- `legend_text_size`: relative size of legend text (default = 0.7)

- `legend_bg_color`: color of legend background (default = "white")

- `legend_bg_alpha`: legend opacity (default = 0.3)

To define a `roi` use one of:

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
    # create a data cube from local files
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir
    )
    # Segment the cube
    segments <- sits_segment(
        cube = cube,
        output_dir = tempdir(),
        multicores = 2,
        memsize = 4
    )
    # plot NDVI band of the second date date of the data cube
    plot(segments, band = "NDVI", date = sits_timeline(cube)[1])
}
```
