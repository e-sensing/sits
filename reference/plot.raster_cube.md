# Plot RGB data cubes

Plot RGB raster cube

## Usage

``` r
# S3 method for class 'raster_cube'
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

  Dates to be plotted

- roi:

  Spatial extent to plot in WGS 84 - named vector with either (lon_min,
  lon_max, lat_min, lat_max) or (xmin, xmax, ymin, ymax)

- palette:

  An RColorBrewer palette

- rev:

  Reverse the color order in the palette?

- scale:

  Scale to plot map (0.4 to 1.0)

- first_quantile:

  First quantile for stretching images

- last_quantile:

  Last quantile for stretching images

- max_cog_size:

  Maximum size of COG overviews (lines or columns)

- legend_position:

  Where to place the legend (default = "inside")

## Value

A plot object with an RGB image or a B/W image on a color scale

## Note

Use `scale` parameter for general output control. The `dates` parameter
indicates the date allows plotting of different dates when a single band
and three dates are provided, \`sits\` will plot a multi-temporal RGB
image for a single band (useful in the case of SAR data). For RGB bands
with multi-dates, multiple plots will be produced.

If the user does not provide band names for b/w or RGB plots, and also
does not provide dates, `plot.raster_cube` tries to display some
reasonable color composites, using the following algorithm:

1.  Each image in `sits` is associated to a source and a collection
    (e.g, "MPC" and "SENTINEL-2-L2A").

2.  For each source/collection pair, `sits` has a set of possible color
    composites stored in "./extdata/config_colors.yml". For example, the
    following composites are available for all "SENTINEL-2" images:

    - AGRICULTURE: ("B11", "B08", "B02")

    - AGRICULTURE2: ("B11", "B8A", "B02")

    - SWIR: ("B11", "B08", "B04")

    - SWIR2: ("B12", "B08", "B04")

    - SWIR3: ("B12", "B8A", "B04")

    - RGB: ("B04", "B03", "B02")

    - RGB-FALSE1 : ("B08", "B06", "B04")

    - RGB-FALSE2 : ("B08", "B11", "B04")

3.  `sits` tries to find if the bands required for one of the color
    composites are part of the cube. If they exist, that RGB composite
    is selected. Otherwise, the first available band is chosen.

4.  After selecting the bands, the algorithm looks for the date with the
    smallest percentage of cloud cover and selects that date to be
    displayed.

. The following optional parameters are available to allow for detailed
control over the plot output:

- `graticules_labels_size`: size of coord labels (default = 0.7)

- `legend_title_size`: size of legend title (default = 0.7)

- `legend_text_size`: size of legend text (default = 0.7)

- `legend_bg_color`: color of legend background (default = "white")

- `legend_bg_alpha`: legend opacity (default = 0.3)

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
    # plot NDVI band of the least cloud cover date
    plot(cube)
}
```
