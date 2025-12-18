# Plot SAR data cubes

Plot SAR raster cube

## Usage

``` r
# S3 method for class 'sar_cube'
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
  palette = "Greys",
  rev = FALSE,
  scale = 1,
  first_quantile = 0.05,
  last_quantile = 0.95,
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

A plot object with an RGB image or a B/W image on a color scale for SAR
cubes

## Note

Use `scale` parameter for general output control. The `dates` parameter
indicates the date allows plotting of different dates when a single band
and three dates are provided, \`sits\` will plot a multi-temporal RGB
image for a single band (useful in the case of SAR data). For RGB bands
with multi-dates, multiple plots will be produced.

The following optional parameters are available to allow for detailed
control over the plot output:

- `graticules_labels_size`: size of coord labels (default = 0.7)

- `legend_title_size`: relative size of legend title (default = 0.7)

- `legend_text_size`: relative size of legend text (default = 0.7)

- `legend_bg_color`: color of legend background (default = "white")

- `legend_bg_alpha`: legend opacity (default = 0.3)

## Author

Gilberto Camara, <gilberto.camara@inpe.br>

## Examples

``` r
if (sits_run_examples()) {
    # create a SAR data cube from cloud services
    cube_s1_grd <- sits_cube(
        source = "MPC",
        collection = "SENTINEL-1-GRD",
        bands = c("VV", "VH"),
        orbit = "descending",
        tiles = c("21LUJ"),
        start_date = "2021-08-01",
        end_date = "2021-09-30"
    )
    # plot VH band of the first date of the data cube
    plot(cube_s1_grd, band = "VH")
}
```
