# Plot DEM cubes

Plot RGB raster cube

## Usage

``` r
# S3 method for class 'dem_cube'
plot(
  x,
  ...,
  band = "ELEVATION",
  tile = x[["tile"]][[1L]],
  roi = NULL,
  palette = "Spectral",
  rev = TRUE,
  scale = 1,
  max_cog_size = 1024L,
  legend_position = "inside"
)
```

## Arguments

- x:

  Object of class "dem_cube".

- ...:

  Further specifications for
  [plot](https://e-sensing.github.io/sits/reference/plot.md).

- band:

  Band for plotting grey images.

- tile:

  Tile to be plotted.

- roi:

  Spatial extent to plot in WGS 84 - named vector with either (lon_min,
  lon_max, lat_min, lat_max) or (xmin, xmax, ymin, ymax)

- palette:

  An RColorBrewer palette

- rev:

  Reverse the color order in the palette?

- scale:

  Scale to plot map (0.4 to 1.0)

- max_cog_size:

  Maximum size of COG overviews (lines or columns)

- legend_position:

  Where to place the legend (default = "inside")

## Value

A plot object with a DEM cube or a B/W image on a color scale

## Note

Use `scale` parameter for general output control.

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
    # obtain the DEM cube
    dem_cube_19HBA <- sits_cube(
        source = "MPC",
        collection = "COP-DEM-GLO-30",
        bands = "ELEVATION",
        tiles = "19HBA"
    )
    # plot the DEM reversing the palette
    plot(dem_cube_19HBA, band = "ELEVATION")
}
```
