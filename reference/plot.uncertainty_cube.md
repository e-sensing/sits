# Plot uncertainty cubes

plots a uncertainty cube

## Usage

``` r
# S3 method for class 'uncertainty_cube'
plot(
  x,
  ...,
  tile = x[["tile"]][[1L]],
  roi = NULL,
  palette = "RdYlGn",
  rev = TRUE,
  scale = 1,
  first_quantile = 0.02,
  last_quantile = 0.98,
  max_cog_size = 1024L,
  legend_position = "inside"
)
```

## Arguments

- x:

  Object of class "probs_image".

- ...:

  Further specifications for
  [plot](https://e-sensing.github.io/sits/reference/plot.md).

- tile:

  Tiles to be plotted.

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

A plot object produced showing the uncertainty associated to each
classified pixel.

## Note

The following optional parameters are available to allow for detailed
control over the plot output:

- `graticules_labels_size`: size of coord labels (default = 0.7)

- `legend_title_size`: relative size of legend title (default = 1.0)

- `legend_text_size`: relative size of legend text (default = 1.0)

- `legend_bg_color`: color of legend background (default = "white")

- `legend_bg_alpha`: legend opacity (default = 0.5)

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
    # calculate uncertainty
    uncert_cube <- sits_uncertainty(probs_cube, output_dir = tempdir())
    # plot the resulting uncertainty cube
    plot(uncert_cube)
}
```
