# Plot uncertainty vector cubes

plots a probability cube using stars

## Usage

``` r
# S3 method for class 'uncertainty_vector_cube'
plot(
  x,
  ...,
  tile = x[["tile"]][[1L]],
  roi = NULL,
  palette = "RdYlGn",
  rev = TRUE,
  scale = 1,
  legend_position = "inside"
)
```

## Arguments

- x:

  Object of class "probs_vector_cube".

- ...:

  Further specifications for
  [plot](https://e-sensing.github.io/sits/reference/plot.md).

- tile:

  Tile to be plotted.

- roi:

  Region of interest (see note)

- palette:

  RColorBrewer or "cols4all" palette

- rev:

  Reverse order of colors in palette?

- scale:

  Scale to plot map (0.4 to 1.0)

- legend_position:

  Where to place the legend (default = "inside")

## Value

A plot containing probabilities associated to each class for each pixel.

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
    # segment the image
    segments <- sits_segment(
        cube = cube,
        seg_fn = sits_snic(
            grid_seeding = "hexagonal",
            spacing = 7,
            compactness = 0.6,
            padding = 0
        ),
        output_dir = tempdir()
    )
    # classify a data cube
    probs_vector_cube <- sits_classify(
        data = segments,
        ml_model = rfor_model,
        output_dir = tempdir()
    )
    # measure uncertainty
    uncert_vector_cube <- sits_uncertainty(
        cube = probs_vector_cube,
        type = "margin",
        output_dir = tempdir()
    )
    # plot the resulting uncertainty cube
    plot(uncert_vector_cube)
}
```
