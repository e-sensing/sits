# Plot Segments

Plot vector classified cube

## Usage

``` r
# S3 method for class 'class_vector_cube'
plot(
  x,
  ...,
  tile = x[["tile"]][[1L]],
  legend = NULL,
  roi = NULL,
  seg_color = "black",
  line_width = 0.5,
  palette = "Spectral",
  scale = 1,
  legend_position = "outside"
)
```

## Arguments

- x:

  Object of class "segments".

- ...:

  Further specifications for
  [plot](https://e-sensing.github.io/sits/reference/plot.md).

- tile:

  Tile to be plotted.

- legend:

  Named vector that associates labels to colors.

- roi:

  Region of interest (see note)

- seg_color:

  Segment color.

- line_width:

  Segment line width.

- palette:

  A RColorBrewer or "cols4all" palette

- scale:

  Scale to plot map (0.4 to 1.0)

- legend_position:

  Where to place the legend (default = "outside")

## Value

A plot object with an RGB image or a B/W image on a color scale using
the chosen palette

## Note

To see which color palettes are supported, please run
cols4all::c4a_gui().

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
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir
    )
    # segment the image
    segments <- sits_segment(
        cube = cube,
        output_dir = tempdir()
    )
    # create a classification model
    rfor_model <- sits_train(samples_modis_ndvi, sits_rfor())
    # classify the segments
    probs_segs <- sits_classify(
        data = segments,
        ml_model = rfor_model,
        output_dir = tempdir()
    )
    #
    # Create a classified vector cube
    class_segs <- sits_label_classification(
        cube = probs_segs,
        output_dir = tempdir(),
        multicores = 2,
        memsize = 4
    )
    # plot the segments
    plot(class_segs)
}
```
