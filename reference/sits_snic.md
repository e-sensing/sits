# Segment an image using SNIC

Apply a segmentation on a data cube based on the `snic` package. This is
an adaptation and extension to remote sensing data of the SNIC
superpixels algorithm proposed by Achanta and Süsstrunk (2017). See
reference for more details.

## Usage

``` r
sits_snic(
  data = NULL,
  grid_seeding = "rectangular",
  spacing = 10,
  compactness = 0.5,
  padding = floor(spacing/2)
)
```

## Arguments

- data:

  A matrix with time series.

- grid_seeding:

  Method for grid seeding (one of "rectangular", "diamond", "hexagonal",
  "random").

- spacing:

  Distance (in number of cells) between initial supercells' centers

- compactness:

  A compactness value. Larger values cause clusters to be more
  compact/even (square).

- padding:

  Distance (in pixels) from the image borders within which no seeds are
  placed.

## References

"Superpixels and Polygons Using Simple Non-Iterative Clustering", R.
Achanta and S. Süsstrunk, CVPR 2017.

## Author

Rolf Simoes, <rolfsimoes@gmail.com>

Gilberto Camara, <gilberto.camara@inpe.br>

Felipe Carlos, <efelipecarlos@gmail.com>

Felipe Carvalho, <felipe.carvalho@inpe.br>

## Examples

``` r
if (sits_run_examples()) {
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    # create a data cube
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir
    )
    # segment the vector cube
    segments <- sits_segment(
        cube = cube,
        seg_fn = sits_snic(
            grid_seeding = "rectangular",
            spacing = 10,
            compactness = 0.5,
            padding = 5
        ),
        output_dir = tempdir(),
        version = "snic-demo"
    )
    # create a classification model
    rfor_model <- sits_train(samples_modis_ndvi, sits_rfor())
    # classify the segments
    seg_probs <- sits_classify(
        data = segments,
        ml_model = rfor_model,
        output_dir = tempdir(),
        version = "snic-demo"
    )
    # label the probability segments
    seg_label <- sits_label_classification(
        cube = seg_probs,
        output_dir = tempdir(),
        version = "snic-demo"
    )
    plot(seg_label)
}
```
