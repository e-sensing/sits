# Segment an image using SLIC

Apply a segmentation on a data cube using either the `supercells` or
`snic` packages, depending on the chosen algorithm. As of version 1.5.4,
two segmentation methods are supported. The recommended option is SNIC,
implemented via the `snic` package, which applies a non-iterative
clustering strategy to generate compact, homogeneous superpixels from
uniformly distributed seeds (Achanta and Susstrunk, 2017). The
alternative method uses the SLIC algorithm implemented in the
`supercells` package, adapted for remote sensing data following Achanta
et al. (2012). This SLIC variant is deprecated and will be removed in a
future release. See references for more details.

## Usage

``` r
sits_slic(
  data = NULL,
  step = 30L,
  compactness = 1,
  dist_fun = "euclidean",
  avg_fun = "median",
  iter = 30L,
  minarea = 10L,
  verbose = FALSE
)
```

## Arguments

- data:

  A matrix with time series.

- step:

  Distance (in number of cells) between initial supercells' centers

- compactness:

  A compactness value. Larger values cause clusters to be more
  compact/even (square).

- dist_fun:

  Distance function. Currently implemented: `euclidean, jsd, dtw`, and
  any distance function from the `philentropy` package. See
  [`philentropy::getDistMethods()`](https://drostlab.github.io/philentropy/reference/getDistMethods.html).

- avg_fun:

  Averaging function to calculate the values of the supercells' centers.
  Accepts any fitting R function (e.g., base::mean() or stats::median())
  or one of internally implemented "mean" and "median". Default:
  "median"

- iter:

  Number of iterations to create the output.

- minarea:

  Specifies the minimal size of a supercell (in cells).

- verbose:

  Show the progress bar?

## Value

Set of segments for a single tile

## References

Achanta, Radhakrishna, Appu Shaji, Kevin Smith, Aurelien Lucchi, Pascal
Fua, and Sabine Süsstrunk. 2012. “SLIC Superpixels Compared to
State-of-the-Art Superpixel Methods.” IEEE Transactions on Pattern
Analysis and Machine Intelligence 34 (11): 2274–82.

Nowosad, Jakub, and Tomasz F. Stepinski. 2022. “Extended SLIC
Superpixels Algorithm for Applications to Non-Imagery Geospatial
Rasters.” International Journal of Applied Earth Observation and
Geoinformation 112 (August): 102935.

## Author

Rolf Simoes, <rolfsimoes@gmail.com>

Felipe Carvalho, <felipe.carvalho@inpe.br>

Felipe Carlos, <efelipecarlos@gmail.com>

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
            compactness = 0.3,
            padding = 0
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
