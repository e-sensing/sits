# Segment an image

Apply a spatial-temporal segmentation on a data cube based on a user
defined segmentation function. The function applies the segmentation
algorithm "seg_fn" to each tile. The output is a vector data cube, which
is a data cube with an additional vector file in "geopackage" format.

## Usage

``` r
sits_segment(
  cube,
  seg_fn = sits_slic(),
  roi = NULL,
  impute_fn = impute_linear(),
  start_date = NULL,
  end_date = NULL,
  memsize = 4L,
  multicores = 2L,
  output_dir,
  version = "v1",
  progress = TRUE
)
```

## Arguments

- cube:

  Regular data cube

- seg_fn:

  Function to apply the segmentation

- roi:

  Region of interest (see below)

- impute_fn:

  Imputation function to remove NA values.

- start_date:

  Start date for the segmentation

- end_date:

  End date for the segmentation.

- memsize:

  Memory available for classification (in GB).

- multicores:

  Number of cores to be used for classification.

- output_dir:

  Directory for output file.

- version:

  Version of the output (for multiple segmentations).

- progress:

  Show progress bar?

## Value

A tibble of class 'segs_cube' representing the segmentation.

## Note

Segmentation requires the following steps:

1.  Create a regular data cube with
    [`sits_cube`](https://e-sensing.github.io/sits/reference/sits_cube.md)
    and
    [`sits_regularize`](https://e-sensing.github.io/sits/reference/sits_regularize.md);

2.  Run `sits_segment` to obtain a vector data cube with polygons that
    define the boundary of the segments;

3.  Classify the time series associated to the segments with
    [`sits_classify`](https://e-sensing.github.io/sits/reference/sits_classify.md),
    to get obtain a vector probability cube;

4.  Use
    [`sits_label_classification`](https://e-sensing.github.io/sits/reference/sits_label_classification.md)
    to label the vector probability cube;

5.  Display the results with
    [`plot`](https://e-sensing.github.io/sits/reference/plot.md) or
    [`sits_view`](https://e-sensing.github.io/sits/reference/sits_view.md).

The "roi" parameter defines a region of interest. It can be an
sf_object, a shapefile, or a bounding box vector with named XY values
("xmin", "xmax", "ymin", "ymax") or named lat/long values ("lon_min",
"lat_min", "lon_max", "lat_max").

As of version 1.5.3, the only `seg_fn` function available is
[`sits_slic`](https://e-sensing.github.io/sits/reference/sits_slic.md),
which uses the Simple Linear Iterative Clustering (SLIC) algorithm that
clusters pixels to generate compact, nearly uniform superpixels. This
algorithm has been adapted by Nowosad and Stepinski to work with
multispectral and multitemporal images. SLIC uses spectral similarity
and proximity in the spectral and temporal space to segment the image
into superpixels. Superpixels are clusters of pixels with similar
spectral and temporal responses that are spatially close.

The result of `sits_segment` is a data cube tibble with an additional
vector file in the `geopackage` format. The location of the vector file
is included in the data cube tibble in a new column, called
`vector_info`.

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

Gilberto Camara, <gilberto.camara@inpe.br>

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
        seg_fn = sits_slic(
            step = 10,
            compactness = 1,
            dist_fun = "euclidean",
            avg_fun = "median",
            iter = 30,
            minarea = 10
        ),
        output_dir = tempdir()
    )
    # create a classification model
    rfor_model <- sits_train(samples_modis_ndvi, sits_rfor())
    # classify the segments
    seg_probs <- sits_classify(
        data = segments,
        ml_model = rfor_model,
        output_dir = tempdir()
    )
    # label the probability segments
    seg_label <- sits_label_classification(
        cube = seg_probs,
        output_dir = tempdir()
    )
}
```
