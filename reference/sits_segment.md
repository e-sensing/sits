# Segment an image

Apply a spatial-temporal segmentation on a data cube based on a user
defined segmentation function. The function applies the segmentation
algorithm "seg_fn" to each tile. The output is a vector data cube, which
is a data cube with an additional vector file in "geopackage" format.

## Usage

``` r
sits_segment(
  cube,
  seg_fn = sits_snic(),
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

As of version 1.5.4, two segmentation functions are available. The
preferred option is
[`sits_snic`](https://e-sensing.github.io/sits/reference/sits_snic.md),
which implements the Simple Non-Iterative Clustering (SNIC) algorithm to
generate compact and homogeneous superpixels directly from uniformly
distributed seeds. SNIC avoids the iterative refinement step used in
SLIC and is generally faster and more memory-efficient, making it
suitable for large multispectral or multitemporal data cubes.

The previous function
[`sits_slic`](https://e-sensing.github.io/sits/reference/sits_slic.md),
based on the Simple Linear Iterative Clustering (SLIC) algorithm as
adapted by Nowosad and Stepinski for multispectral and multitemporal
imagery, remains available but is now deprecated and will be removed in
a future release. SLIC clusters pixels using spectral similarity and
spatial–temporal proximity to produce nearly uniform superpixels, but
its iterative nature makes it less efficient for large-scale Earth
observation workflows.

The result of `sits_segment` is a data cube tibble with an additional
vector file in the `geopackage` format. The location of the vector file
is included in the data cube tibble in a new column, called
`vector_info`.

## References

Achanta, Radhakrishna, and Sabine Susstrunk. 2017. “Superpixels and
Polygons Using Simple Non-Iterative Clustering.” Proceedings of the IEEE
Conference on Computer Vision and Pattern Recognition, 4651–60.

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
        seg_fn = sits_snic(
            grid_seeding = "diamond",
            spacing = 15,
            compactness = 0.5,
            padding = 2
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
