# Classify a segmented data cube

This function is called when the input is a vector data cube. Vector
data cubes are produced when closed regions are obtained from raster
data cubes using
[`sits_segment`](https://e-sensing.github.io/sits/reference/sits_segment.md).
Classification of a vector data cube produces a vector data structure
with additional columns expressing the class probabilities for each
segment. Probability cubes for vector data cubes are objects of class
"probs_vector_cube".

## Usage

``` r
# S3 method for class 'vector_cube'
sits_classify(
  data,
  ml_model,
  ...,
  roi = NULL,
  filter_fn = NULL,
  impute_fn = impute_linear(),
  start_date = NULL,
  end_date = NULL,
  memsize = 8L,
  multicores = 2L,
  gpu_memory = 4L,
  batch_size = 2L^gpu_memory,
  output_dir,
  version = "v1",
  n_sam_pol = 15L,
  verbose = FALSE,
  progress = TRUE
)
```

## Arguments

- data:

  Data cube (tibble of class "raster_cube")

- ml_model:

  R model trained by
  [`sits_train`](https://e-sensing.github.io/sits/reference/sits_train.md)
  (closure of class "sits_model")

- ...:

  Other parameters for specific functions.

- roi:

  Region of interest (either an sf object, shapefile, or a numeric
  vector in WGS 84 with named XY values ("xmin", "xmax", "ymin", "ymax")
  or named lat/long values ("lon_min", "lat_min", "lon_max", "lat_max").

- filter_fn:

  Smoothing filter to be applied - optional (closure containing object
  of class "function").

- impute_fn:

  Imputation function to remove NA.

- start_date:

  Starting date for the classification (Date in YYYY-MM-DD format).

- end_date:

  Ending date for the classification (Date in YYYY-MM-DD format).

- memsize:

  Memory available for classification in GB (integer, min = 1, max =
  16384).

- multicores:

  Number of cores to be used for classification (integer, min = 1, max =
  2048).

- gpu_memory:

  Memory available in GPU in GB (default = 4)

- batch_size:

  Batch size for GPU classification.

- output_dir:

  Directory for output file.

- version:

  Version of the output.

- n_sam_pol:

  Number of time series per segment to be classified (integer, min = 10,
  max = 50).

- verbose:

  Logical: print information about processing time?

- progress:

  Logical: Show progress bar?

## Value

Vector data cube with probabilities for each class included in new
columns of the tibble. (tibble of class "probs_vector_cube").

## Note

The `roi` parameter defines a region of interest. Either:

1.  A path to a shapefile with polygons;

2.  An `sf` object with POLYGON or MULTIPOLYGON geometry;

3.  A named XY vector (`xmin`, `xmax`, `ymin`, `ymax`) in WGS84;

4.  A name lat/long vector (`lon_min`, `lon_max`, `lat_min`, `lat_max`);

Parameter `filter_fn` parameter specifies a smoothing filter to be
applied to each time series for reducing noise. Currently, options are
Savitzky-Golay (see
[`sits_sgolay`](https://e-sensing.github.io/sits/reference/sits_sgolay.md))
and Whittaker (see
[`sits_whittaker`](https://e-sensing.github.io/sits/reference/sits_whittaker.md))
filters.

Parameter `impute_fn` defines a 1D function that will be used to
interpolate NA values in each time series. Currently sits supports the
[`impute_linear`](https://e-sensing.github.io/sits/reference/impute_linear.md)
function, but users can define imputation functions which are defined
externally.

Parameter `memsize` controls the amount of memory available for
classification, while `multicores` defines the number of cores used for
processing. We recommend using as much memory as possible.

For classifying vector data cubes created by
[`sits_segment`](https://e-sensing.github.io/sits/reference/sits_segment.md),
`n_sam_pol` controls is the number of time series to be classified per
segment.

When using a GPU for deep learning, `gpu_memory` indicates the memory of
the graphics card which is available for processing. The parameter
`batch_size` defines the size of the matrix (measured in number of rows)
which is sent to the GPU for classification. Users can test different
values of `batch_size` to find out which one best fits their GPU
architecture.

It is not possible to have an exact idea of the size of Deep Learning
models in GPU memory, as the complexity of the model and factors such as
CUDA Context increase the size of the model in memory. Therefore, we
recommend that you leave at least 1GB free on the video card to store
the Deep Learning model that will be used.

For users of Apple M3 chips or similar with a Neural Engine, be aware
that these chips share memory between the GPU and the CPU. Tests
indicate that the `memsize` should be set to half to the total memory
and the `batch_size` parameter should be a small number (we suggest the
value of 64). Be aware that increasing these parameters may lead to
memory conflicts.

Please refer to the sits documentation available in
<https://e-sensing.github.io/sitsbook/> for detailed examples.

## Examples

``` r
if (sits_run_examples()) {
    # train a random forest model
    rf_model <- sits_train(samples_modis_ndvi, ml_method = sits_rfor)
    # Example of classification of a data cube
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
        seg_fn = sits_slic(
            step = 5,
            compactness = 1,
            dist_fun = "euclidean",
            avg_fun = "median",
            iter = 50,
            minarea = 10,
            verbose = FALSE
        ),
        output_dir = tempdir()
    )
    # Create a classified vector cube
    probs_segs <- sits_classify(
        data = segments,
        ml_model = rf_model,
        output_dir = tempdir(),
        multicores = 4,
        n_sam_pol = 15,
        version = "segs"
    )
    # Create a labelled vector cube
    class_segs <- sits_label_classification(
        cube = probs_segs,
        output_dir = tempdir(),
        multicores = 2,
        memsize = 4,
        version = "segs_classify"
    )
    # plot class_segs
    plot(class_segs)
}
```
