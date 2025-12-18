# Classify a regular raster cube

Called when the input is a regular raster data cube. The output is a
probability cube, which has the same tiles as the raster cube. Each tile
contains a multiband image; each band contains the probability that each
pixel belongs to a given class. Probability cubes are objects of class
"probs_cube".

## Usage

``` r
# S3 method for class 'raster_cube'
sits_classify(
  data,
  ml_model,
  ...,
  roi = NULL,
  exclusion_mask = NULL,
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

- ...:

  Other parameters for specific functions.

- roi:

  Region of interest (either an sf object, shapefile, or a numeric
  vector in WGS 84 with named XY values ("xmin", "xmax", "ymin", "ymax")
  or named lat/long values ("lon_min", "lat_min", "lon_max", "lat_max").

- exclusion_mask:

  Areas to be excluded from the classification process. It can be
  defined by a sf object or by a shapefile.

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

- verbose:

  Logical: print information about processing time?

- progress:

  Logical: Show progress bar?

## Value

Time series with predicted labels for each point (tibble of class
"sits") or a data cube with probabilities for each class (tibble of
class "probs_cube").

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

Parameter `exclusion_mask` defines a region that will not be classify.
The region can be defined by multiple polygons. Either a path to a
shapefile with polygons or a `sf` object with POLYGON or MULTIPOLYGON
geometry;

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

## Examples

``` r
if (sits_run_examples()) {
    # Retrieve the samples for Mato Grosso
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
    # classify a data cube
    probs_cube <- sits_classify(
        data = cube,
        ml_model = rf_model,
        output_dir = tempdir(),
        version = "classify"
    )
    # label the probability cube
    label_cube <- sits_label_classification(
        probs_cube,
        output_dir = tempdir(),
        version = "ex_classify"
    )
    # plot the classified image
    plot(label_cube)
}
```
