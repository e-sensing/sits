# Create a results cube from local files

Creates a data cube from local files produced by sits operations that
produces results (such as probs_cubs and class_cubes)

## Usage

``` r
# S3 method for class 'results_cube'
sits_cube(
  source,
  collection,
  ...,
  data_dir,
  tiles = NULL,
  bands,
  labels = NULL,
  parse_info = c("X1", "X2", "tile", "start_date", "end_date", "band", "version"),
  version = "v1",
  delim = "_",
  multicores = 2L,
  memsize = 2L,
  progress = TRUE
)
```

## Arguments

- source:

  Data source: one of `"AWS"`, `"BDC"`, `"CDSE"`, `"DEAFRICA"`,
  `"DEAUSTRALIA"`, `"HLS"`, `"PLANETSCOPE"`, `"MPC"`, `"SDC"` or
  `"USGS"`. This is the source from which the original data has been
  downloaded.

- collection:

  Image collection in data source from which the original data has been
  downloaded. To find out the supported collections, use
  [`sits_list_collections()`](https://e-sensing.github.io/sits/reference/sits_list_collections.md)).

- ...:

  Other parameters to be passed for specific types.

- data_dir:

  Local directory where images are stored

- tiles:

  Tiles from the collection to be included in the cube.

- bands:

  Results bands to be retrieved ("probs", "bayes", "variance", "class",
  "uncertainty")

- labels:

  Named vector with labels associated to the classes

- parse_info:

  Parsing information for local files (see notes below).

- version:

  Version of the classified and/or labelled files.

- delim:

  Delimiter for parsing local results cubes (default = "\_")

- multicores:

  Number of workers for parallel processing (integer, min = 1, max =
  2048).

- memsize:

  Memory available (in GB)

- progress:

  Logical: show a progress bar?

## Value

A `tibble` describing the contents of a data cube.

## Note

This function creates result cubes from local files produced by
classification or post-classification algorithms. In this case, the
`parse_info` is specified differently, and additional parameters are
required. The parameter `bands` should be a single character vector with
the name associated to the type of result:

- `"probs"`, for probability cubes produced by
  [`sits_classify`](https://e-sensing.github.io/sits/reference/sits_classify.md).

- `"bayes"`, for smoothed cubes produced by
  [`sits_smooth`](https://e-sensing.github.io/sits/reference/sits_smooth.md).

- `"entropy"` when using
  [`sits_uncertainty`](https://e-sensing.github.io/sits/reference/sits_uncertainty.md)
  to measure entropy in pixel classification.

- `"margin"` when using
  [`sits_uncertainty`](https://e-sensing.github.io/sits/reference/sits_uncertainty.md)
  to measure probability margin in pixel classification.

- `"least"` when using
  [`sits_uncertainty`](https://e-sensing.github.io/sits/reference/sits_uncertainty.md)
  to measure difference between 100% and most probable class in pixel
  classification.

- `"class"` for cubes produced by
  [`sits_label_classification`](https://e-sensing.github.io/sits/reference/sits_label_classification.md).

For cubes of type `"probs"`, `"bayes"`, `"class"`, the `labels`
parameter should be named vector associated to the classification
results. For `"class"` cubes, its names should be integers associated to
the values of the raster files that represent the classified cube.

Parameter `parse_info` should contain parsing information to deduce the
values of `tile`, `start_date`, `end_date` and `band` from the file
name. Default is c("X1", "X2", "tile", "start_date", "end_date",
"band"). Cubes processed by `sits` adhere to this format.

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
    output_dir <- file.path(tempdir(), "local_results")
    dir.create(output_dir, showWarnings = FALSE)
    # classify a data cube
    probs_cube <- sits_classify(
        data = cube, ml_model = rfor_model, output_dir = output_dir
    )
    # plot the probability cube
    plot(probs_cube)

    # obtain and name the labels of the local probs cube
    labels <- sits_labels(rfor_model)
    names(labels) <- seq_along(labels)

    # recover the local probability cube
    probs_local_cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = output_dir,
        bands = "probs",
        labels = labels,
        multicores = 1
    )
    # compare the two plots (they should be the same)
    plot(probs_local_cube)

    # smooth the probability cube using Bayesian statistics
    bayes_cube <- sits_smooth(probs_cube, output_dir = output_dir)
    # plot the smoothed cube
    plot(bayes_cube)

    # recover the local smoothed cube
    smooth_local_cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = output_dir,
        bands = "bayes",
        labels = labels
    )
    # compare the two plots (they should be the same)
    plot(smooth_local_cube)

    # label the probability cube
    label_cube <- sits_label_classification(
        bayes_cube,
        output_dir = output_dir
    )
    # plot the labelled cube
    plot(label_cube)

    # recover the local classified cube
    class_local_cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = output_dir,
        bands = "class",
        labels = labels
    )
    # compare the two plots (they should be the same)
    plot(class_local_cube)

    # obtain an uncertainty cube with entropy
    entropy_cube <- sits_uncertainty(
        cube = bayes_cube,
        type = "entropy",
        output_dir = output_dir
    )
    # plot entropy values
    plot(entropy_cube)

    # recover an uncertainty cube with entropy
    entropy_local_cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = output_dir,
        bands = "entropy"
    )
    # plot recovered entropy values
    plot(entropy_local_cube)

    # obtain an uncertainty cube with margin
    margin_cube <- sits_uncertainty(
        cube = bayes_cube,
        type = "margin",
        output_dir = output_dir
    )
    # plot entropy values
    plot(margin_cube)

    # recover an uncertainty cube with entropy
    margin_local_cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = output_dir,
        bands = "margin"
    )
    # plot recovered entropy values
    plot(margin_local_cube)
}
```
