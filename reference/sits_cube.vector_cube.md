# Create a vector cube from local files

Creates a data cube from local files which include a vector file
produced by a segmentation algorithm.

## Usage

``` r
# S3 method for class 'vector_cube'
sits_cube(
  source,
  collection,
  ...,
  raster_cube,
  vector_dir,
  vector_band,
  parse_info = c("X1", "X2", "tile", "start_date", "end_date", "band", "version"),
  version = "v1",
  delim = "_",
  multicores = 2L,
  progress = TRUE
)
```

## Arguments

- source:

  Data source: one of `"AWS"`, `"BDC"`, `"CDSE"`, `"DEAFRICA"`,
  `"DEAUSTRALIA"`, `"HLS"`, `"PLANETSCOPE"`, `"MPC"`, `"SDC"` or
  `"USGS"`. This is the source from which the data has been downloaded.

- collection:

  Image collection in data source. To find out the supported
  collections, use
  [`sits_list_collections()`](https://e-sensing.github.io/sits/reference/sits_list_collections.md)).

- ...:

  Other parameters to be passed for specific types.

- raster_cube:

  Raster cube to be merged with vector data

- vector_dir:

  Local directory where vector files are stored

- vector_band:

  Band for vector cube ("segments", "probs", "class")

- parse_info:

  Parsing information for local image files

- version:

  Version of the classified and/or labelled files.

- delim:

  Delimiter for parsing local files (default = "\_")

- multicores:

  Number of workers for parallel processing (integer, min = 1, max =
  2048).

- progress:

  Logical: show a progress bar?

## Value

A `tibble` describing the contents of a data cube.

## Note

This function creates vector cubes from local files produced by
[`sits_segment`](https://e-sensing.github.io/sits/reference/sits_segment.md),
[`sits_classify`](https://e-sensing.github.io/sits/reference/sits_classify.md)
or
[`sits_label_classification`](https://e-sensing.github.io/sits/reference/sits_label_classification.md)
when the output is a vector cube. In this case, `parse_info` is
specified differently as
`c("X1", "X2", "tile", "start_date", "end_date", "band")`. The parameter
`vector_dir` is the directory where the vector file is stored. Parameter
`vector_band` is band name of the type of vector cube:

- `"segments"`, for vector cubes produced by
  [`sits_segment`](https://e-sensing.github.io/sits/reference/sits_segment.md).

- `"probs"`, for probability cubes produced by
  [`sits_classify.vector_cube`](https://e-sensing.github.io/sits/reference/sits_classify.vector_cube.md).

- `"entropy"` when using
  [`sits_uncertainty.probs_vector_cube`](https://e-sensing.github.io/sits/reference/sits_uncertainty.md).

- `"class"` for cubes produced by
  [`sits_label_classification`](https://e-sensing.github.io/sits/reference/sits_label_classification.md).

## Examples

``` r
if (sits_run_examples()) {
    # --- Create a cube based on a local MODIS data
    # MODIS local files have names such as
    # "TERRA_MODIS_012010_NDVI_2013-09-14.jp2"
    # see the parse info parameter as an example on how to
    # decode local files
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    modis_cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir,
        parse_info = c("satellite", "sensor", "tile", "band", "date")
    )
    # segment the vector cube
    segs_cube <- sits_segment(
        cube = modis_cube,
        seg_fn = sits_snic(
            grid_seeding = "rectangular",
            spacing = 15,
            compactness = 0.4,
            padding = 2
        ),
        output_dir = tempdir()
    )
    plot(segs_cube)

    # recover the local segmented cube
    local_segs_cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        raster_cube = modis_cube,
        vector_dir = tempdir(),
        vector_band = "segments"
    )
    # plot the recover model and compare
    plot(local_segs_cube)

    # classify the segments
    # create a random forest model
    rfor_model <- sits_train(samples_modis_ndvi, sits_rfor())
    probs_vector_cube <- sits_classify(
        data = segs_cube,
        ml_model = rfor_model,
        output_dir = tempdir(),
        n_sam_pol = 10
    )
    plot(probs_vector_cube)

    # recover vector cube
    local_probs_vector_cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        raster_cube = modis_cube,
        vector_dir = tempdir(),
        vector_band = "probs"
    )
    plot(local_probs_vector_cube)

    # label the segments
    class_vector_cube <- sits_label_classification(
        cube = probs_vector_cube,
        output_dir = tempdir(),
    )
    plot(class_vector_cube)

    # recover vector cube
    local_class_vector_cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        raster_cube = modis_cube,
        vector_dir = tempdir(),
        vector_band = "class"
    )
    plot(local_class_vector_cube)
}
```
