# Summarize data cubes

This is a generic function. Parameters depend on the specific type of
input.

## Usage

``` r
# S3 method for class 'class_cube'
summary(object, ...)
```

## Arguments

- object:

  Object of class "class_cube"

- ...:

  Further specifications for
  [summary](https://rdrr.io/r/base/summary.html).

## Value

A summary of a classified cube

## Author

Gilberto Camara, <gilberto.camara@inpe.br>

## Examples

``` r
if (sits_run_examples()) {
    # create a data cube from local files
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir
    )
    # create a random forest model
    rfor_model <- sits_train(samples_modis_ndvi, sits_rfor())
    # classify a data cube
    probs_cube <- sits_classify(
        data = cube, ml_model = rfor_model, output_dir = tempdir()
    )
    # label the probability cube
    label_cube <- sits_label_classification(
        probs_cube,
        output_dir = tempdir()
    )
    summary(label_cube)
}
```
