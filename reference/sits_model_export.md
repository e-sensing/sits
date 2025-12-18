# Export classification models

Given a trained machine learning or deep learning model, exports the
model as an object for further exploration outside the `sits` package.

## Usage

``` r
sits_model_export(ml_model)

# S3 method for class 'sits_model'
sits_model_export(ml_model)
```

## Arguments

- ml_model:

  A trained machine learning model

## Value

An R object containing the model in the original format of machine
learning or deep learning package.

## Author

Rolf Simoes, <rolfsimoes@gmail.com>

## Examples

``` r
if (sits_run_examples()) {
    # create a classification model
    rfor_model <- sits_train(samples_modis_ndvi, sits_rfor())
    # export the model
    rfor_object <- sits_model_export(rfor_model)
}
```
