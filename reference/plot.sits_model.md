# Message for models whose plots are not available

Plots trees in an extreme gradient boosting model.

## Usage

``` r
# S3 method for class 'sits_model'
plot(x, ...)
```

## Arguments

- x:

  Object of class "sits_model".

- ...:

  Further specifications for
  [plot](https://e-sensing.github.io/sits/reference/plot.md).

## Value

Called for side effects

## Author

Gilberto Camara, <gilberto.camara@inpe.br>

## Examples

``` r
if (sits_run_examples()) {
    # Retrieve the samples for Mato Grosso
    # train an extreme gradient boosting
    svm_model <- sits_train(samples_modis_ndvi,
        ml_method = sits_svm()
    )
    plot(svm_model)
}
```
