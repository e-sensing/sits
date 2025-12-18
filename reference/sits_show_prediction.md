# Shows the predicted labels for a classified tibble

This function takes a tibble with a classified time series by a machine
learning method and displays the result.

## Usage

``` r
sits_show_prediction(class)
```

## Arguments

- class:

  A SITS tibble that has been classified.

## Value

Tibble with the columns "from", "to", "class"

## Author

Gilberto Camara, <gilberto.camara@inpe.br>

## Examples

``` r
if (sits_run_examples()) {
    # Retrieve the samples for Mato Grosso
    # train an SVM model
    ml_model <- sits_train(samples_modis_ndvi, ml_method = sits_svm)
    # classify the point
    point_ndvi <- sits_select(point_mt_6bands, bands = "NDVI")
    point_class <- sits_classify(
        data = point_ndvi, ml_model = ml_model
    )
    sits_show_prediction(point_class)
}
```
