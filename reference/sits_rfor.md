# Train random forest models

Use Random Forest algorithm to classify samples. This function is a
front-end to the `randomForest` package. Please refer to the
documentation in that package for more details.

## Usage

``` r
sits_rfor(samples = NULL, num_trees = 100L, mtry = NULL, ...)
```

## Arguments

- samples:

  Time series with the training samples (tibble of class "sits").

- num_trees:

  Number of trees to grow. This should not be set to too small a number,
  to ensure that every input row gets predicted at least a few times
  (default: 100) (integer, min = 50, max = 150).

- mtry:

  Number of variables randomly sampled as candidates at each split
  (default: NULL - use default value of
  [`randomForest::randomForest()`](https://rdrr.io/pkg/randomForest/man/randomForest.html)
  function, i.e. `floor(sqrt(features))`).

- ...:

  Other parameters to be passed to \`randomForest::randomForest\`
  function.

## Value

Model fitted to input data (to be passed to
[`sits_classify`](https://e-sensing.github.io/sits/reference/sits_classify.md)).

## Author

Alexandre Ywata de Carvalho, <alexandre.ywata@ipea.gov.br>

Rolf Simoes, <rolfsimoes@gmail.com>

Gilberto Camara, <gilberto.camara@inpe.br>

## Examples

``` r
if (sits_run_examples()) {
    # Example of training a model for time series classification
    # Retrieve the samples for Mato Grosso
    # train a random forest model
    rf_model <- sits_train(samples_modis_ndvi,
        ml_method = sits_rfor
    )
    # classify the point
    point_ndvi <- sits_select(point_mt_6bands, bands = "NDVI")
    # classify the point
    point_class <- sits_classify(
        data = point_ndvi, ml_model = rf_model
    )
    plot(point_class)
}
```
