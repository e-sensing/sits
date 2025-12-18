# Train light gradient boosting model

Use LightGBM algorithm to classify samples. This function is a front-end
to the `lightgbm` package. LightGBM (short for Light Gradient Boosting
Machine) is a gradient boosting framework developed by Microsoft that's
designed for fast, scalable, and efficient training of decision
tree-based models. It is widely used in machine learning for
classification, regression, ranking, and other tasks, especially with
large-scale data.

## Usage

``` r
sits_lightgbm(
  samples = NULL,
  boosting_type = "gbdt",
  objective = "multiclass",
  min_samples_leaf = 20,
  max_depth = 6,
  learning_rate = 0.1,
  num_iterations = 100,
  n_iter_no_change = 10,
  validation_split = 0.2,
  ...
)
```

## Arguments

- samples:

  Time series with the training samples.

- boosting_type:

  Type of boosting algorithm (default = "gbdt")

- objective:

  Aim of the classifier (default = "multiclass").

- min_samples_leaf:

  Minimal number of data in one leaf. Can be used to deal with
  over-fitting.

- max_depth:

  Limit the max depth for tree model.

- learning_rate:

  Shrinkage rate for leaf-based algorithm.

- num_iterations:

  Number of iterations to train the model.

- n_iter_no_change:

  Number of iterations without improvements until training stops.

- validation_split:

  Fraction of the training data for validation. The model will set apart
  this fraction and will evaluate the loss and any model metrics on this
  data at the end of each epoch.

- ...:

  Other parameters to be passed to \`lightgbm::lightgbm\` function.

## Value

Model fitted to input data (to be passed to
[`sits_classify`](https://e-sensing.github.io/sits/reference/sits_classify.md)).

## Author

Gilberto Camara, <gilberto.camara@inpe.br>

## Examples

``` r
if (sits_run_examples()) {
    # Example of training a model for time series classification
    # Retrieve the samples for Mato Grosso
    # train a random forest model
    lgb_model <- sits_train(samples_modis_ndvi,
        ml_method = sits_lightgbm
    )
    # classify the point
    point_ndvi <- sits_select(point_mt_6bands, bands = "NDVI")
    # classify the point
    point_class <- sits_classify(
        data = point_ndvi, ml_model = lgb_model
    )
    plot(point_class)
}
```
