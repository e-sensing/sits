# Train extreme gradient boosting models

This function uses the extreme gradient boosting algorithm. Boosting
iteratively adds basis functions in a greedy fashion so that each new
basis function further reduces the selected loss function. This function
is a front-end to the methods in the "xgboost" package. Please refer to
the documentation in that package for more details.

## Usage

``` r
sits_xgboost(
  samples = NULL,
  learning_rate = 0.15,
  min_split_loss = 1,
  max_depth = 5L,
  min_child_weight = 1,
  max_delta_step = 1,
  subsample = 0.85,
  nfold = 5L,
  nrounds = 100L,
  nthread = 6L,
  early_stopping_rounds = 20L,
  verbose = FALSE
)
```

## Arguments

- samples:

  Time series with the training samples.

- learning_rate:

  Learning rate: scale the contribution of each tree by a factor of 0 \<
  lr \< 1 when it is added to the current approximation. Used to prevent
  overfitting. Default: 0.15

- min_split_loss:

  Minimum loss reduction to make a further partition of a leaf. Default:
  1.

- max_depth:

  Maximum depth of a tree. Increasing this value makes the model more
  complex and more likely to overfit. Default: 5.

- min_child_weight:

  If the leaf node has a minimum sum of instance weights lower than
  min_child_weight, tree splitting stops. The larger min_child_weight
  is, the more conservative the algorithm is. Default: 1.

- max_delta_step:

  Maximum delta step we allow each leaf output to be. If the value is
  set to 0, there is no constraint. If it is set to a positive value, it
  can help making the update step more conservative. Default: 1.

- subsample:

  Percentage of samples supplied to a tree. Default: 0.8.

- nfold:

  Number of the subsamples for the cross-validation.

- nrounds:

  Number of rounds to iterate the cross-validation (default: 100)

- nthread:

  Number of threads (default = 6)

- early_stopping_rounds:

  Training with a validation set will stop if the performance doesn't
  improve for k rounds.

- verbose:

  Print information on statistics during the process

## Value

Model fitted to input data (to be passed to
[`sits_classify`](https://e-sensing.github.io/sits/reference/sits_classify.md))

## Note

Please refer to the sits documentation available in
<https://e-sensing.github.io/sitsbook/> for detailed examples.

## References

Tianqi Chen, Carlos Guestrin, "XGBoost : Reliable Large-scale Tree
Boosting System", SIG KDD 2016.

## Author

Gilberto Camara, <gilberto.camara@inpe.br>

## Examples

``` r
if (sits_run_examples()) {
    # Example of training a model for time series classification
    # Retrieve the samples for Mato Grosso
    # train a xgboost model
    ml_model <- sits_train(samples_modis_ndvi, ml_method = sits_xgboost)
    # classify the point
    point_ndvi <- sits_select(point_mt_6bands, bands = "NDVI")
    # classify the point
    point_class <- sits_classify(
        data = point_ndvi, ml_model = ml_model
    )
    plot(point_class)
}
```
