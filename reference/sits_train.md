# Train classification models

Given a tibble with a set of time series, returns trained models.
Currently, sits supports the following models:

- support vector machines:
  [`sits_svm`](https://e-sensing.github.io/sits/reference/sits_svm.md);

- random forests:
  [`sits_rfor`](https://e-sensing.github.io/sits/reference/sits_rfor.md);

- extreme gradient boosting:
  [`sits_xgboost`](https://e-sensing.github.io/sits/reference/sits_xgboost.md);

- light gradient boosting:
  [`sits_lightgbm`](https://e-sensing.github.io/sits/reference/sits_lightgbm.md);

- multi-layer perceptrons:
  [`sits_mlp`](https://e-sensing.github.io/sits/reference/sits_mlp.md);

- temporal CNN:
  [`sits_tempcnn`](https://e-sensing.github.io/sits/reference/sits_tempcnn.md);

- residual network encoders:
  [`sits_resnet`](https://e-sensing.github.io/sits/reference/sits_resnet.md);

- LSTM with convolutional networks:
  [`sits_lstm_fcn`](https://e-sensing.github.io/sits/reference/sits_lstm_fcn.md);

- temporal self-attention encoders:
  [`sits_lighttae`](https://e-sensing.github.io/sits/reference/sits_lighttae.md)
  and
  [`sits_tae`](https://e-sensing.github.io/sits/reference/sits_tae.md).

## Usage

``` r
sits_train(samples, ml_method = sits_svm())
```

## Arguments

- samples:

  Time series with the training samples.

- ml_method:

  Machine learning method.

## Value

Model fitted to input data to be passed to
[`sits_classify`](https://e-sensing.github.io/sits/reference/sits_classify.md)

## Note

The main `sits` classification workflow has the following steps:

1.  [`sits_cube`](https://e-sensing.github.io/sits/reference/sits_cube.md):
    selects a ARD image collection from a cloud provider.

2.  [`sits_cube_copy`](https://e-sensing.github.io/sits/reference/sits_cube_copy.md):
    copies an ARD image collection from a cloud provider to a local
    directory for faster processing.

3.  [`sits_regularize`](https://e-sensing.github.io/sits/reference/sits_regularize.md):
    create a regular data cube from an ARD image collection.

4.  [`sits_apply`](https://e-sensing.github.io/sits/reference/sits_apply.md):
    create new indices by combining bands of a regular data cube
    (optional).

5.  [`sits_get_data`](https://e-sensing.github.io/sits/reference/sits_get_data.md):
    extract time series from a regular data cube based on user-provided
    labelled samples.

6.  `sits_train`: train a machine learning model based on image time
    series.

7.  [`sits_classify`](https://e-sensing.github.io/sits/reference/sits_classify.md):
    classify a data cube using a machine learning model and obtain a
    probability cube.

8.  [`sits_smooth`](https://e-sensing.github.io/sits/reference/sits_smooth.md):
    post-process a probability cube using a spatial smoother to remove
    outliers and increase spatial consistency.

9.  [`sits_label_classification`](https://e-sensing.github.io/sits/reference/sits_label_classification.md):
    produce a classified map by selecting the label with the highest
    probability from a smoothed cube.

`sits_train` provides a standard interface to machine learning models.
It takes two mandatory parameters: the training data (`samples`) and the
ML algorithm (`ml_method`). The output is a model that can be used to
classify individual time series or data cubes with
[`sits_classify`](https://e-sensing.github.io/sits/reference/sits_classify.md).

`sits` provides a set of default values for all classification models.
These settings have been chosen based on testing by the authors.
Nevertheless, users can control all parameters for each model. Novice
users can rely on the default values, while experienced ones can
fine-tune deep learning models using
[`sits_tuning`](https://e-sensing.github.io/sits/reference/sits_tuning.md).

## Author

Rolf Simoes, <rolfsimoes@gmail.com>

Gilberto Camara, <gilberto.camara@inpe.br>

Alexandre Ywata de Carvalho, <alexandre.ywata@ipea.gov.br>

## Examples

``` r
if (sits_run_examples()) {
    # Retrieve the set of samples for Mato Grosso
    # fit a training model (rfor model)
    ml_model <- sits_train(samples_modis_ndvi, sits_rfor(num_trees = 50))
    # get a point and classify the point with the ml_model
    point_ndvi <- sits_select(point_mt_6bands, bands = "NDVI")
    class <- sits_classify(
        data = point_ndvi, ml_model = ml_model
    )
}
```
