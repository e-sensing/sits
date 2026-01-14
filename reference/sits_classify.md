# Classify time series or data cubes

This function classifies a set of time series or data cube using a
trained model prediction model created by
[`sits_train`](https://e-sensing.github.io/sits/reference/sits_train.md).

The `sits_classify` function takes three types of data as input and
produce there types of output. Users should call `sits_classify` but be
aware that the parameters are different for each type of input.

- [`sits_classify.sits`](https://e-sensing.github.io/sits/reference/sits_classify.sits.md)
  is called when the input is a set of time series. The output is the
  same set with the additional column `predicted`.

- [`sits_classify.raster_cube`](https://e-sensing.github.io/sits/reference/sits_classify.raster_cube.md)
  is called when the input is a regular raster data cube. The output is
  a probability cube, which has the same tiles as the raster cube. Each
  tile contains a multiband image; each band contains the probability
  that each pixel belongs to a given class. Probability cubes are
  objects of class "probs_cube".

- [`sits_classify.vector_cube`](https://e-sensing.github.io/sits/reference/sits_classify.vector_cube.md)
  is called for vector data cubes. Vector data cubes are produced when
  closed regions are obtained from raster data cubes using
  [`sits_segment`](https://e-sensing.github.io/sits/reference/sits_segment.md).
  Classification of a vector data cube produces a vector data structure
  with additional columns expressing the class probabilities for each
  object. Probability cubes for vector data cubes are objects of class
  "probs_vector_cube".

## Usage

``` r
sits_classify(data, ml_model, ...)

# S3 method for class 'tbl_df'
sits_classify(data, ml_model, ...)

# S3 method for class 'derived_cube'
sits_classify(data, ml_model, ...)

# Default S3 method
sits_classify(data, ml_model, ...)
```

## Arguments

- data:

  Data cube (tibble of class "raster_cube")

- ml_model:

  R model trained by
  [`sits_train`](https://e-sensing.github.io/sits/reference/sits_train.md)

- ...:

  Other parameters for specific functions.

## Value

Time series with predicted labels for each point (tibble of class
"sits") or a data cube with probabilities for each class (tibble of
class "probs_cube").

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

6.  [`sits_train`](https://e-sensing.github.io/sits/reference/sits_train.md):
    train a machine learning model based on image time series.

7.  `sits_classify`: classify a data cube using a machine learning model
    and obtain a probability cube.

8.  [`sits_smooth`](https://e-sensing.github.io/sits/reference/sits_smooth.md):
    post-process a probability cube using a spatial smoother to remove
    outliers and increase spatial consistency.

9.  [`sits_label_classification`](https://e-sensing.github.io/sits/reference/sits_label_classification.md):
    produce a classified map by selecting the label with the highest
    probability from a smoothed cube.

SITS supports the following models:

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

Please refer to the sits documentation available in
<https://e-sensing.github.io/sitsbook/> for detailed examples.

## Author

Rolf Simoes, <rolfsimoes@gmail.com>

Gilberto Camara, <gilberto.camara@inpe.br>

Felipe Carvalho, <lipecaso@gmail.com>

Felipe Carlos, <efelipecarlos@gmail.com>
