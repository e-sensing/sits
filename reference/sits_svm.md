# Train support vector machine models

This function receives a tibble with a set of attributes X for each
observation Y. These attributes are the values of the time series for
each band. The SVM algorithm is used for multiclass-classification. For
this purpose, it uses the "one-against-one" approach, in which k(k-1)/2
binary classifiers are trained; the appropriate class is found by a
voting scheme. This function is a front-end to the "svm" method in the
"e1071" package. Please refer to the documentation in that package for
more details.

## Usage

``` r
sits_svm(
  samples = NULL,
  formula = sits_formula_linear(),
  scale = FALSE,
  cachesize = 1000L,
  kernel = "radial",
  degree = 3L,
  coef0 = 0L,
  cost = 10,
  tolerance = 0.001,
  epsilon = 0.1,
  cross = 10L,
  ...
)
```

## Arguments

- samples:

  Time series with the training samples.

- formula:

  Symbolic description of the model to be fit. (default:
  sits_formula_linear).

- scale:

  Logical vector indicating the variables to be scaled.

- cachesize:

  Cache memory in MB (default = 1000).

- kernel:

  Kernel used in training and predicting. options: "linear",
  "polynomial", "radial", "sigmoid" (default: "radial").

- degree:

  Exponential of polynomial type kernel (default: 3).

- coef0:

  Parameter needed for kernels of type polynomial and sigmoid (default:
  0).

- cost:

  Cost of constraints violation (default: 10).

- tolerance:

  Tolerance of termination criterion (default: 0.001).

- epsilon:

  Epsilon in the insensitive-loss function (default: 0.1).

- cross:

  Number of cross validation folds applied to assess the quality of the
  model (default: 10).

- ...:

  Other parameters to be passed to e1071::svm function.

## Value

Model fitted to input data (to be passed to
[`sits_classify`](https://e-sensing.github.io/sits/reference/sits_classify.md))

## Note

Please refer to the sits documentation available in
<https://e-sensing.github.io/sitsbook/> for detailed examples.

## Author

Alexandre Ywata de Carvalho, <alexandre.ywata@ipea.gov.br>

Rolf Simoes, <rolfsimoes@gmail.com>

Gilberto Camara, <gilberto.camara@inpe.br>

## Examples

``` r
if (sits_run_examples()) {
    # Example of training a model for time series classification
    # Retrieve the samples for Mato Grosso
    # train an SVM model
    ml_model <- sits_train(samples_modis_ndvi, ml_method = sits_svm)
    # classify the point
    point_ndvi <- sits_select(point_mt_6bands, bands = "NDVI")
    # classify the point
    point_class <- sits_classify(
        data = point_ndvi, ml_model = ml_model
    )
    plot(point_class)
}
```
