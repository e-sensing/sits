# Define a linear formula for classification models

Provides a symbolic description of a fitting model. Tells the model to
do a linear transformation of the input values. The \`predictors_index\`
parameter informs the positions of fields corresponding to formula
independent variables. If no value is given, that all fields will be
used as predictors.

## Usage

``` r
sits_formula_linear(predictors_index = -2L:0L)
```

## Arguments

- predictors_index:

  Index of the valid columns whose names are used to compose formula
  (default: -2:0).

## Value

A function that computes a valid formula using a linear function.

## Author

Gilberto Camara, <gilberto.camara@inpe.br>

Alexandre Ywata de Carvalho, <alexandre.ywata@ipea.gov.br>

Rolf Simoes, <rolfsimoes@gmail.com>

## Examples

``` r
if (sits_run_examples()) {
    # Example of training a model for time series classification
    # Retrieve the samples for Mato Grosso
    # train an SVM model
    ml_model <- sits_train(samples_modis_ndvi,
        ml_method = sits_svm(formula = sits_formula_logref())
    )
    # classify the point
    point_ndvi <- sits_select(point_mt_6bands, bands = "NDVI")
    # classify the point
    point_class <- sits_classify(
        data = point_ndvi, ml_model = ml_model
    )
    plot(point_class)
}
```
