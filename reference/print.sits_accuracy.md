# Print the values of a confusion matrix

Adaptation of the caret::print.confusionMatrix method for the more
common usage in Earth Observation.

## Usage

``` r
# S3 method for class 'sits_accuracy'
print(x, ..., digits = NULL)
```

## Arguments

- x:

  Object of class `confusionMatrix`.

- ...:

  Other parameters passed to the "print" function.

- digits:

  Number of significant digits when printed.

## Value

Called for side effects.

## Author

Gilberto Camara, <gilberto.camara@inpe.br>
