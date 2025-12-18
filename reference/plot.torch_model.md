# Plot Torch (deep learning) model

Plots a deep learning model developed using torch.

## Usage

``` r
# S3 method for class 'torch_model'
plot(x, y, ...)
```

## Arguments

- x:

  Object of class "torch_model".

- y:

  Ignored.

- ...:

  Further specifications for
  [plot](https://e-sensing.github.io/sits/reference/plot.md).

## Value

A plot object produced by the ggplot2 package showing the evolution of
the loss and accuracy of the model.

## Note

This code has been lifted from the "keras" package.

## Author

Felipe Carvalho, <lipecaso@gmail.com>

Rolf Simoes, <rolfsimoes@gmail.com>

Alber Sanchez, <alber.ipia@inpe.br>

## Examples

``` r
if (sits_run_examples()) {
    # Retrieve the samples for Mato Grosso
    # train a tempCNN model
    ml_model <- sits_train(samples_modis_ndvi, ml_method = sits_tempcnn)
    # plot the model
    plot(ml_model)
}
```
