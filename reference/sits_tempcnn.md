# Train temporal convolutional neural network models

Use a TempCNN algorithm to classify data, which has two stages: a 1D CNN
and a multi-layer perceptron. Users can define the depth of the 1D
network, as well as the number of perceptron layers.

## Usage

``` r
sits_tempcnn(
  samples = NULL,
  samples_validation = NULL,
  cnn_layers = c(64L, 64L, 64L),
  cnn_kernels = c(3L, 3L, 3L),
  cnn_dropout_rates = c(0.2, 0.2, 0.2),
  dense_layer_nodes = 256L,
  dense_layer_dropout_rate = 0.5,
  epochs = 150L,
  batch_size = 64L,
  validation_split = 0.2,
  optimizer = torch::optim_adamw,
  opt_hparams = list(lr = 5e-04, eps = 1e-08, weight_decay = 1e-06),
  lr_decay_epochs = 1L,
  lr_decay_rate = 0.95,
  patience = 20L,
  min_delta = 0.01,
  seed = NULL,
  verbose = FALSE
)
```

## Arguments

- samples:

  Time series with the training samples.

- samples_validation:

  Time series with the validation samples. if the `samples_validation`
  parameter is provided, the `validation_split` parameter is ignored.

- cnn_layers:

  Number of 1D convolutional filters per layer

- cnn_kernels:

  Size of the 1D convolutional kernels.

- cnn_dropout_rates:

  Dropout rates for 1D convolutional filters.

- dense_layer_nodes:

  Number of nodes in the dense layer.

- dense_layer_dropout_rate:

  Dropout rate (0,1) for the dense layer.

- epochs:

  Number of iterations to train the model.

- batch_size:

  Number of samples per gradient update.

- validation_split:

  Fraction of training data to be used for validation.

- optimizer:

  Optimizer function to be used.

- opt_hparams:

  Hyperparameters for optimizer: lr : Learning rate of the optimizer
  eps: Term added to the denominator to improve numerical stability.
  weight_decay: L2 regularization

- lr_decay_epochs:

  Number of epochs to reduce learning rate.

- lr_decay_rate:

  Decay factor for reducing learning rate.

- patience:

  Number of epochs without improvements until training stops.

- min_delta:

  Minimum improvement in loss function to reset the patience counter.

- seed:

  Seed for random values.

- verbose:

  Verbosity mode (TRUE/FALSE). Default is FALSE.

## Value

A fitted model to be used for classification.

## Note

`sits` provides a set of default values for all classification models.
These settings have been chosen based on testing by the authors.
Nevertheless, users can control all parameters for each model. Novice
users can rely on the default values, while experienced ones can
fine-tune deep learning models using
[`sits_tuning`](https://e-sensing.github.io/sits/reference/sits_tuning.md).

This function is based on the paper by Charlotte Pelletier referenced
below. If you use this method, please cite the original tempCNN paper.

The torch version is based on the code made available by the BreizhCrops
team: Marc Russwurm, Charlotte Pelletier, Marco Korner, Maximilian
Zollner. The original python code is available at the website
<https://github.com/dl4sits/BreizhCrops>. This code is licensed as
GPL-3.

## References

Charlotte Pelletier, Geoffrey Webb and François Petitjean, "Temporal
Convolutional Neural Network for the Classification of Satellite Image
Time Series", Remote Sensing, 11,523, 2019.
[doi:10.3390/rs11050523](https://doi.org/10.3390/rs11050523) .

## Author

Charlotte Pelletier, <charlotte.pelletier@univ-ubs.fr>

Gilberto Camara, <gilberto.camara@inpe.br>

Rolf Simoes, <rolfsimoes@gmail.com>

Felipe Souza, <lipecaso@gmail.com>

## Examples

``` r
if (sits_run_examples()) {
    # create a TempCNN model
    torch_model <- sits_train(
        samples_modis_ndvi,
        sits_tempcnn(epochs = 20, verbose = TRUE)
    )
    # plot the model
    plot(torch_model)
    # create a data cube from local files
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir
    )
    # classify a data cube
    probs_cube <- sits_classify(
        data = cube, ml_model = torch_model, output_dir = tempdir()
    )
    # plot the probability cube
    plot(probs_cube)
    # smooth the probability cube using Bayesian statistics
    bayes_cube <- sits_smooth(probs_cube, output_dir = tempdir())
    # plot the smoothed cube
    plot(bayes_cube)
    # label the probability cube
    label_cube <- sits_label_classification(
        bayes_cube,
        output_dir = tempdir()
    )
    # plot the labelled cube
    plot(label_cube)
}
```
