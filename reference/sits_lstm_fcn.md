# Train a Long Short Term Memory Fully Convolutional Network

Uses a branched neural network consisting of a lstm (long short term
memory) branch and a three-layer fully convolutional branch (FCN)
followed by concatenation to classify time series data.

This function is based on the paper by Fazle Karim, Somshubra Majumdar,
and Houshang Darabi. If you use this method, please cite the original
LSTM with FCN paper.

The torch version is based on the code made available by the titu1994.
The original python code is available at the website
<https://github.com/titu1994/LSTM-FCN>. This code is licensed as GPL-3.

## Usage

``` r
sits_lstm_fcn(
  samples = NULL,
  samples_validation = NULL,
  cnn_layers = c(128, 256, 128),
  cnn_kernels = c(8, 5, 3),
  lstm_width = 8,
  lstm_dropout = 0.8,
  epochs = 50,
  batch_size = 64,
  validation_split = 0.2,
  optimizer = torch::optim_adamw,
  opt_hparams = list(lr = 5e-04, eps = 1e-08, weight_decay = 1e-06),
  lr_decay_epochs = 1,
  lr_decay_rate = 0.95,
  patience = 20,
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

- lstm_width:

  Number of neuros in the lstm's hidden layer.

- lstm_dropout:

  Dropout rate of the lstm layer.

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

## References

F. Karim, S. Majumdar, H. Darabi and S. Chen, "LSTM Fully Convolutional
Networks for Time Series Classification," in IEEE Access, vol. 6, pp.
1662-1669, 2018,
[doi:10.1109/ACCESS.2017.2779939](https://doi.org/10.1109/ACCESS.2017.2779939)
.

## Author

Alexandre Assuncao, <alexcarssuncao@gmail.com>
