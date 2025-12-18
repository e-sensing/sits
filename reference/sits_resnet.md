# Train ResNet classification models

Use a ResNet architecture for classifying image time series. The ResNet
(or deep residual network) was proposed by a team in Microsoft Research
for 2D image classification. ResNet tries to address the degradation of
accuracy in a deep network. The idea is to replace a deep network with a
combination of shallow ones. In the paper by Fawaz et al. (2019), ResNet
was considered the best method for time series classification, using the
UCR dataset. Please refer to the paper for more details.

The R-torch version is based on the code made available by Zhiguang
Wang, author of the original paper. The code was developed in python
using keras.

<https://github.com/cauchyturing> (repo:
UCR_Time_Series_Classification_Deep_Learning_Baseline)

The R-torch version also considered the code by Ignacio Oguiza, whose
implementation is available at
<https://github.com/timeseriesAI/tsai/blob/main/tsai/models/ResNet.py>.

There are differences between Wang's Keras code and Oguiza torch code.
In this case, we have used Wang's keras code as the main reference.

## Usage

``` r
sits_resnet(
  samples = NULL,
  samples_validation = NULL,
  blocks = c(64, 128, 128),
  kernels = c(7, 5, 3),
  epochs = 100,
  batch_size = 64,
  validation_split = 0.2,
  optimizer = torch::optim_adamw,
  opt_hparams = list(lr = 0.001, eps = 1e-08, weight_decay = 1e-06),
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

  Time series with the validation samples. If the parameter is provided,
  the `validation_split` is ignored.

- blocks:

  Number of 1D convolutional filters for each block of three layers.

- kernels:

  Size of the 1D convolutional kernels

- epochs:

  Number of iterations to train the model. for each layer of each block.

- batch_size:

  Number of samples per gradient update.

- validation_split:

  Fraction of training data to be used as validation data.

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

Hassan Fawaz, Germain Forestier, Jonathan Weber, Lhassane Idoumghar, and
Pierre-Alain Muller, "Deep learning for time series classification: a
review", Data Mining and Knowledge Discovery, 33(4): 917–963, 2019.

Zhiguang Wang, Weizhong Yan, and Tim Oates, "Time series classification
from scratch with deep neural networks: A strong baseline", 2017
International Joint conference on Neural Networks (IJCNN).

## Author

Gilberto Camara, <gilberto.camara@inpe.br>

Rolf Simoes, <rolf.simoes@inpe.br>

Felipe Souza, <lipecaso@gmail.com>

Felipe Carlos, <efelipecarlos@gmail.com>

Charlotte Pelletier, <charlotte.pelletier@univ-ubs.fr>

Daniel Falbel, <dfalbel@gmail.com>

## Examples

``` r
if (sits_run_examples()) {
    # create a ResNet model
    torch_model <- sits_train(samples_modis_ndvi, sits_resnet())
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
        bayes_cube, output_dir = tempdir()
    )
    # plot the labelled cube
    plot(label_cube)
}
```
