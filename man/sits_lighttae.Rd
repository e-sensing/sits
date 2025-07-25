% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sits_lighttae.R
\name{sits_lighttae}
\alias{sits_lighttae}
\title{Train a model using Lightweight Temporal Self-Attention Encoder}
\usage{
sits_lighttae(
  samples = NULL,
  samples_validation = NULL,
  epochs = 150L,
  batch_size = 128L,
  validation_split = 0.2,
  optimizer = torch::optim_adamw,
  opt_hparams = list(lr = 5e-04, eps = 1e-08, weight_decay = 7e-04),
  lr_decay_epochs = 50L,
  lr_decay_rate = 1,
  patience = 20L,
  min_delta = 0.01,
  verbose = FALSE
)
}
\arguments{
\item{samples}{Time series with the training samples
(tibble of class "sits").}

\item{samples_validation}{Time series with the validation samples
(tibble of class "sits").
If \code{samples_validation} parameter is provided,
\code{validation_split} is ignored.}

\item{epochs}{Number of iterations to train the model
(integer, min = 1, max = 20000).}

\item{batch_size}{Number of samples per gradient update
(integer, min = 16L, max = 2048L)}

\item{validation_split}{Fraction of training data
to be used as validation data.}

\item{optimizer}{Optimizer function to be used.}

\item{opt_hparams}{Hyperparameters for optimizer:
\code{lr} : Learning rate of the optimizer
\code{eps}: Term added to the denominator
     to improve numerical stability.
\code{weight_decay}:       L2 regularization rate.}

\item{lr_decay_epochs}{Number of epochs to reduce learning rate.}

\item{lr_decay_rate}{Decay factor for reducing learning rate.}

\item{patience}{Number of epochs without improvements until
training stops.}

\item{min_delta}{Minimum improvement in loss function
to reset the patience counter.}

\item{verbose}{Verbosity mode (TRUE/FALSE). Default is FALSE.}
}
\value{
A fitted model to be used for classification of data cubes.
}
\description{
Implementation of Light Temporal Attention Encoder (L-TAE)
for satellite image time series
}
\note{
\code{sits} provides a set of default values for all classification models.
These settings have been chosen based on testing by the authors.
Nevertheless, users can control all parameters for each model.
Novice users can rely on the default values,
while experienced ones can fine-tune deep learning models
using \code{\link[sits]{sits_tuning}}.

This function is based on the paper by Vivien Garnot referenced below
and code available on github at
https://github.com/VSainteuf/lightweight-temporal-attention-pytorch
If you use this method, please cite the original TAE and the LTAE paper.

We also used the code made available by Maja Schneider in her work with
Marco Körner referenced below and available at
https://github.com/maja601/RC2020-psetae.
}
\examples{
if (sits_run_examples()) {
    # create a lightTAE model
    torch_model <- sits_train(samples_modis_ndvi, sits_lighttae())
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
}
\references{
Vivien Garnot, Loic Landrieu, Sebastien Giordano, and Nesrine Chehata,
"Satellite Image Time Series Classification with Pixel-Set Encoders
and Temporal Self-Attention",
2020 Conference on Computer Vision and Pattern Recognition.
pages 12322-12331.
DOI: 10.1109/CVPR42600.2020.01234

Vivien Garnot, Loic Landrieu,
"Lightweight Temporal Self-Attention  for Classifying
Satellite Images Time Series",
arXiv preprint arXiv:2007.00586, 2020.

Schneider, Maja; Körner, Marco,
"[Re] Satellite Image Time Series Classification
with Pixel-Set Encoders and Temporal Self-Attention."
ReScience C 7 (2), 2021.
DOI: 10.5281/zenodo.4835356
}
\author{
Gilberto Camara, \email{gilberto.camara@inpe.br}

Rolf Simoes, \email{rolfsimoes@gmail.com}

Charlotte Pelletier, \email{charlotte.pelletier@univ-ubs.fr}
}
