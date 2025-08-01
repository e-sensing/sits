% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sits_classify.R
\name{sits_classify.sits}
\alias{sits_classify.sits}
\title{Classify a set of time series}
\usage{
\method{sits_classify}{sits}(
  data,
  ml_model,
  ...,
  filter_fn = NULL,
  impute_fn = impute_linear(),
  multicores = 2L,
  gpu_memory = 4L,
  batch_size = 2L^gpu_memory,
  progress = TRUE
)
}
\arguments{
\item{data}{Set of time series ("sits tibble")}

\item{ml_model}{R model trained by \code{\link[sits]{sits_train}}
(closure of class "sits_model")}

\item{...}{Other parameters for specific functions.}

\item{filter_fn}{Smoothing filter to be applied - optional
(closure containing object of class "function").}

\item{impute_fn}{Imputation function to remove NA.}

\item{multicores}{Number of cores to be used for classification
(integer, min = 1, max = 2048).}

\item{gpu_memory}{Memory available in GPU in GB (default = 4)}

\item{batch_size}{Batch size for GPU classification.}

\item{progress}{Logical: Show progress bar?}
}
\value{
Time series with predicted labels for
                          each point (tibble of class "sits").
}
\description{
\code{\link[sits]{sits_classify.sits}} is called when the input is
   a set of time series. The output is the same set
   with the additional column \code{predicted}.
}
\note{
Parameter \code{filter_fn} specifies a smoothing filter
   to be applied to each time series for reducing noise. Currently, options
   are Savitzky-Golay (see \code{\link[sits]{sits_sgolay}}) and Whittaker
   (see \code{\link[sits]{sits_whittaker}}) filters. Note that this
   parameter should also have been applied to the training set to obtain
   the model.

   Parameter \code{impute_fn} defines a 1D function that will be used
   to interpolate NA values in each time series. Currently sits supports
   the \code{\link{impute_linear}} function, but users can define
   imputation functions which are defined externally.

   Parameter \code{multicores}  defines the number of cores
   used for processing. We recommend using as much memory as possible.

   When using a GPU for deep learning, \code{gpu_memory} indicates the
   memory of the graphics card which is available for processing.
   The parameter \code{batch_size} defines the size of the matrix
   (measured in number of rows) which is sent to the GPU for classification.
   Users can test different values of \code{batch_size} to
   find out which one best fits their GPU architecture.

   It is not possible to have an exact idea of the size of Deep Learning
   models in GPU memory, as the complexity of the model and factors
   such as CUDA Context increase the size of the model in memory.
   Therefore, we recommend that you leave at least 1GB free on the
   video card to store the Deep Learning model that will be used.

   For users of Apple M3 chips or similar with a Neural Engine, be
   aware that these chips share memory between the GPU and the CPU.
   Tests indicate that the \code{memsize}
   should be set to half to the total memory and the \code{batch_size}
   parameter should be a small number (we suggest the value of 64).
   Be aware that increasing these parameters may lead to memory
   conflicts.
}
\examples{
if (sits_run_examples()) {
    # Example of classification of a time series
    # Retrieve the samples for Mato Grosso
    # train a random forest model
    rf_model <- sits_train(samples_modis_ndvi, ml_method = sits_rfor)

    # classify the point
    point_ndvi <- sits_select(point_mt_6bands, bands = c("NDVI"))
    point_class <- sits_classify(
        data = point_ndvi, ml_model = rf_model
    )
    plot(point_class)
}
}
