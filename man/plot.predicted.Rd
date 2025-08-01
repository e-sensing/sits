% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sits_plot.R
\name{plot.predicted}
\alias{plot.predicted}
\title{Plot time series predictions}
\usage{
\method{plot}{predicted}(x, y, ..., bands = "NDVI", palette = "Harmonic")
}
\arguments{
\item{x}{Object of class "predicted".}

\item{y}{Ignored.}

\item{...}{Further specifications for \link{plot}.}

\item{bands}{Bands for visualization.}

\item{palette}{HCL palette used for visualization
in case classes are not in the default sits palette.}
}
\value{
A plot object produced by ggplot2
                      showing the time series and its label.
}
\description{
Given a sits tibble with a set of predictions, plot them.
             Useful to show multi-year predictions for a time series.
}
\note{
This code is reused from the dtwSat package by Victor Maus.
}
\examples{
if (sits_run_examples()) {
    # Retrieve the samples for Mato Grosso
    # train an svm model
    ml_model <- sits_train(samples_modis_ndvi, ml_method = sits_svm)
    # classify the point
    point_ndvi <- sits_select(point_mt_6bands, bands = "NDVI")
    point_class <- sits_classify(
        data = point_ndvi, ml_model = ml_model
    )
    plot(point_class)
}
}
\author{
Victor Maus, \email{vwmaus1@gmail.com}

Gilberto Camara, \email{gilberto.camara@inpe.br}
}
