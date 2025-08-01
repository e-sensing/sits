% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sits_predictors.R
\name{sits_pred_references}
\alias{sits_pred_references}
\title{Obtain categorical id and predictor labels for time series samples}
\usage{
sits_pred_references(pred)
}
\arguments{
\item{pred}{X-Y predictors: a data.frame with one row per sample.}
}
\value{
A character vector with labels associated to training samples.
}
\description{
Predictors are X-Y values required for machine learning
algorithms, organized as a data table where each row corresponds
to a training sample. The first two columns of the predictors table
are categorical ("label_id" and "label"). The other columns are
the values of each band and time, organized first by band and then by time.
This function returns the numeric values associated to each sample.
}
\examples{
if (sits_run_examples()) {
    pred <- sits_predictors(samples_modis_ndvi)
    ref <- sits_pred_references(pred)
}
}
\author{
Gilberto Camara, \email{gilberto.camara@inpe.br}
}
