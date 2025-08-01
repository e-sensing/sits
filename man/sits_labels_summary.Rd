% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sits_labels.R
\name{sits_labels_summary}
\alias{sits_labels_summary}
\alias{sits_labels_summary.sits}
\title{Inform label distribution of a set of time series}
\usage{
sits_labels_summary(data)

\method{sits_labels_summary}{sits}(data)
}
\arguments{
\item{data}{Data.frame - Valid sits tibble}
}
\value{
A tibble with the frequency of each label.
}
\description{
Describes labels in a sits tibble
}
\examples{
# read a tibble with 400 samples of Cerrado and 346 samples of Pasture
data(cerrado_2classes)
# print the labels
sits_labels_summary(cerrado_2classes)
}
\author{
Rolf Simoes, \email{rolfsimoes@gmail.com}
}
