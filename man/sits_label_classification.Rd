% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sits_label_classification.R
\name{sits_label_classification}
\alias{sits_label_classification}
\title{Post-process a classified data raster probs to obtain a labelled image}
\usage{
sits_label_classification(
  cube,
  multicores = 1,
  memsize = 1,
  output_dir = tempdir(),
  version = "v1"
)
}
\arguments{
\item{cube}{Classified image data cube.}

\item{multicores}{Number of process to label the classification in
snow subprocess.}

\item{memsize}{Maximum overall memory (in GB) to label the
classification.}

\item{output_dir}{Output directory where to out the file}

\item{version}{Version of resulting image
(in the case of multiple tests)}
}
\value{
A data cube
}
\description{
Takes a set of classified raster layers with probabilities,
             and label them based on the maximum probability for each pixel.
}
\examples{
\dontrun{
# Retrieve the samples for Mato Grosso
# select band "ndvi"
samples_ndvi <- sits_select(samples_modis_4bands, bands = "NDVI")

# select a random forest model
rfor_model <- sits_train(samples_ndvi, sits_rfor(num_trees = 500))

# create a data cube based on the information about the files
data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
cube <- sits_cube(
    source = "LOCAL",
    name = "sinop_2014",
    satellite = "TERRA",
    sensor = "MODIS",
    data_dir = data_dir,
    delim = "_",
    parse_info = c("X1", "X2", "tile", "band", "date")
)

# classify the raster image
probs_cube <- sits_classify(cube,
    ml_model = rfor_model,
    output_dir = tempdir(),
    memsize = 4, multicores = 2
)

# label the classification
label_cube <- sits_label_classification(probs_cube, output_dir = tempdir())
}

}
\author{
Rolf Simoes, \email{rolf.simoes@inpe.br}
}
