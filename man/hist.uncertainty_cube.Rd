% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sits_histogram.R
\name{hist.uncertainty_cube}
\alias{hist.uncertainty_cube}
\title{Histogram uncertainty cubes}
\usage{
\method{hist}{uncertainty_cube}(x, ..., tile = x[["tile"]][[1L]], size = 100000L)
}
\arguments{
\item{x}{Object of class "variance_cube"}

\item{...}{Further specifications for \link{hist}.}

\item{tile}{Tile to be summarized}

\item{size}{Sample size}
}
\value{
A histogram of a uncertainty cube
}
\description{
This is a generic function. Parameters depend on the specific
type of input.
}
\examples{
if (sits_run_examples()) {
    # create a data cube from local files
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir
    )
    # create a random forest model
    rfor_model <- sits_train(samples_modis_ndvi, sits_rfor())
    # classify a data cube
    probs_cube <- sits_classify(
        data = cube, ml_model = rfor_model, output_dir = tempdir()
    )
    uncert_cube <- sits_uncertainty(
        cube = probs_cube,
        output_dir = tempdir()
    )
    hist(uncert_cube)
}
}
\author{
Gilberto Camara, \email{gilberto.camara@inpe.br}
}
