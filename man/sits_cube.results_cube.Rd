% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sits_cube_local.R
\name{sits_cube.results_cube}
\alias{sits_cube.results_cube}
\title{Create a results cube from local files}
\usage{
\method{sits_cube}{results_cube}(
  source,
  collection,
  ...,
  data_dir,
  tiles = NULL,
  bands,
  labels = NULL,
  parse_info = c("X1", "X2", "tile", "start_date", "end_date", "band", "version"),
  version = "v1",
  delim = "_",
  multicores = 2L,
  progress = TRUE
)
}
\arguments{
\item{source}{Data source: one of \code{"AWS"}, \code{"BDC"},
\code{"CDSE"}, \code{"DEAFRICA"}, \code{"DEAUSTRALIA"},
\code{"HLS"}, \code{"PLANETSCOPE"}, \code{"MPC"},
\code{"SDC"} or \code{"USGS"}. This is the source
from which the original data has been downloaded.}

\item{collection}{Image collection in data source from which
the original data has been downloaded.
To find out the supported collections,
use \code{\link{sits_list_collections}()}).}

\item{...}{Other parameters to be passed for specific types.}

\item{data_dir}{Local directory where images are stored}

\item{tiles}{Tiles from the collection to be included in
the cube.}

\item{bands}{Results bands to be retrieved
("probs", "bayes", "variance", "class", "uncertainty")}

\item{labels}{Named vector with labels associated to the classes}

\item{parse_info}{Parsing information for local files
(see notes below).}

\item{version}{Version of the classified and/or labelled files.}

\item{delim}{Delimiter for parsing local results cubes
(default = "_")}

\item{multicores}{Number of workers for parallel processing
(integer, min = 1, max = 2048).}

\item{progress}{Logical: show a progress bar?}
}
\value{
A \code{tibble} describing the contents of a data cube.
}
\description{
Creates a data cube from local files produced by sits operations
that produces results (such as probs_cubs and class_cubes)
}
\note{
This function creates result cubes from local files produced by
classification or post-classification algorithms. In this case, the
\code{parse_info} is specified differently, and additional parameters
are required.
The parameter \code{bands} should be a single character vector with
the name associated to the type of result:
\itemize{
\item{\code{"probs"}, for probability cubes produced by
  \code{\link[sits]{sits_classify}}.}
\item{\code{"bayes"}, for smoothed cubes produced by
   \code{\link[sits]{sits_smooth}}.}
\item{\code{"entropy"} when using \code{\link[sits]{sits_uncertainty}} to
  measure entropy in pixel classification.}
\item{\code{"margin"} when using \code{\link[sits]{sits_uncertainty}} to
 measure  probability margin in pixel classification.}
\item{\code{"least"} when using \code{\link[sits]{sits_uncertainty}} to
 measure difference between 100\% and
 most probable class in pixel classification.}
\item{\code{"class"} for cubes produced by
         \code{\link[sits]{sits_label_classification}}.}
}
For cubes of type \code{"probs"}, \code{"bayes"}, \code{"class"}, the
    \code{labels} parameter should be named vector associated to the
    classification results. For \code{"class"} cubes, its names should be
    integers associated to the values of the raster files that represent
    the classified cube.

Parameter \code{parse_info} should contain parsing information
  to deduce the values of \code{tile}, \code{start_date},
  \code{end_date} and \code{band} from the file name.
  Default is c("X1", "X2", "tile", "start_date", "end_date", "band").
  Cubes processed by \code{sits} adhere to this format.
}
\examples{
if (sits_run_examples()) {
    # create a random forest model
    rfor_model <- sits_train(samples_modis_ndvi, sits_rfor())
    # create a data cube from local files
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir
    )
    # classify a data cube
    probs_cube <- sits_classify(
        data = cube, ml_model = rfor_model, output_dir = tempdir()
    )
    # plot the probability cube
    plot(probs_cube)

    # obtain and name the labels of the local probs cube
    labels <- sits_labels(rfor_model)
    names(labels) <- seq_along(labels)

    # recover the local probability cube
    probs_local_cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = tempdir(),
        bands = "probs",
        labels = labels
    )
    # compare the two plots (they should be the same)
    plot(probs_local_cube)

    # smooth the probability cube using Bayesian statistics
    bayes_cube <- sits_smooth(probs_cube, output_dir = tempdir())
    # plot the smoothed cube
    plot(bayes_cube)

    # recover the local smoothed cube
    smooth_local_cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = tempdir(),
        bands = "bayes",
        labels = labels
    )
    # compare the two plots (they should be the same)
    plot(smooth_local_cube)

    # label the probability cube
    label_cube <- sits_label_classification(
        bayes_cube,
        output_dir = tempdir()
    )
    # plot the labelled cube
    plot(label_cube)

    # recover the local classified cube
    class_local_cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = tempdir(),
        bands = "class",
        labels = labels
    )
    # compare the two plots (they should be the same)
    plot(class_local_cube)

    # obtain an uncertainty cube with entropy
    entropy_cube <- sits_uncertainty(
        cube = bayes_cube,
        type = "entropy",
        output_dir = tempdir()
    )
    # plot entropy values
    plot(entropy_cube)

    # recover an uncertainty cube with entropy
    entropy_local_cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = tempdir(),
        bands = "entropy"
    )
    # plot recovered entropy values
    plot(entropy_local_cube)

    # obtain an uncertainty cube with margin
    margin_cube <- sits_uncertainty(
        cube = bayes_cube,
        type = "margin",
        output_dir = tempdir()
    )
    # plot entropy values
    plot(margin_cube)

    # recover an uncertainty cube with entropy
    margin_local_cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = tempdir(),
        bands = "margin"
    )
    # plot recovered entropy values
    plot(margin_local_cube)
}
}
