% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sits_cube_local.R
\name{sits_cube.local_cube}
\alias{sits_cube.local_cube}
\title{Create sits cubes from cubes in flat files in a local}
\usage{
\method{sits_cube}{local_cube}(
  source,
  collection,
  ...,
  bands = NULL,
  tiles = NULL,
  start_date = NULL,
  end_date = NULL,
  data_dir,
  parse_info = c("X1", "X2", "tile", "band", "date"),
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
from which the data has been downloaded.}

\item{collection}{Image collection in data source.
To find out the supported collections,
use \code{\link{sits_list_collections}()}).}

\item{...}{Other parameters to be passed for specific types.}

\item{bands}{Spectral bands and indices to be included
in the cube (optional).}

\item{tiles}{Tiles from the collection to be included in
the cube (see details below).}

\item{start_date, end_date}{Initial and final dates to include
images from the collection in the cube (optional).
(Date in YYYY-MM-DD format).}

\item{data_dir}{Local directory where images are stored.}

\item{parse_info}{Parsing information for local files.}

\item{delim}{Delimiter for parsing local files (default = "_")}

\item{multicores}{Number of workers for parallel processing
(integer, min = 1, max = 2048).}

\item{progress}{Logical: show a progress bar?}
}
\value{
A \code{tibble} describing the contents of a data cube.
}
\description{
Creates data cubes based on files on local directory. Assumes users
have downloaded the data from a known cloud collection or the data
has been created by \code{sits}.
}
\note{
To create a cube from local files, please inform:
\itemize{
 \item \code{source}: The data provider from which the data was
 downloaded (e.g, "BDC", "MPC");
 \item \code{collection}: The collection from which the data comes from.
 (e.g., \code{"SENTINEL-2-L2A"} for the Sentinel-2 MPC collection level 2A);
 \item \code{data_dir}: The local directory where the image files are stored.
 \item \code{parse_info}: Defines how to extract metadata from file names
 by specifying the order and meaning of each part, separated by the
 \code{"delim"} character. Default value is
 \code{c("X1", "X2", "tile", "band", "date")}.
 \item \code{delim}: The delimiter character used to separate components in
 the file names. Default is \code{"_"}.
}
Please ensure that local files meet the following requirements:

\itemize{
 \item All image files must have the same spatial resolution and projection;
 \item Each file should represent a single image band for a single date;
 \item File names must include information about the \code{tile},
 \code{date}, and \code{band} in their names.
 \item{The \code{parse_info} parameter tells \code{sits} how to extract
         metadata from file names.}
 \item{By default the \code{parse_info} parameter is
         \code{c(satellite, sensor, tile, band, date)}.}
}
Example of supported file names are:
\itemize{
 \item \code{"CBERS-4_WFI_022024_B13_2021-05-15.tif"};
 \item \code{"SENTINEL-1_GRD_30TXL_VV_2023-03-10.tif"};
 \item \code{"LANDSAT-8_OLI_198030_B04_2020-09-12.tif"}.
}

When you load a local data cube specifying the \code{source}
(e.g., AWS, MPC) and \code{collection}, \code{sits} assumes that the data
properties (e.g., scale factor, minimum, and maximum values) match those
defined for the selected provider. If you are working with
custom data from an unsupported source or data that does not follow the
standard definitions of providers in sits, refer to the Technical Annex of
the \code{sits} online book for guidance on handling such cases
(e-sensing.github.io/sitsbook/technical-annex.html).
}
\examples{
if (sits_run_examples()) {
    # --- Create a cube based on a local MODIS data
    # MODIS local files have names such as
    # "TERRA_MODIS_012010_NDVI_2013-09-14.jp2"
    # see the parse info parameter as an example on how to
    # decode local files
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    modis_cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir,
        parse_info = c("satellite", "sensor", "tile", "band", "date")
    )
}
}
