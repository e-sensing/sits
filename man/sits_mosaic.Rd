% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sits_mosaic.R
\name{sits_mosaic}
\alias{sits_mosaic}
\title{Mosaic classified cubes}
\usage{
sits_mosaic(
  cube,
  crs = "EPSG:3857",
  roi = NULL,
  multicores = 2,
  output_dir,
  version = "v1",
  progress = TRUE
)
}
\arguments{
\item{cube}{A sits data cube.}

\item{crs}{A target coordinate reference system of raster mosaic.
The provided crs could be a string
(e.g, "EPSG:4326" or a proj4string), or
an EPSG code number (e.g. 4326).
Default is "EPSG:3857" - WGS 84 / Pseudo-Mercator.}

\item{roi}{Region of interest (see below).}

\item{multicores}{Number of cores that will be used to
crop the images in parallel.}

\item{output_dir}{Directory for output images.}

\item{version}{Version of resulting image
(in the case of multiple tests)}

\item{progress}{Show progress bar? Default is TRUE.}
}
\value{
a sits cube with only one tile.
}
\description{
Creates a mosaic of all tiles of a sits cube.
Mosaics can be created from EO cubes and derived cubes.
In sits EO cubes, the mosaic will be generated for each band and date.
It is recommended to filter the image with the less cloud cover to create
a mosaic for the EO cubes.
It is possible to provide a \code{roi} to crop the mosaic.
}
\note{
The "roi" parameter defines a region of interest. It can be
 an sf_object, a shapefile, or a bounding box vector with
 named XY values (\code{xmin}, \code{xmax}, \code{ymin}, \code{ymax}) or
 named lat/long values (\code{lon_min}, \code{lon_max},
   \code{lat_min}, \code{lat_max}).

 The user should specify the crs of the mosaic since in many cases the
 input images will be in different coordinate systems. For example,
 when mosaicking Sentinel-2 images the inputs will be in general in
 different UTM grid zones.
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
    # smooth the probability cube using Bayesian statistics
    bayes_cube <- sits_smooth(probs_cube, output_dir = tempdir())
    # label the probability cube
    label_cube <- sits_label_classification(
        bayes_cube,
        output_dir = tempdir()
    )
    # create roi
    roi <- sf::st_sfc(
        sf::st_polygon(
            list(rbind(
                c(-55.64768, -11.68649),
                c(-55.69654, -11.66455),
                c(-55.62973, -11.61519),
                c(-55.64768, -11.68649)
            ))
        ),
        crs = "EPSG:4326"
    )
    # crop and mosaic classified image
    mosaic_cube <- sits_mosaic(
        cube = label_cube,
        roi = roi,
        crs = "EPSG:4326",
        output_dir = tempdir()
    )
}

}
\author{
Felipe Carvalho, \email{felipe.carvalho@inpe.br}

Rolf Simoes,     \email{rolf.simoes@inpe.br}
}
