% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sits_plot.R
\name{plot.uncertainty_cube}
\alias{plot.uncertainty_cube}
\title{Plot uncertainty cubes}
\usage{
\method{plot}{uncertainty_cube}(
  x,
  ...,
  tile = x[["tile"]][[1L]],
  roi = NULL,
  palette = "RdYlGn",
  rev = TRUE,
  scale = 1,
  first_quantile = 0.02,
  last_quantile = 0.98,
  max_cog_size = 1024L,
  legend_position = "inside"
)
}
\arguments{
\item{x}{Object of class "probs_image".}

\item{...}{Further specifications for \link{plot}.}

\item{tile}{Tiles to be plotted.}

\item{roi}{Spatial extent to plot in WGS 84 - named vector
with either (lon_min, lon_max, lat_min, lat_max) or
(xmin, xmax, ymin, ymax)}

\item{palette}{An RColorBrewer palette}

\item{rev}{Reverse the color order in the palette?}

\item{scale}{Scale to plot map (0.4 to 1.0)}

\item{first_quantile}{First quantile for stretching images}

\item{last_quantile}{Last quantile for stretching images}

\item{max_cog_size}{Maximum size of COG overviews (lines or columns)}

\item{legend_position}{Where to place the legend (default = "inside")}
}
\value{
A plot object produced showing the uncertainty
                      associated to each classified pixel.
}
\description{
plots a uncertainty cube
}
\note{
The following optional parameters are available to allow for detailed
      control over the plot output:
\itemize{
\item \code{graticules_labels_size}: size of coord labels (default = 0.7)
\item \code{legend_title_size}: relative size of legend title (default = 1.0)
\item \code{legend_text_size}: relative size of legend text (default = 1.0)
\item \code{legend_bg_color}: color of legend background (default = "white")
\item \code{legend_bg_alpha}: legend opacity (default = 0.5)
}
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
    # calculate uncertainty
    uncert_cube <- sits_uncertainty(probs_cube, output_dir = tempdir())
    # plot the resulting uncertainty cube
    plot(uncert_cube)
}
}
\author{
Gilberto Camara, \email{gilberto.camara@inpe.br}
}
