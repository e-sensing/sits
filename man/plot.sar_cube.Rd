% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sits_plot.R
\name{plot.sar_cube}
\alias{plot.sar_cube}
\title{Plot SAR data cubes}
\usage{
\method{plot}{sar_cube}(
  x,
  ...,
  band = NULL,
  red = NULL,
  green = NULL,
  blue = NULL,
  tile = x[["tile"]][[1L]],
  dates = NULL,
  roi = NULL,
  palette = "Greys",
  rev = FALSE,
  scale = 1,
  first_quantile = 0.05,
  last_quantile = 0.95,
  max_cog_size = 1024L,
  legend_position = "inside"
)
}
\arguments{
\item{x}{Object of class "raster_cube".}

\item{...}{Further specifications for \link{plot}.}

\item{band}{Band for plotting grey images.}

\item{red}{Band for red color.}

\item{green}{Band for green color.}

\item{blue}{Band for blue color.}

\item{tile}{Tile to be plotted.}

\item{dates}{Dates to be plotted.}

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
A plot object with an RGB image
                      or a B/W image on a color scale for SAR cubes
}
\description{
Plot SAR raster cube
}
\note{
Use \code{scale} parameter for general output control.
      The \code{dates} parameter indicates the date
      allows plotting of different dates when
      a single band and three dates are provided, `sits` will plot a
      multi-temporal RGB image for a single band (useful in the case of
      SAR data). For RGB bands with multi-dates, multiple plots will be
      produced.

The following optional parameters are available to allow for detailed
      control over the plot output:
\itemize{
\item \code{graticules_labels_size}: size of coord labels (default = 0.7)
\item \code{legend_title_size}: relative size of legend title (default = 0.7)
\item \code{legend_text_size}: relative size of legend text (default = 0.7)
\item \code{legend_bg_color}: color of legend background (default = "white")
\item \code{legend_bg_alpha}: legend opacity (default = 0.3)
}
}
\examples{
if (sits_run_examples()) {
    # create a SAR data cube from cloud services
    cube_s1_grd <- sits_cube(
        source = "MPC",
        collection = "SENTINEL-1-GRD",
        bands = c("VV", "VH"),
        orbit = "descending",
        tiles = c("21LUJ"),
        start_date = "2021-08-01",
        end_date = "2021-09-30"
    )
    # plot VH band of the first date of the data cube
    plot(cube_s1_grd, band = "VH")
}
}
\author{
Gilberto Camara, \email{gilberto.camara@inpe.br}
}
