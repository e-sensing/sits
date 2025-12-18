# Compute the minimum distances among samples and prediction points.

Compute the minimum distances among samples and samples to prediction
points, following the approach proposed by Meyer and Pebesma(2022).

## Usage

``` r
sits_geo_dist(samples, roi, n = 1000L, crs = "EPSG:4326")
```

## Arguments

- samples:

  Time series (tibble of class "sits").

- roi:

  A region of interest (ROI), either a file containing a shapefile or an
  "sf" object

- n:

  Maximum number of samples to consider (integer)

- crs:

  CRS of the `samples`.

## Value

A tibble with sample-to-sample and sample-to-prediction distances
(object of class "distances").

## Note

As pointed out by Meyer and Pebesma, many classifications using machine
learning assume that the reference data are independent and
well-distributed in space. In practice, many training samples are
strongly concentrated in some areas, and many large areas have no
samples. This function compares two distributions:

1.  The distribution of the spatial distances of reference data to their
    nearest neighbor (sample-to-sample.

2.  The distribution of distances from all points of study area to the
    nearest reference data point (sample-to-prediction).

## References

Meyer, H., Pebesma, E. "Machine learning-based global maps of ecological
variables and the challenge of assessing them", Nature Communications
13, 2208 (2022).
[doi:10.1038/s41467-022-29838-9](https://doi.org/10.1038/s41467-022-29838-9)
.

## Author

Alber Sanchez, <alber.ipia@inpe.br>

Rolf Simoes, <rolfsimoes@gmail.com>

Felipe Carvalho, <felipe.carvalho@inpe.br>

Gilberto Camara, <gilberto.camara@inpe.br>

## Examples

``` r
if (sits_run_examples()) {
    # read a shapefile for the state of Mato Grosso, Brazil
    mt_shp <- system.file("extdata/shapefiles/mato_grosso/mt.shp",
        package = "sits"
    )
    # convert to an sf object
    mt_sf <- sf::read_sf(mt_shp)
    # calculate sample-to-sample and sample-to-prediction distances
    distances <- sits_geo_dist(
        samples = samples_modis_ndvi,
        roi = mt_sf
    )
    # plot sample-to-sample and sample-to-prediction distances
    plot(distances)
}
```
