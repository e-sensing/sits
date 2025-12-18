# Make a kernel density plot of samples distances.

Make a kernel density plot of samples distances.

## Usage

``` r
# S3 method for class 'geo_distances'
plot(x, y, ...)
```

## Arguments

- x:

  Object of class "geo_distances".

- y:

  Ignored.

- ...:

  Further specifications for
  [plot](https://e-sensing.github.io/sits/reference/plot.md).

## Value

A plot showing the sample-to-sample distances and sample-to-prediction
distances.

## References

Hanna Meyer and Edzer Pebesma, "Machine learning-based global maps of
ecological variables and the challenge of assessing them" Nature
Communications, 13,2022.
[doi:10.1038/s41467-022-29838-9](https://doi.org/10.1038/s41467-022-29838-9)
.

## Author

Felipe Souza, <lipecaso@gmail.com>

Rolf Simoes, <rolfsimoes@gmail.com>

Alber Sanchez, <alber.ipia@inpe.br>

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
    distances <- sits_geo_dist(samples_modis_ndvi, mt_sf)
    # plot sample-to-sample and sample-to-prediction distances
    plot(distances)
}
```
