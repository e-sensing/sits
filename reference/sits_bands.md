# Get the names of the bands

Finds the names of the bands of a set of time series or of a data cube

## Usage

``` r
sits_bands(x)

# S3 method for class 'sits'
sits_bands(x)

# S3 method for class 'raster_cube'
sits_bands(x)

# S3 method for class 'patterns'
sits_bands(x)

# S3 method for class 'sits_model'
sits_bands(x)

# Default S3 method
sits_bands(x)

sits_bands(x) <- value

# S3 method for class 'sits'
sits_bands(x) <- value

# S3 method for class 'raster_cube'
sits_bands(x) <- value

# Default S3 method
sits_bands(x) <- value
```

## Arguments

- x:

  Valid sits tibble (time series or a cube)

- value:

  New value for the bands

## Value

A vector with the names of the bands.

## Author

Gilberto Camara, <gilberto.camara@inpe.br>

Rolf Simoes, <rolfsimoes@gmail.com>

## Examples

``` r
if (sits_run_examples()) {
    # Create a data cube from local files
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir
    )
    # Get the bands from a daya cube
    bands <- sits_bands(cube)
    # Get the bands from a sits tibble
    bands <- sits_bands(samples_modis_ndvi)
    # Get the bands from patterns
    bands <- sits_bands(sits_patterns(samples_modis_ndvi))
    # Get the bands from ML model
    rf_model <- sits_train(samples_modis_ndvi, sits_rfor())
    bands <- sits_bands(rf_model)
    # Set the bands for a SITS time series
    sits_bands(samples_modis_ndvi) <- "NDVI2"
    # Set the bands for a SITS cube
    sits_bands(cube) <- "NDVI2"
}
```
