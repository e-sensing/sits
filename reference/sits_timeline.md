# Get timeline of a cube or a set of time series

This function returns the timeline for a given data set, either a set of
time series, a data cube, or a trained model.

## Usage

``` r
sits_timeline(data)

# S3 method for class 'sits'
sits_timeline(data)

# S3 method for class 'sits_model'
sits_timeline(data)

# S3 method for class 'raster_cube'
sits_timeline(data)

# S3 method for class 'derived_cube'
sits_timeline(data)

# S3 method for class 'tbl_df'
sits_timeline(data)

# Default S3 method
sits_timeline(data)
```

## Arguments

- data:

  Tibble of class "sits" or class "raster_cube"

## Value

Vector of class Date with timeline of samples or data cube.

## Author

Gilberto Camara, <gilberto.camara@inpe.br>

## Examples

``` r
sits_timeline(samples_modis_ndvi)
#>  [1] "2013-09-14" "2013-10-16" "2013-11-17" "2013-12-19" "2014-01-17"
#>  [6] "2014-02-18" "2014-03-22" "2014-04-23" "2014-05-25" "2014-06-26"
#> [11] "2014-07-28" "2014-08-29"
```
