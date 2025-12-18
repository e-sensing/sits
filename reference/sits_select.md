# Filter a data set (tibble or cube) for bands, tiles, and dates

Filter the bands, tiles, dates and labels from a set of time series or
from a data cube.

## Usage

``` r
sits_select(data, ...)

# S3 method for class 'sits'
sits_select(
  data,
  ...,
  bands = NULL,
  start_date = NULL,
  end_date = NULL,
  dates = NULL,
  labels = NULL
)

# S3 method for class 'raster_cube'
sits_select(
  data,
  ...,
  bands = NULL,
  start_date = NULL,
  end_date = NULL,
  dates = NULL,
  tiles = NULL
)

# Default S3 method
sits_select(data, ...)
```

## Arguments

- data:

  Tibble with time series or data cube.

- ...:

  Additional parameters to be provided

- bands:

  Character vector with the names of the bands.

- start_date:

  Date in YYYY-MM-DD format: start date to be filtered.

- end_date:

  Date in YYYY-MM-DD format: end date to be filtered.

- dates:

  Character vector with sparse dates to be selected.

- labels:

  Character vector with sparse labels to be selected (Only applied for
  sits tibble data).

- tiles:

  Character vector with the names of the tiles.

## Value

Tibble with time series or data cube.

## Author

Rolf Simoes, <rolfsimoes@gmail.com>

Felipe Carlos, <efelipecarlos@gmail.com>

Felipe Carvalho, <felipe.carvalho@inpe.br>

## Examples

``` r
# Retrieve a set of time series with 2 classes
data(cerrado_2classes)
# Print the original bands
sits_bands(cerrado_2classes)
#> [1] "NDVI" "EVI" 
# Select only the NDVI band
data <- sits_select(cerrado_2classes, bands = c("NDVI"))
# Print the labels of the resulting tibble
sits_bands(data)
#> [1] "NDVI"
# select start and end date
point_2010 <- sits_select(point_mt_6bands,
    start_date = "2000-09-13",
    end_date = "2017-08-29"
)
```
