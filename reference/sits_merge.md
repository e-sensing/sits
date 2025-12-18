# Merge two data sets (time series or cubes)

To merge two series, we consider that they contain different attributes
but refer to the same data cube and spatiotemporal location. This
function is useful for merging different bands of the same location. For
example, one may want to put the raw and smoothed bands for the same set
of locations in the same tibble.

In the case of data cubes, the function merges the images based on the
following conditions:

1.  If the two cubes have different bands but compatible timelines, the
    bands are combined, and the timeline is adjusted to overlap. To
    create the overlap, we align the timelines like a "zipper": for each
    interval defined by a pair of consecutive dates in the first
    timeline, we include matching dates from the second timeline. If the
    second timeline has multiple dates in the same interval, only the
    minimum date is kept. This ensures the final timeline avoids
    duplicates and is consistent. This is useful when merging data from
    different sensors (e.g., Sentinel-1 with Sentinel-2).

2.  If the bands are the same, the cube will have the combined timeline
    of both cubes. This is useful for merging data from the same sensors
    from different satellites (e.g., Sentinel-2A with Sentinel-2B).

3.  otherwise, the function will produce an error.

## Usage

``` r
sits_merge(data1, data2, ...)

# S3 method for class 'sits'
sits_merge(data1, data2, ..., suffix = c(".1", ".2"))

# S3 method for class 'raster_cube'
sits_merge(data1, data2, ...)

# Default S3 method
sits_merge(data1, data2, ...)
```

## Arguments

- data1:

  Time series (tibble of class "sits") or data cube (tibble of class
  "raster_cube") .

- data2:

  Time series (tibble of class "sits") or data cube (tibble of class
  "raster_cube") .

- ...:

  Additional parameters

- suffix:

  If data1 and data2 are tibble with duplicate bands, this suffix will
  be added (character vector).

## Value

merged data sets (tibble of class "sits" or tibble of class
"raster_cube")

## Author

Felipe Carvalho, <felipe.carvalho@inpe.br>

Felipe Carlos, <efelipecarlos@gmail.com>

## Examples

``` r
if (sits_run_examples()) {
    # Retrieve a time series with values of NDVI
    point_ndvi <- sits_select(point_mt_6bands, bands = "NDVI")

    # Filter the point using the Whittaker smoother
    point_whit <- sits_filter(point_ndvi, sits_whittaker(lambda = 3.0))
    # Merge time series
    point_ndvi <- sits_merge(point_ndvi, point_whit, suffix = c("", ".WHIT"))

    # Plot the two points to see the smoothing effect
    plot(point_ndvi)
}
```
