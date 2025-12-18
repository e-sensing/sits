# Filter time series with smoothing filter

Applies a filter to all bands, using a filter function such as
[`sits_whittaker`](https://e-sensing.github.io/sits/reference/sits_whittaker.md)
or
[`sits_sgolay`](https://e-sensing.github.io/sits/reference/sits_sgolay.md).

## Usage

``` r
sits_filter(data, filter = sits_whittaker())
```

## Arguments

- data:

  Time series (tibble of class "sits") or matrix.

- filter:

  Filter function to be applied.

## Value

Filtered time series

## Examples

``` r
if (sits_run_examples()) {
    # Retrieve a time series with values of NDVI
    point_ndvi <- sits_select(point_mt_6bands, bands = "NDVI")
    # Filter the point using the Whittaker smoother
    point_whit <- sits_filter(point_ndvi, sits_whittaker(lambda = 3.0))
    # Merge time series
    point_ndvi <- sits_merge(point_ndvi, point_whit,
        suffix = c("", ".WHIT")
    )
    # Plot the two points to see the smoothing effect
    plot(point_ndvi)
}
```
