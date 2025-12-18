# Filter time series with Savitzky-Golay filter

An optimal polynomial for warping a time series. The degree of smoothing
depends on the filter order (usually 3.0). The order of the polynomial
uses the parameter \`order\` (default = 3), the size of the temporal
window uses the parameter \`length\` (default = 5).

## Usage

``` r
sits_sgolay(data = NULL, order = 3L, length = 5L)
```

## Arguments

- data:

  Time series or matrix.

- order:

  Filter order (integer).

- length:

  Filter length (must be odd).

## Value

Filtered time series

## References

A. Savitzky, M. Golay, "Smoothing and Differentiation of Data by
Simplified Least Squares Procedures". Analytical Chemistry, 36 (8):
1627–39, 1964.

## Author

Rolf Simoes, <rolfsimoes@gmail.com>

Gilberto Camara, <gilberto.camara@inpe.br>

Felipe Carvalho, <felipe.carvalho@inpe.br>

## Examples

``` r
if (sits_run_examples()) {
    # Retrieve a time series with values of NDVI
    point_ndvi <- sits_select(point_mt_6bands, bands = "NDVI")

    # Filter the point using the Savitzky-Golay smoother
    point_sg <- sits_filter(point_ndvi,
        filter = sits_sgolay(order = 3, length = 5)
    )
    # Merge time series
    point_ndvi <- sits_merge(point_ndvi, point_sg, suffix = c("", ".SG"))

    # Plot the two points to see the smoothing effect
    plot(point_ndvi)
}
```
