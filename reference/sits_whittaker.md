# Filter time series with whittaker filter

The algorithm searches for an optimal warping polynomial. The degree of
smoothing depends on smoothing factor lambda (usually from 0.5 to 10.0).
Use lambda = 0.5 for very slight smoothing and lambda = 5.0 for strong
smoothing.

## Usage

``` r
sits_whittaker(data = NULL, lambda = 0.5)
```

## Arguments

- data:

  Time series or matrix.

- lambda:

  Smoothing factor to be applied (default 0.5).

## Value

Filtered time series

## References

Francesco Vuolo, Wai-Tim Ng, Clement Atzberger, "Smoothing and
gap-filling of high resolution multi-spectral time series: Example of
Landsat data", Int Journal of Applied Earth Observation and
Geoinformation, vol. 57, pg. 202-213, 2107.

## See also

[sits_apply](https://e-sensing.github.io/sits/reference/sits_apply.md)

## Author

Rolf Simoes, <rolfsimoes@gmail.com>

Gilberto Camara, <gilberto.camara@inpe.br>

Felipe Carvalho, <felipe.carvalho@inpe.br>

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
