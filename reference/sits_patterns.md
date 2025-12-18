# Find temporal patterns associated to a set of time series

This function takes a set of time series samples as input estimates a
set of patterns. The patterns are calculated using a GAM model. The idea
is to use a formula of type y ~ s(x), where x is a temporal reference
and y if the value of the signal. For each time, there will be as many
predictions as there are sample values. The GAM model predicts a
suitable approximation that fits the assumptions of the statistical
model, based on a smooth function.

This method is based on the "createPatterns" method of the R dtwSat
package, which is also described in the reference paper.

## Usage

``` r
sits_patterns(data = NULL, freq = 8L, formula = y ~ s(x), ...)
```

## Arguments

- data:

  Time series.

- freq:

  Interval in days for estimates.

- formula:

  Formula to be applied in the estimate.

- ...:

  Any additional parameters.

## Value

Time series with patterns.

## References

Maus V, Camara G, Cartaxo R, Sanchez A, Ramos F, Queiroz GR. A
Time-Weighted Dynamic Time Warping Method for Land-Use and Land-Cover
Mapping. IEEE Journal of Selected Topics in Applied Earth Observations
and Remote Sensing, 9(8):3729-3739, August 2016. ISSN 1939-1404.
[doi:10.1109/JSTARS.2016.2517118](https://doi.org/10.1109/JSTARS.2016.2517118)
.

Maus, V., Câmara, G., Appel, M., & Pebesma, E. (2019). dtwSat:
Time-Weighted Dynamic Time Warping for Satellite Image Time Series
Analysis in R. Journal of Statistical Software, 88(5), 1–31.
[doi:10.18637/jss.v088.i05](https://doi.org/10.18637/jss.v088.i05) .

## Author

Victor Maus, <vwmaus1@gmail.com>

Gilberto Camara, <gilberto.camara@inpe.br>

Rolf Simoes, <rolfsimoes@gmail.com>

## Examples

``` r
if (sits_run_examples()) {
    patterns <- sits_patterns(cerrado_2classes)
    plot(patterns)
}
```
