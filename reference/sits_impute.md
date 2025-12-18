# Replace NA values in time series with imputation function

Remove NA

## Usage

``` r
sits_impute(samples, impute_fn = impute_linear())
```

## Arguments

- samples:

  A time series tibble

- impute_fn:

  Imputation function

## Value

A set of filtered time series using the imputation function.

## Author

Gilberto Camara, <gilberto.camara@inpe.br>
