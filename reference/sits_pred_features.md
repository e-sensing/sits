# Obtain numerical values of predictors for time series samples

Predictors are X-Y values required for machine learning algorithms,
organized as a data table where each row corresponds to a training
sample. The first two columns of the predictors table are categorical
("label_id" and "label"). The other columns are the values of each band
and time, organized first by band and then by time. This function
returns the numeric values associated to each sample.

## Usage

``` r
sits_pred_features(pred)
```

## Arguments

- pred:

  X-Y predictors: a data.frame with one row per sample.

## Value

The Y predictors for the sample: data.frame with one row per sample.

## Author

Gilberto Camara, <gilberto.camara@inpe.br>

## Examples

``` r
if (sits_run_examples()) {
    pred <- sits_predictors(samples_modis_ndvi)
    features <- sits_pred_features(pred)
}
```
