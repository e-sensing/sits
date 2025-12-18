# Obtain predictors for time series samples

Predictors are X-Y values required for machine learning algorithms,
organized as a data table where each row corresponds to a training
sample. The first two columns of the predictors table are categorical
(`label_id` and `label`). The other columns are the values of each band
and time, organized first by band and then by time.

## Usage

``` r
sits_predictors(samples)
```

## Arguments

- samples:

  Time series in sits format (tibble of class "sits")

## Value

The predictors for the sample: a data.frame with one row per sample.

## Author

Gilberto Camara, <gilberto.camara@inpe.br>

## Examples

``` r
if (sits_run_examples()) {
    # Include a new machine learning function (multiple linear regression)
    # function that returns mlr model based on a sits sample tibble

    sits_mlr <- function(samples = NULL, formula = sits_formula_linear(),
                         n_weights = 20000, maxit = 2000) {
        # create a training function
        train_fun <- function(samples) {
            # Data normalization
            ml_stats <- sits_stats(samples)
            train_samples <- sits_predictors(samples)
            train_samples <- sits_pred_normalize(
                pred = train_samples,
                stats = ml_stats
            )
            formula <- formula(train_samples[, -1])
            # call method and return the trained model
            result_mlr <- nnet::multinom(
                formula = formula,
                data = train_samples,
                maxit = maxit,
                MaxNWts = n_weights,
                trace = FALSE,
                na.action = stats::na.fail
            )

            # construct model predict closure function and returns
            predict_fun <- function(values) {
                # retrieve the prediction (values and probs)
                prediction <- tibble::as_tibble(
                    stats::predict(result_mlr,
                        newdata = values,
                        type = "probs"
                    )
                )
                return(prediction)
            }
            class(predict_fun) <- c("sits_model", class(predict_fun))
            return(predict_fun)
        }
        result <- sits_factory_function(samples, train_fun)
        return(result)
    }
    # create an mlr model using a set of samples
    mlr_model <- sits_train(samples_modis_ndvi, sits_mlr)
    # classify a point
    point_ndvi <- sits_select(point_mt_6bands, bands = "NDVI")
    point_class <- sits_classify(point_ndvi, mlr_model, multicores = 1)
    plot(point_class)
}
```
