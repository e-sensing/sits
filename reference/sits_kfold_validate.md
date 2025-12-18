# Cross-validate time series samples

Splits the set of time series into training and validation and perform
k-fold cross-validation.

## Usage

``` r
sits_kfold_validate(
  samples,
  folds = 5L,
  ml_method = sits_rfor(),
  filter_fn = NULL,
  impute_fn = impute_linear(),
  multicores = 2L,
  gpu_memory = 4L,
  batch_size = 2L^gpu_memory,
  progress = TRUE
)
```

## Arguments

- samples:

  Time series.

- folds:

  Number of partitions to create.

- ml_method:

  Machine learning method.

- filter_fn:

  Smoothing filter to be applied - optional (closure containing object
  of class "function").

- impute_fn:

  Imputation function to remove NA.

- multicores:

  Number of cores to process in parallel.

- gpu_memory:

  Memory available in GPU in GB (default = 4)

- batch_size:

  Batch size for GPU classification.

- progress:

  Logical: Show progress bar?

## Value

A
[`caret::confusionMatrix`](https://rdrr.io/pkg/caret/man/confusionMatrix.html)
object to be used for validation assessment.

## Note

Cross-validation is a technique for assessing how the results of a
statistical analysis will generalize to an independent data set. It is
mainly used in settings where the goal is prediction, and one wants to
estimate how accurately a predictive model will perform. One round of
cross-validation involves partitioning a sample of data into
complementary subsets, performing the analysis on one subset (called the
training set), and validating the analysis on the other subset (called
the validation set or testing set).

The k-fold cross validation method involves splitting the dataset into
k-subsets. For each subset is held out while the model is trained on all
other subsets. This process is completed until accuracy is determine for
each instance in the dataset, and an overall accuracy estimate is
provided.

This function returns the confusion matrix, and Kappa values.

## Author

Rolf Simoes, <rolfsimoes@gmail.com>

Gilberto Camara, <gilberto.camara@inpe.br>

## Examples

``` r
if (sits_run_examples()) {
    # A dataset containing a tibble with time series samples
    # for the Mato Grosso state in Brasil
    # create a list to store the results
    results <- list()
    # accuracy assessment lightTAE
    acc_rfor <- sits_kfold_validate(
        samples_modis_ndvi,
        folds = 5,
        ml_method = sits_rfor()
    )
    # use a name
    acc_rfor$name <- "Rfor"
    # put the result in a list
    results[[length(results) + 1]] <- acc_rfor
    # save to xlsx file
    sits_to_xlsx(
        results,
        file = tempfile("accuracy_mato_grosso_dl_", fileext = ".xlsx")
    )
}
```
