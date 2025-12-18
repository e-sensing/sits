# Validate time series samples

One round of cross-validation involves partitioning a sample of data
into complementary subsets, performing the analysis on one subset
(called the training set), and validating the analysis on the other
subset (called the validation set or testing set).

The function takes two arguments: a set of time series with a machine
learning model and another set with validation samples. If the
validation sample set is not provided, The sample dataset is split into
two parts, as defined by the parameter validation_split. The accuracy is
determined by the result of the validation test set.

This function returns the confusion matrix, and Kappa values.

## Usage

``` r
sits_validate(
  samples,
  samples_validation = NULL,
  validation_split = 0.2,
  ml_method = sits_rfor(),
  gpu_memory = 4L,
  batch_size = 2L^gpu_memory
)
```

## Arguments

- samples:

  Time series to be validated (class "sits").

- samples_validation:

  Optional: Time series used for validation (class "sits")

- validation_split:

  Percent of original time series set to be used for validation if
  samples_validation is NULL (numeric value).

- ml_method:

  Machine learning method (function)

- gpu_memory:

  Memory available in GPU in GB (default = 4)

- batch_size:

  Batch size for GPU classification.

## Value

A
[`caret::confusionMatrix`](https://rdrr.io/pkg/caret/man/confusionMatrix.html)
object to be used for validation assessment.

## Note

When using a GPU for deep learning, `gpu_memory` indicates the memory of
the graphics card which is available for processing. The parameter
`batch_size` defines the size of the matrix (measured in number of rows)
which is sent to the GPU for classification. Users can test different
values of `batch_size` to find out which one best fits their GPU
architecture.

It is not possible to have an exact idea of the size of Deep Learning
models in GPU memory, as the complexity of the model and factors such as
CUDA Context increase the size of the model in memory. Therefore, we
recommend that you leave at least 1GB free on the video card to store
the Deep Learning model that will be used.

For users of Apple M3 chips or similar with a Neural Engine, be aware
that these chips share memory between the GPU and the CPU. Tests
indicate that the `memsize` should be set to half to the total memory
and the `batch_size` parameter should be a small number (we suggest the
value of 64). Be aware that increasing these parameters may lead to
memory conflicts.

## Author

Rolf Simoes, <rolfsimoes@gmail.com>

Gilberto Camara, <gilberto.camara@inpe.br>

## Examples

``` r
if (sits_run_examples()) {
    samples <- sits_sample(cerrado_2classes, frac = 0.5)
    samples_validation <- sits_sample(cerrado_2classes, frac = 0.5)
    conf_matrix_1 <- sits_validate(
        samples = samples,
        samples_validation = samples_validation,
        ml_method = sits_rfor()
    )
    conf_matrix_2 <- sits_validate(
        samples = cerrado_2classes,
        validation_split = 0.2,
        ml_method = sits_rfor()
    )
}
```
