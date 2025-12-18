# Tuning machine learning models hyper-parameters

This function allow user building the hyper-parameters space used by
[`sits_tuning()`](https://e-sensing.github.io/sits/reference/sits_tuning.md)
function search randomly the best parameter combination.

Users should pass the possible values for hyper-parameters as constants
or by calling the following random functions:

- `uniform(min = 0, max = 1, n = 1)`: returns random numbers from a
  uniform distribution with parameters min and max.

- `choice(..., replace = TRUE, n = 1)`: returns random objects passed to
  `...` with replacement or not (parameter `replace`).

- `randint(min, max, n = 1)`: returns random integers from a uniform
  distribution with parameters min and max.

- `normal(mean = 0, sd = 1, n = 1)`: returns random numbers from a
  normal distribution with parameters min and max.

- `lognormal(meanlog = 0, sdlog = 1, n = 1)`: returns random numbers
  from a lognormal distribution with parameters min and max.

- `loguniform(minlog = 0, maxlog = 1, n = 1)`: returns random numbers
  from a loguniform distribution with parameters min and max.

- `beta(shape1, shape2, n = 1)`: returns random numbers from a beta
  distribution with parameters min and max.

These functions accepts `n` parameter to indicate how many values should
be returned.

## Usage

``` r
sits_tuning_hparams(...)
```

## Arguments

- ...:

  Used to prepare hyper-parameter space

## Value

A list containing the hyper-parameter space to be passed to
[`sits_tuning()`](https://e-sensing.github.io/sits/reference/sits_tuning.md)'s
`params` parameter.

## Examples

``` r
if (sits_run_examples()) {
    # find best learning rate parameters for TempCNN
    tuned <- sits_tuning(
        samples_modis_ndvi,
        ml_method = sits_tempcnn(),
        params = sits_tuning_hparams(
            optimizer = choice(
                torch::optim_adamw,
                torch::optim_adagrad
            ),
            opt_hparams = list(
                lr = loguniform(10^-2, 10^-4),
                weight_decay = loguniform(10^-2, 10^-8)
            )
        ),
        trials = 20,
        multicores = 2,
        progress = FALSE
    )
}
```
