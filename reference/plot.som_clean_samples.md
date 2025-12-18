# Plot SOM samples evaluated

It is useful to visualize the output of the SOM evaluation, which
classifies the samples as "clean" (good samples), "remove" (possible
outliers), and "analyse" (borderline cases). This function plots the
percentual distribution of the SOM evaluation per class. To use it,
please run `sits_som_clean_samples` using the parameter "keep" as
"c("clean", "analyze", "remove").

## Usage

``` r
# S3 method for class 'som_clean_samples'
plot(x, ...)
```

## Arguments

- x:

  Object of class "som_clean_samples".

- ...:

  Further specifications for
  [plot](https://e-sensing.github.io/sits/reference/plot.md).

## Value

Called for side effects.

## Author

Estefania Pizarro, <eapizarroa@ine.gob.cl>

## Examples

``` r
if (sits_run_examples()) {
    # create a SOM map
    som_map <- sits_som_map(samples_modis_ndvi)
    # plot the SOM map
    eval <- sits_som_clean_samples(som_map)
    plot(eval)
}
```
