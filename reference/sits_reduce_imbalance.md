# Reduce imbalance in a set of samples

Takes a sits tibble with different labels and returns a new tibble.
Deals with class imbalance using the synthetic minority oversampling
technique (SMOTE) for oversampling. Undersampling is done using the SOM
methods available in the sits package.

## Usage

``` r
sits_reduce_imbalance(
  samples,
  n_samples_over = 200L,
  n_samples_under = 400L,
  method = "smote",
  multicores = 2L
)
```

## Arguments

- samples:

  Sample set to rebalance

- n_samples_over:

  Number of samples to oversample for classes with samples less than
  this number.

- n_samples_under:

  Number of samples to undersample for classes with samples more than
  this number.

- method:

  Method for oversampling (default = "smote")

- multicores:

  Number of cores to process the data (default 2).

## Value

A sits tibble with reduced sample imbalance.

## Note

Many training samples for Earth observation data analysis are
imbalanced. This situation arises when the distribution of samples
associated with each label is uneven. Sample imbalance is an undesirable
property of a training set. Reducing sample imbalance improves
classification accuracy.

The function `sits_reduce_imbalance` increases the number of samples of
least frequent labels, and reduces the number of samples of most
frequent labels. To generate new samples, `sits` uses the SMOTE method
that estimates new samples by considering the cluster formed by the
nearest neighbors of each minority label.

To perform undersampling, `sits_reduce_imbalance`) builds a SOM map for
each majority label based on the required number of samples. Each
dimension of the SOM is set to ceiling(sqrt(new_number_samples/4)) to
allow a reasonable number of neurons to group similar samples. After
calculating the SOM map, the algorithm extracts four samples per neuron
to generate a reduced set of samples that approximates the variation of
the original one. See also
[`sits_som_map`](https://e-sensing.github.io/sits/reference/sits_som_map.md).

## References

The reference paper on SMOTE is N. V. Chawla, K. W. Bowyer, L. O.Hall,
W. P. Kegelmeyer, “SMOTE: synthetic minority over-sampling technique”,
Journal of artificial intelligence research, 321-357, 2002,
[doi:10.1613/jair.953](https://doi.org/10.1613/jair.953) .

The SOM map technique for time series is described in the paper: Lorena
Santos, Karine Ferreira, Gilberto Camara, Michelle Picoli, Rolf Simoes,
“Quality control and class noise reduction of satellite image time
series”. ISPRS Journal of Photogrammetry and Remote Sensing, vol. 177,
pp 75-88, 2021.
[doi:10.1016/j.isprsjprs.2021.04.014](https://doi.org/10.1016/j.isprsjprs.2021.04.014)
.

## Author

Gilberto Camara, <gilberto.camara@inpe.br>

## Examples

``` r
if (sits_run_examples()) {
    # print the labels summary for a sample set
    summary(samples_modis_ndvi)
    # reduce the sample imbalance
    new_samples <- sits_reduce_imbalance(samples_modis_ndvi,
        n_samples_over = 200,
        n_samples_under = 200,
        multicores = 1
    )
    # print the labels summary for the rebalanced set
    summary(new_samples)
}
```
