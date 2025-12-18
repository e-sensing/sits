# Cleans the samples based on SOM map information

`sits_som_clean_samples()` evaluates the quality of the samples based on
the results of the SOM map.

## Usage

``` r
sits_som_clean_samples(
  som_map,
  prior_threshold = 0.6,
  posterior_threshold = 0.6,
  keep = c("clean", "analyze", "remove")
)
```

## Arguments

- som_map:

  Returned by
  [`sits_som_map`](https://e-sensing.github.io/sits/reference/sits_som_map.md).

- prior_threshold:

  Threshold of conditional probability (frequency of samples assigned to
  the same SOM neuron).

- posterior_threshold:

  Threshold of posterior probability (influenced by the SOM
  neighborhood).

- keep:

  Which types of evaluation to be maintained in the data.

## Value

tibble with an two additional columns. The first indicates if each
sample is clean, should be analyzed or should be removed. The second is
the posterior probability of the sample. The "keep" parameter indicates
which

## Note

The algorithm identifies noisy samples, using \`prior_threshold\` for
the prior probability and \`posterior_threshold\` for the posterior
probability. Each sample receives an evaluation tag, according to the
following rule: (a) If the prior probability is \< \`prior_threshold\`,
the sample is tagged as "remove"; (b) If the prior probability is \>=
\`prior_threshold\` and the posterior probability is
\>=\`posterior_threshold\`, the sample is tagged as "clean"; (c) If the
prior probability is \>= \`posterior_threshold\` and the posterior
probability is \< \`posterior_threshold\`, the sample is tagged as
"analyze" for further inspection. The user can define which tagged
samples will be returned using the "keep" parameter, with the following
options: "clean", "analyze", "remove".

## Author

Lorena Alves, <lorena.santos@inpe.br>

Karine Ferreira. <karine.ferreira@inpe.br>

Gilberto Camara, <gilberto.camara@inpe.br>

## Examples

``` r
if (sits_run_examples()) {
    # create a som map
    som_map <- sits_som_map(samples_modis_ndvi)
    # plot the som map
    plot(som_map)
    # evaluate the som map and create clusters
    clusters_som <- sits_som_evaluate_cluster(som_map)
    # plot the cluster evaluation
    plot(clusters_som)
    # clean the samples
    new_samples <- sits_som_clean_samples(som_map)
}
```
