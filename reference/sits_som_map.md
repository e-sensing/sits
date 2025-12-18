# Build a SOM for quality analysis of time series samples

These function use self-organized maps to perform quality analysis in
satellite image time series.

## Usage

``` r
sits_som_map(
  data,
  grid_xdim = 10L,
  grid_ydim = 10L,
  alpha = 1,
  rlen = 100L,
  distance = "dtw",
  som_radius = 2L,
  mode = "online"
)
```

## Arguments

- data:

  A tibble with samples to be clustered.

- grid_xdim:

  X dimension of the SOM grid (default = 25).

- grid_ydim:

  Y dimension of the SOM grid.

- alpha:

  Starting learning rate (decreases according to number of iterations).

- rlen:

  Number of iterations to produce the SOM.

- distance:

  The type of similarity measure (distance). The following similarity
  measurements are supported: `"euclidean"` and `"dtw"`. The default
  similarity measure is `"dtw"`.

- som_radius:

  Radius of SOM neighborhood.

- mode:

  Type of learning algorithm. The following learning algorithm are
  available: `"online"`, `"batch"`, and `"pbatch"`. The default learning
  algorithm is `"online"`.

## Value

`sits_som_map()` produces a list with three members: (1) the samples
tibble, with one additional column indicating to which neuron each
sample has been mapped; (2) the Kohonen map, used for plotting and
cluster quality measures; (3) a tibble with the labelled neurons, where
each class of each neuron is associated to two values: (a) the prior
probability that this class belongs to a cluster based on the frequency
of samples of this class allocated to the neuron; (b) the posterior
probability that this class belongs to a cluster, using data for the
neighbours on the SOM map.

## Note

`sits_som_map` creates a SOM map, where high-dimensional data is mapped
into a two dimensional map, keeping the topological relations between
data patterns. Each sample is assigned to a neuron, and neurons are
placed in the grid based on similarity.

[`sits_som_evaluate_cluster`](https://e-sensing.github.io/sits/reference/sits_som_evaluate_cluster.md)
analyses the neurons of the SOM map, and builds clusters based on them.
Each cluster is a neuron or a set of neuron categorized with same label.
It produces a tibble with the percentage of mixture of classes in each
cluster.

[`sits_som_clean_samples`](https://e-sensing.github.io/sits/reference/sits_som_clean_samples.md)
evaluates sample quality based on the results of the SOM map. The
algorithm identifies noisy samples, using \`prior_threshold\` for the
prior probability and \`posterior_threshold\` for the posterior
probability. Each sample receives an evaluation tag, according to the
following rule: (a) If the prior probability is \< \`prior_threshold\`,
the sample is tagged as "remove"; (b) If the prior probability is \>=
\`prior_threshold\` and the posterior probability is
\>=\`posterior_threshold\`, the sample is tagged as "clean"; (c) If the
prior probability is \>= \`posterior_threshold\` and the posterior
probability is \< \`posterior_threshold\`, the sample is tagged as
"analyze" for further inspection.

The user can define which tagged samples will be returned using the
"keep" parameter, with the following options: "clean", "analyze",
"remove".

To learn more about the learning algorithms, check the
[`kohonen::supersom`](https://rdrr.io/pkg/kohonen/man/supersom.html)
function.

The `sits` package implements the `"dtw"` (Dynamic Time Warping)
similarity measure. The `"euclidean"` similarity measurement come from
the
[`kohonen::supersom (dist.fcts)`](https://rdrr.io/pkg/kohonen/man/supersom.html)
function.

## References

Lorena Santos, Karine Ferreira, Gilberto Camara, Michelle Picoli, Rolf
Simoes, “Quality control and class noise reduction of satellite image
time series”. ISPRS Journal of Photogrammetry and Remote Sensing, vol.
177, pp 75-88, 2021.
[doi:10.1016/j.isprsjprs.2021.04.014](https://doi.org/10.1016/j.isprsjprs.2021.04.014)
.

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
