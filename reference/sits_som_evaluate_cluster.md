# Evaluate cluster

`sits_som_evaluate_cluster()` produces a tibble with the clusters found
by the SOM map. For each cluster, it provides the percentage of classes
inside it.

## Usage

``` r
sits_som_evaluate_cluster(som_map)
```

## Arguments

- som_map:

  A SOM map produced by the som_map() function

## Value

A tibble stating the purity for each cluster

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
