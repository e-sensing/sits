# Evaluate cluster

Remove samples from a given class inside a neuron of another class

## Usage

``` r
sits_som_remove_samples(som_map, som_eval, class_cluster, class_remove)
```

## Arguments

- som_map:

  A SOM map produced by the som_map() function

- som_eval:

  An evaluation produced by the som_eval() function

- class_cluster:

  Dominant class of a set of neurons

- class_remove:

  Class to be removed from the neurons of "class_cluster"

## Value

A new set of samples with the desired class neurons remove

## Author

Lorena Alves, <lorena.santos@inpe.br>

Karine Ferreira. <karine.ferreira@inpe.br>

Gilberto Camara, <gilberto.camara@inpe.br>

## Examples

``` r
if (sits_run_examples()) {
    # create a som map
    som_map <- sits_som_map(samples_modis_ndvi)
    # evaluate the som map and create clusters
    som_eval <- sits_som_evaluate_cluster(som_map)
    # clean the samples
    new_samples <- sits_som_remove_samples(
        som_map, som_eval,
        "Pasture", "Cerrado"
    )
}
```
