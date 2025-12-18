# Plot confusion between clusters

Plot a bar graph with informations about each cluster. The percentage of
mixture between the clusters.

## Usage

``` r
# S3 method for class 'som_evaluate_cluster'
plot(x, y, ..., name_cluster = NULL, title = "Confusion by cluster")
```

## Arguments

- x:

  Object of class "plot.som_evaluate_cluster".

- y:

  Ignored.

- ...:

  Further specifications for
  [plot](https://e-sensing.github.io/sits/reference/plot.md).

- name_cluster:

  Choose the cluster to plot.

- title:

  Title of plot.

## Value

A plot object produced by the ggplot2 package containing color bars
showing the confusion between classes.

## Author

Lorena Santos <lorena.santos@inpe.br>

## Examples

``` r
if (sits_run_examples()) {
    # create a SOM map
    som_map <- sits_som_map(samples_modis_ndvi)
    # evaluate the SOM cluster
    som_clusters <- sits_som_evaluate_cluster(som_map)
    # plot the SOM cluster evaluation
    plot(som_clusters)
}
```
