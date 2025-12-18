# Show label frequency in each cluster produced by dendrogram analysis

Show label frequency in each cluster produced by dendrogram analysis

## Usage

``` r
sits_cluster_frequency(samples)
```

## Arguments

- samples:

  Tibble with input set of time series with additional cluster
  information produced by
  [`sits_cluster_dendro()`](https://e-sensing.github.io/sits/reference/sits_cluster_dendro.md).

## Value

A matrix containing frequencies of labels in clusters.

## Author

Rolf Simoes, <rolfsimoes@gmail.com>

## Examples

``` r
if (sits_run_examples()) {
    clusters <- sits_cluster_dendro(cerrado_2classes)
    freq <- sits_cluster_frequency(clusters)
    freq
}
```
