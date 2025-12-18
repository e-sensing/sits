# Removes labels that are minority in each cluster.

Takes a tibble with time series that has an additional \`cluster\`
produced by
[`sits_cluster_dendro()`](https://e-sensing.github.io/sits/reference/sits_cluster_dendro.md)
and removes labels that are minority in each cluster.

## Usage

``` r
sits_cluster_clean(samples)
```

## Arguments

- samples:

  Tibble with set of time series with additional cluster information
  produced by
  [`sits_cluster_dendro()`](https://e-sensing.github.io/sits/reference/sits_cluster_dendro.md)

## Value

Tibble with time series (class "sits")

## Author

Rolf Simoes, <rolfsimoes@gmail.com>

## Examples

``` r
if (sits_run_examples()) {
    clusters <- sits_cluster_dendro(cerrado_2classes)
    freq1 <- sits_cluster_frequency(clusters)
    freq1
    clean_clusters <- sits_cluster_clean(clusters)
    freq2 <- sits_cluster_frequency(clean_clusters)
    freq2
}
```
