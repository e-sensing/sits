# Find clusters in time series samples

These functions support hierarchical agglomerative clustering in sits.
They provide support from creating a dendrogram and using it for
cleaning samples.

`sits_cluster_dendro()` takes a tibble with time series and produces a
sits tibble with an added "cluster" column. The function first
calculates a dendrogram and obtains a validity index for best clustering
using the adjusted Rand Index. After cutting the dendrogram using the
chosen validity index, it assigns a cluster to each sample.

[`sits_cluster_frequency()`](https://e-sensing.github.io/sits/reference/sits_cluster_frequency.md)
computes the contingency table between labels and clusters and produces
a matrix. Its input is a tibble produced by `sits_cluster_dendro()`.

[`sits_cluster_clean()`](https://e-sensing.github.io/sits/reference/sits_cluster_clean.md)
takes a tibble with time series that has an additional \`cluster\`
produced by `sits_cluster_dendro()` and removes labels that are minority
in each cluster.

## Usage

``` r
sits_cluster_dendro(
  samples,
  bands = NULL,
  dist_method = "dtw_basic",
  linkage = "ward.D2",
  k = NULL,
  palette = "RdYlGn",
  ...
)
```

## Arguments

- samples:

  Tibble with input set of time series (class "sits").

- bands:

  Bands to be used in the clustering (character vector)

- dist_method:

  One of the supported distances (single char vector) "dtw": DTW with a
  Sakoe-Chiba constraint. "dtw2": DTW with L2 norm and Sakoe-Chiba
  constraint. "dtw_basic": A faster DTW with less functionality. "lbk":
  Keogh's lower bound for DTW. "lbi": Lemire's lower bound for DTW.

- linkage:

  Agglomeration method to be used (single char vector) One of "ward.D",
  "ward.D2", "single", "complete", "average", "mcquitty", "median" or
  "centroid".

- k:

  Desired number of clusters (overrides default value)

- palette:

  Color palette as per \`grDevices::hcl.pals()\` function.

- ...:

  Additional parameters to be passed to dtwclust::tsclust() function.

## Value

Tibble with "cluster" column (class "sits_cluster").

## Note

Please refer to the sits documentation available in
<https://e-sensing.github.io/sitsbook/> for detailed examples.

## References

"dtwclust" R package.

## Author

Rolf Simoes, <rolfsimoes@gmail.com>

## Examples

``` r
if (sits_run_examples()) {
    # default
    clusters <- sits_cluster_dendro(cerrado_2classes)
    # with parameters
    clusters <- sits_cluster_dendro(cerrado_2classes,
        bands = "NDVI", k = 5
    )
}
```
