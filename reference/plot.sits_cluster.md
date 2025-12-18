# Plot a dendrogram cluster

Plot a dendrogram

## Usage

``` r
# S3 method for class 'sits_cluster'
plot(x, ..., cluster, cutree_height, palette)
```

## Arguments

- x:

  sits tibble with cluster indexes.

- ...:

  Further specifications for
  [plot](https://e-sensing.github.io/sits/reference/plot.md).

- cluster:

  cluster object produced by \`sits_cluster\` function.

- cutree_height:

  dashed horizontal line to be drawn indicating the height of dendrogram
  cutting.

- palette:

  HCL color palette.

## Value

The dendrogram object.

## Author

Rolf Simoes, <rolfsimoes@gmail.com>

## Examples

``` r
if (sits_run_examples()) {
    samples <- sits_cluster_dendro(cerrado_2classes,
        bands = c("NDVI", "EVI")
    )
}
```
