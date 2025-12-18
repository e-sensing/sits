# List the cloud collections supported by sits

Prints the collections available in each cloud service supported by
sits. Users can select to get information only for a single service by
using the `source` parameter.

## Usage

``` r
sits_list_collections(source = NULL)
```

## Arguments

- source:

  Data source to be shown in detail.

## Value

Prints collections available in each cloud service supported by sits.

## Examples

``` r
if (sits_run_examples()) {
    # show the names of the colors supported by SITS
    sits_list_collections()
}
```
