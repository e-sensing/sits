# Function to retrieve sits color table

Returns the default color table.

## Usage

``` r
sits_colors(legend = NULL)
```

## Arguments

- legend:

  One of the accepted legends in sits

## Value

A tibble with color names and values

## Note

SITS has a predefined color palette with 238 class names. These colors
are grouped by typical legends used by the Earth observation community,
which include “IGBP”, “UMD”, “ESA_CCI_LC”, and “WORLDCOVER”. Use
[`sits_colors_show`](https://e-sensing.github.io/sits/reference/sits_colors_show.md)
to see a specific palette. The default color table can be extended using
[`sits_colors_set`](https://e-sensing.github.io/sits/reference/sits_colors_set.md).

## Author

Gilberto Camara, <gilberto.camara@inpe.br>

## Examples

``` r
if (sits_run_examples()) {
    # return the names of all colors supported by SITS
    sits_colors()
}
```
