# Function to show colors in SITS

Shows the default SITS colors

## Usage

``` r
sits_colors_show(legend = NULL, font_family = "sans")
```

## Arguments

- legend:

  One of the accepted legends in sits

- font_family:

  A font family loaded in SITS

## Value

no return, called for side effects

## Author

Gilberto Camara, <gilberto.camara@inpe.br>

## Examples

``` r
if (sits_run_examples()) {
    # show the colors supported by SITS
    sits_colors_show()
}
```
