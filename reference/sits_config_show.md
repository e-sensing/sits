# Show current sits configuration

Prints the current sits configuration options. To show specific
configuration options for a source, a collection, or a palette, users
can inform the corresponding keys to `source` and `collection`.

## Usage

``` r
sits_config_show()
```

## Value

No return value, called for side effects.

## Examples

``` r
sits_config_show()
#> Data sources and user configurable parameters in sits
#> 
#> Data sources available in sits
#> AWS, BDC, CDSE-OS, CDSE, DCCHILE, DEAFRICA, DEAUSTRALIA, HLS, MPC, OGH, PLANET, SDC, TERRASCOPE, USGS
#> 
#> Use sits_list_collections(<source>) to get info for each source
#> 
#> User configurable parameters for plotting
#> max_size: 1200
#> midpoint: NA
#> graticules_labels_size: 0.7
#> legend_title_size: 0.7
#> legend_text_size: 0.7
#> legend_bg_color: white
#> legend_bg_alpha: 0.7
#> legend_width: 1
#> legend_position: inside
#> legend_height: 1
#> legend_title: values
#> scale: 1
#> font_family: sans
#> 
#> User configurable parameters for visualisation
#> []
#> 
#> Use sits_config_user_file() to create a user configuration file
```
