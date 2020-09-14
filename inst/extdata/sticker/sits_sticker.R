library(hexSticker)

image <- "/users/gilbertocamara/sits/inst/extdata/plot/pasture_ndvi.png"

library(showtext)
sysfonts::font_add_google("IBM Plex Sans", "plexs")
sysfonts::font_add_google("Roboto", "rob")
hexSticker::sticker(image, package="sits", p_size=16, s_x=1, s_y=.75, s_width=.6,
                    p_family = "rob", h_color = "sienna4", h_fill = "sienna3", dpi = 1200,
        filename="/users/gilbertocamara/sits/inst/extdata/plot/imgfile.png")
