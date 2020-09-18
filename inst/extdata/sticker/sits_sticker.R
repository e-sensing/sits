library(hexSticker)

image <- "/users/gilbertocamara/sits/inst/extdata/sticker/ts.png"


library(showtext)
sysfonts::font_add_google("IBM Plex Sans", "plexs")
sysfonts::font_add_google("Roboto", "rob")
hexSticker::sticker(image, package="sits", p_size=12,
                    s_x=1, s_y=.75, s_width = 0.2, s_height = 0.2,
                    p_family = "rob", h_color = "sienna4", h_fill = "sienna3", dpi = 1200,
        filename="/users/gilbertocamara/sits/inst/extdata/sticker/sits.png")
