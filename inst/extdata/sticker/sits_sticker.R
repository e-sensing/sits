library(hexSticker)

image <- "/users/gilbertocamara/sits/inst/extdata/plot/pasture_ndvi.png"

rfor_model <- sits_train(samples_mt_4bands, sits_rfor())
# Classify using Random Forest model and plot the result
point_mt_4bands <- sits_select_bands(point_mt_6bands, ndvi, evi, nir, mir)
class.tb <- point_mt_4bands %>%
    sits_whittaker(lambda = 0.2, bands_suffix = "") %>%
    sits_classify(rfor_model)

p <- sits:::.sits_plot_classification(class.tb, bands = c("NDVI", "EVI"))

    plot(bands = c("ndvi", "evi"))

library(showtext)
sysfonts::font_add_google("IBM Plex Sans", "plexs")
sysfonts::font_add_google("Roboto", "rob")
hexSticker::sticker(p, package="sits", p_size=12,
                    s_x=1, s_y=.75, s_width = 2.0, s_height = 1.0,
                    p_family = "rob", h_color = "sienna4", h_fill = "sienna3", dpi = 1200,
        filename="/users/gilbertocamara/sits/inst/extdata/sticker/sits.png")
