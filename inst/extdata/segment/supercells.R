library(supercells)    # superpixels for spatial data
library(terra)         # spatial raster data reading and handling
library(sf)            # spatial vector data reading and handling

# =====
data_dir <- "~/sitsbook/tempdir/chp5"

s2_cube <- sits_cube(
    source = "AWS",
    collection = "SENTINEL-2-L2A",
    bands = c(
        "B02", "B03", "B04",
        "B05", "B06", "B07",
        "B08", "B8A", "B11",
        "B12"
    ),
    data_dir = data_dir,
    parse_info = c("X1", "X2", "tile", "band", "date")
)

segments2 <- sits_supercells(
    cube = s2_cube,
    tiles = "20LKP",
    bands = c("B03", "B04", "B08", "B8A", "B11"),
    date = as.Date("2018-07-03"),
    step = 20,
    multicores = 1
)

plot(s2_cube,
     tile = "20LKP",
     red = "B11",
     green = "B8A",
     blue = "B03",
     segments = segments2,
     seg_color = "white"
)
