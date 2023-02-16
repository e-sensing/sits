library(sits)
# Open mask map
data_dir <- system.file("extdata/PRODES", package = "sitsdata")
prodes2021 <- sits_cube(
    source = "USGS",
    collection = "LANDSAT-C2L2-SR",
    data_dir = data_dir,
    parse_info = c("X1", "X2", "tile", "start_date", "end_date",
                   "band", "version"),
    bands = "class",
    labels = c("Forest", "Water", "NonForest",
               "NonForest2", "NoClass", "d2007", "d2008",
               "d2009", "d2010", "d2011", "d2012",
               "d2013", "d2014", "d2015", "d2016",
               "d2017", "d2018", "r2010", "r2011",
               "r2012", "r2013", "r2014", "r2015",
               "r2016", "r2017", "r2018", "d2019",
               "r2019", "d2020", "NoClass", "r2020",
               "Clouds2021", "d2021", "r2021")
)
colors <- grDevices::hcl.colors(n = 15, palette = "YlOrBr")
# define the legend for the deforestation map
def_legend <- c(
    "Forest" = "forestgreen", "Water" = "dodgerblue3",
    "NonForest" = "gray", "NonForest2" = "gray",
    "d2007" = colors[1],  "d2008" = colors[2],
    "d2009" = colors[3],  "d2010" = colors[4],
    "d2011" = colors[5],  "d2012" = colors[6],
    "d2013" = colors[7],  "d2014" = colors[8],
    "d2015" = colors[9],  "d2016" = colors[10],
    "d2017" = colors[11], "d2018" = colors[12],
    "d2019" = colors[13], "d2020" = colors[14],
    "d2021" = colors[15], "r2010" = "azure2",
    "r2011" = "azure2",   "r2012" = "azure2",
    "r2013" = "azure2",   "r2014" = "azure2",
    "r2015" = "azure2",   "r2016" = "azure2",
    "r2017" = "azure2",   "r2018" = "azure2",
    "r2019" = "azure2",   "r2020" = "azure2",
    "r2021" = "azure2",   "NoClass" = "grey90",
    "Clouds2021" = "grey90"
)
plot(prodes2021, legend = def_legend)

# Open classification map
data_dir <- system.file("extdata/Rondonia-Class", package = "sitsdata")
ro_class <- sits_cube(
    source = "MPC",
    collection = "SENTINEL-2-L2A",
    data_dir = data_dir,
    parse_info = c("X1", "X2", "tile", "start_date", "end_date",
                   "band", "version"),
    bands = "class",
    labels = c("Water", "ClearCut_Burn", "ClearCut_Soil",
               "ClearCut_Veg", "Forest", "Bare_Soil", "Wetland")
)

# Reclassify cube
ro_mask <- sits_reclassify(
    cube = ro_class,
    mask = prodes2021,
    rules = list(
        "Defor_2020" = mask %in% c(
            "d2007", "d2008", "d2009",
            "d2010", "d2011", "d2012",
            "d2013", "d2014", "d2015",
            "d2016", "d2017", "d2018",
            "r2010", "r2011", "r2012",
            "r2013", "r2014", "r2015",
            "r2016", "r2017", "r2018",
            "d2019", "r2019", "d2020",
            "r2020", "r2021"
        ),
        "Water" = mask == "Water",
        "NonForest" = mask %in% c("NonForest", "NonForest2")
    ),
    memsize = 8,
    multicores = 2,
    output_dir = "~/"
)
sits_labels(ro_mask)

mask_legend = c(
    "Defor_2020" = "azure4"
)
plot(ro_mask, legend = mask_legend)
