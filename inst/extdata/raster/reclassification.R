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

# Open classification map
data_dir <- system.file("extdata/Rondonia-Class", package = "sitsdata")
ro_class <- sits_cube(
    source = "MPC",
    collection = "SENTINEL-2-L2A",
    data_dir = data_dir,
    parse_info = c("X1", "X2", "tile", "start_date", "end_date",
                   "band", "version"),
    bands = "class",
    labels = c("Water", "ClearCut_Burn", "ClearCut_BareSoil",
               "ClearCut_Veg", "Forest", "Bare_Soil", "Wetlands")
)

# Reclassify cube
ro_mask <- sits_reclassify(
    cube = ro_class,
    mask = prodes2021,
    rules = list(
        "Deforestation_Mask" = mask %in% c(
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
    memsize = 1,
    multicores = 1,
    output_dir = "~/"
)

plot(ro_mask)
