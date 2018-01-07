library (sits)

MT_modcol6 <- readRDS("./inst/extdata/samples/embrapa_damien_rodrigo_michelle_mod13q1_col6.rds")

data("samples_MT_9classes")

pasture_col5.tb <- dplyr::filter (samples_MT_9classes, label == "Pasture")

conv.lst = list (
    "Cerrado_Campo" = "Cerrado_Campo",
    "Cerrado_Rupestre" = "Cerrado_Rupestre",
    "Cerrado.1" = "Cerrado",
    "Cerrado.2" = "Cerrado",
    "Corn_Cotton" = "Corn_Cotton",
    "Fallow_Cotton.1" = "Fallow_Cotton",
    "Fallow_Cotton.2" = "Fallow_Cotton",
    "Forest" = "Forest",
    "Millet_Cotton" = "Millet_Cotton",
    "Pasture" = "Pasture",
    "Soy_Corn.1" = "Soy_Corn",
    "Soy_Corn.2" = "Soy_Corn",
    "Soy_Cotton.1" = "Soy_Cotton",
    "Soy_Cotton.2" = "Soy_Cotton",
    "Soy_Fallow" = "Soy_Fallow",
    "Soy_Millet" = "Soy_Millet",
    "Soy_Sunflower" = "Soy_Sunflower"
)

MT_col6_13classes <- sits_relabel(MT_modcol6, conv.lst)

sits_labels (MT_col6_13classes)

MT_pasture_col6 <- dplyr::filter (MT_col6_13classes, label == "Pasture")

Cerrado_col6_nopasture <- dplyr::filter (MT_col6_13classes, label != "Pasture")

Cerrado_col6_pasture <- sits_tibble()

locs <- dplyr::distinct (pasture_col5.tb, longitude, latitude)
locs %>%
    purrrlyr::by_row(function (r){
        long = as.double (r$longitude)
        lat  = as.double (r$latitude)
        # filter only those rows with the same label
        rows <- dplyr::filter (MT_pasture_col6, longitude == long, latitude == lat)
        Cerrado_col6_pasture <<- dplyr::bind_rows(Cerrado_col6_pasture, rows)
    })
