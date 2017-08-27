# Reads different confusion matrices stored in JSON.GZ files and saves the results in CSV

library (sits)

json_files <- list.files ("./inst/extdata/results/", pattern= "json", full.names = TRUE)

for (i in 1:length(json_files)) {

    conf.data <- sits_conf_fromGZ(json_files[i])

    conf.mx <- sits_accuracy(conf.data)

    prefix <- unlist(strsplit(json_files[i], split =".json.gz"))

    sits_accuracy_save(conf.mx, prefix)
}

newlabels1.lst <- tibble::lst (
    "Fallow_Cotton"   = "Fallow_Cotton",
    "Cerrado"         = "Cerrado",
    "Forest"          = "Forest",
    "Pasture"         = "Pasture",
    "Soy_Cotton"      = "Soy_Cotton",
    "Soy_Coverage"    = "Soy_Coverage",
    "Soy_Crotalaria"  = "Soy_Coverage",
    "Soy_Beans"       = "Soy_Coverage",
    "Soy_Sunflower"   = "Soy_Coverage",
    "Soy_Millet"      = "Soy_Coverage",
    "Soy_Corn"        = "Soy_Corn",
    "Soy_Pasture"     = "Soy_Coverage",
    "Soy_Fallow"      = "Soy_Fallow",
    "Soy_Sorghum"     = "Soy_Coverage")

newlabels2.lst <- tibble::lst (
    "Fallow_Cotton"   = "Fallow_Cotton",
    "Cerrado"         = "Cerrado",
    "Forest"          = "Forest",
    "Pasture"         = "Pasture",
    "Soy_Cotton"      = "Soy_Cotton",
    "Soy_Coverage"    = "Soy_Coverage",
    "Soy_Crotalaria"  = "Soy_Coverage",
    "Soy_Beans"       = "Soy_Coverage",
    "Soy_Sunflower"   = "Soy_Coverage",
    "Soy_Millet"      = "Soy_Coverage",
    "Soy_Corn"        = "Soy_Coverage",
    "Soy_Pasture"     = "Soy_Coverage",
    "Soy_Fallow"      = "Soy_Fallow",
    "Soy_Sorghum"     = "Soy_Coverage")

newlabels3.lst <- tibble::lst (
    "Fallow_Cotton"   = "Croplands",
    "Cerrado"         = "Cerrado",
    "Forest"          = "Forest",
    "Pasture"         = "Pasture",
    "Soy_Cotton"      = "Croplands",
    "Soy_Coverage"    = "Croplands",
    "Soy_Crotalaria"  = "Croplands",
    "Soy_Beans"       = "Croplands",
    "Soy_Sunflower"   = "Croplands",
    "Soy_Millet"      = "Croplands",
    "Soy_Corn"        = "Croplands",
    "Soy_Pasture"     = "Croplands",
    "Soy_Fallow"      = "Croplands",
    "Soy_Sorghum"     = "Croplands")

json_original <- list.files ("./inst/extdata/results/", pattern= ".json", full.names = TRUE)

for (i in 1:length(json_original)) {

    conf.data <- sits_conf_fromGZ(json_original[i])
    prefix <- unlist(strsplit(json_original[i], split =".json.gz"))

    conf1.mx <- sits_accuracy(conf.data,newlabels1.lst)
    prefix1 <- paste0 (prefix, "reclass_8classes")
    sits_accuracy_save(conf1.mx, prefix1)

    conf2.mx <- sits_accuracy(conf.data,newlabels2.lst)
    prefix2 <- paste0 (prefix, "reclass_7classes")
    sits_accuracy_save(conf2.mx, prefix2)

    conf3.mx <- sits_accuracy(conf.data,newlabels3.lst)
    prefix3 <- paste0 (prefix, "reclass_4classes")
    sits_accuracy_save(conf3.mx, prefix3)
}
