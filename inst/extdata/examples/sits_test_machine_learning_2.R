library (sits)

# get the data from embrapa
embrapa.tb <- sits_getdata(file = system.file("extdata/samples/embrapa_cerrado_forest.json", package="sits"))
# select the bands
bands <-  c("ndvi", "evi", "nir")
embrapa.tb <- sits_select(embrapa.tb, bands)


#get the patterns from Damien
matogrosso.tb <- sits_getdata(file = system.file("extdata/samples/matogrosso.json", package="sits"))

labels_embrapa <- dplyr::distinct(embrapa.tb, label)
labels_damien  <- dplyr::distinct(matogrosso.tb, label)

# Relabel embrapa data

newlabels1.lst <- tibble::lst (
    "Algodao_Pousio"  = "Fallow_Cotton",
    "Cerrado"         = "Cerrado",
    "Forest"          = "Forest",
    "Pasture"         = "Pasture",
    "Pasto Limpo"     = "Pasture",
    "Pasto Sujo"      = "Pasture",
    "Reflorestamento" = "Planted_Forest",
    "Soja_Algodao"    = "Soy_Cotton",
    "Soja_Cobertura"  = "Soy_Other",
    "Soja_Crotalaria" = "Soy_Other",
    "Soja_Feijao"     = "Soy_Other",
    "Soja_Girassol"   = "Soy_Other",
    "Soja_Milheto"    = "Soy_Other",
    "Soja_Milho"      = "Soy_Corn",
    "Soja_Pasto"      = "Soy_Other",
    "Soja_Pousio"     = "Soy_Fallow",
    "Soja_Sorgo"      = "Soy_Other")

#relabel the data
embrapa1.tb <- sits_relabel(embrapa.tb, newlabels1.lst)

#remove soy_fallow and Planted Forest from embrapa patterns
embrapa1.tb <- dplyr::filter(embrapa1.tb, label != "Soy_Fallow")
embrapa1.tb <- dplyr::filter(embrapa1.tb, label != "Planted_Forest")

# include soy fallow from Damien
soy_fallow.tb <- dplyr::filter(matogrosso.tb, label == "Soybean_Fallow2")
newlabels2.lst <- tibble::lst ("Soybean_Fallow2" = "Soy_Fallow")
soy_fallow.tb <-  sits_relabel (soy_fallow.tb, newlabels2.lst)
embrapa2.tb <- dplyr::bind_rows(embrapa1.tb, soy_fallow.tb)

# validate embrapa2.tb

conf5.tb <- sits_kfold_validate(embrapa2.tb, tr_method = sits_svm (cost = 1000, kernel = "radial"), folds = 5, multicores = 5)

sits_accuracy(conf5.tb)
