# A script for using SVM together with the TWDTW alignements for classification of time series
# Gilberto Camara, revised 19.08.2017

#load the sits library
library (sits)
#load a data set for with samples for EMBRAPA data set
embrapa.tb <- sits_getdata(file = "inst/extdata/samples/dados_matogrosso_alex.json.gz")

embrapa.tb <- dplyr::filter (embrapa.tb, label != "Water")

bands <- c ("ndvi", "evi", "nir", "mir")

embrapa.tb <- sits_select (embrapa.tb, bands)


# test accuracy of TWDTW to measure distances
conf_svm.tb <- sits_kfold_validate(embrapa.tb, folds = 2,
                                   pt_method   = sits_gam(),
                                   dist_method = sits_TWDTW_distances(),
                                   tr_method   = sits_svm (cost = 1000, kernel = "radial"))

sits_save (conf_svm.tb, file = "/Users/gilberto/sits/inst/extdata/results/svm_original_labels.json")

# test accuracy of TWDTW to measure distances
conf_lda.tb <- sits_kfold_validate(embrapa.tb, folds = 2,
                                     pt_method   = sits_gam(),
                                     dist_method = sits_TWDTW_distances(),
                                     tr_method   = sits_lda ())

# print the accuracy of the SVM- 94%
print("==================================================")
print ("== Confusion Matrix = SVM =======================")
sits_save (conf_lda.tb, file = "/Users/gilberto/sits/inst/extdata/results/lda_original_labels.json")

# Linear Discriminant Analysis
conf_qda.tb <- sits_kfold_validate(embrapa.tb, folds = 2,
                                     pt_method   = sits_gam(),
                                     dist_method = sits_TWDTW_distances(),
                                     tr_method   = sits_qda())

# print the accuracy of the Linear Discriminant Analysis
print("==================================================")
print ("== Confusion Matrix = LDA =======================")
sits_save (conf_qda.tb, file = "/Users/gilberto/sits/inst/extdata/results/qda_original_labels.json")

# "multinomial log-linear (mlr)
conf_mlr.tb <- sits_kfold_validate(embrapa.tb, folds = 2,
                                    pt_method   = sits_gam(),
                                    dist_method = sits_TWDTW_distances(),
                                    tr_method   = sits_mlr())

# print the accuracy of the Multinomial log-linear
print("===============================================")
print ("== Confusion Matrix = MLR =======================")
sits_save (conf_mlr.tb, file = "/Users/gilberto/sits/inst/extdata/results/mlr_original_labels.json")

# generalized liner model (glm)
conf_glm.tb <- sits_kfold_validate(embrapa.tb, folds = 2,
                                   pt_method   = sits_gam(),
                                   dist_method = sits_TWDTW_distances(),
                                   tr_method   = sits_glm())

# print the accuracy of the generalized liner model (glm)
print("===============================================")
print ("== Confusion Matrix = GLM  =======================")
sits_save (conf_glm.tb, file = "/Users/gilberto/sits/inst/extdata/results/glm_original_labels.json")

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

embrapa2.tb <- sits_relabel (embrapa.tb)
# test accuracy of TWDTW to measure distances
conf_svm2.tb <- sits_kfold_validate(embrapa2.tb, folds = 2,
                                   pt_method   = sits_gam(),
                                   dist_method = sits_TWDTW_distances(),
                                   tr_method   = sits_svm (cost = 1000, kernel = "radial"))

sits_save (conf_svm2.tb, file = "/Users/gilberto/sits/inst/extdata/results/svm_relabel1.json")

# test accuracy of TWDTW to measure distances
conf_lda2.tb <- sits_kfold_validate(embrapa2.tb, folds = 2,
                                   pt_method   = sits_gam(),
                                   dist_method = sits_TWDTW_distances(),
                                   tr_method   = sits_lda ())

# print the accuracy of the SVM- 94%
print("==================================================")
print ("== Confusion Matrix = SVM =======================")
sits_save (conf_lda2.tb, file = "/Users/gilberto/sits/inst/extdata/results/lda_relabel1.json")

# Linear Discriminant Analysis
conf_qda2.tb <- sits_kfold_validate(embrapa2.tb, folds = 2,
                                   pt_method   = sits_gam(),
                                   dist_method = sits_TWDTW_distances(),
                                   tr_method   = sits_qda())

# print the accuracy of the Linear Discriminant Analysis
print("==================================================")
print ("== Confusion Matrix = LDA =======================")
sits_save (conf_qda2.tb, file = "/Users/gilberto/sits/inst/extdata/results/qda_relabel1.json")

# "multinomial log-linear (mlr)
conf_mlr2.tb <- sits_kfold_validate(embrapa2.tb, folds = 2,
                                   pt_method   = sits_gam(),
                                   dist_method = sits_TWDTW_distances(),
                                   tr_method   = sits_mlr())

# print the accuracy of the Multinomial log-linear
print("===============================================")
print ("== Confusion Matrix = MLR =======================")
sits_save (conf_mlr2.tb, file = "/Users/gilberto/sits/inst/extdata/results/mlr_relabel1.json")

# generalized liner model (glm)
conf_glm2.tb <- sits_kfold_validate(embrapa2.tb, folds = 2,
                                   pt_method   = sits_gam(),
                                   dist_method = sits_TWDTW_distances(),
                                   tr_method   = sits_glm())

# print the accuracy of the generalized liner model (glm)
print("===============================================")
print ("== Confusion Matrix = GLM  =======================")
sits_save (conf_glm2.tb, file = "/Users/gilberto/sits/inst/extdata/results/glm_relabel1.json")

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

embrapa3.tb <- sits_relabel (embrapa.tb)

# test accuracy of TWDTW to measure distances
conf_svm3.tb <- sits_kfold_validate(embrapa3.tb, folds = 2,
                                    pt_method   = sits_gam(),
                                    dist_method = sits_TWDTW_distances(),
                                    tr_method   = sits_svm (cost = 1000, kernel = "radial"))

sits_save (conf_svm2.tb, file = "/Users/gilberto/sits/inst/extdata/results/svm_relabel2.json")

# test accuracy of TWDTW to measure distances
conf_lda3.tb <- sits_kfold_validate(embrapa3.tb, folds = 2,
                                    pt_method   = sits_gam(),
                                    dist_method = sits_TWDTW_distances(),
                                    tr_method   = sits_lda ())

# print the accuracy of the SVM- 94%
print("==================================================")
print ("== Confusion Matrix = SVM =======================")
sits_save (conf_lda3.tb, file = "/Users/gilberto/sits/inst/extdata/results/lda_relabel2.json")

# Linear Discriminant Analysis
conf_qda3.tb <- sits_kfold_validate(embrapa3.tb, folds = 2,
                                    pt_method   = sits_gam(),
                                    dist_method = sits_TWDTW_distances(),
                                    tr_method   = sits_qda())

# print the accuracy of the Linear Discriminant Analysis
print("==================================================")
print ("== Confusion Matrix = LDA =======================")
sits_save (conf_qda3.tb, file = "/Users/gilberto/sits/inst/extdata/results/qda_relabel2.json")

# "multinomial log-linear (mlr)
conf_mlr3.tb <- sits_kfold_validate(embrapa3.tb, folds = 2,
                                    pt_method   = sits_gam(),
                                    dist_method = sits_TWDTW_distances(),
                                    tr_method   = sits_mlr())

# print the accuracy of the Multinomial log-linear
print("===============================================")
print ("== Confusion Matrix = MLR =======================")
sits_save (conf_mlr3.tb, file = "/Users/gilberto/sits/inst/extdata/results/mlr_relabel2.json")

# generalized liner model (glm)
conf_glm3.tb <- sits_kfold_validate(embrapa3.tb, folds = 2,
                                    pt_method   = sits_gam(),
                                    dist_method = sits_TWDTW_distances(),
                                    tr_method   = sits_glm())

# print the accuracy of the generalized liner model (glm)
print("===============================================")
print ("== Confusion Matrix = GLM  =======================")
sits_save (conf_glm3.tb, file = "/Users/gilberto/sits/inst/extdata/results/glm_relabel2.json")

