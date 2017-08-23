# A script for using SVM together with the TWDTW alignements for classification of time series
# Gilberto Camara, revised 19.08.2017

#load the sits library
library (sits)
#load a data set for with samples for EMBRAPA data set
embrapa.tb <- sits_getdata(file = "/Users/gilbertocamara/sits/inst/extdata/samples/embrapa_damien_gilberto.json")

# test accuracy of TWDTW to measure distances
conf_svm.tb <- sits_kfold_validate(embrapa.tb, folds = 2,
                                     pt_method   = sits_gam(),
                                     dist_method = sits_TWDTW_distances(),
                                     tr_method   = sits_svm (cost = 100, method = "radial"))

# print the accuracy of the SVM- 94%
print("==================================================")
print ("== Confusion Matrix = SVM =======================")
sits_accuracy(conf_svm.tb)

# Linear Discriminant Analysis
conf_lda.tb <- sits_kfold_validate(embrapa.tb, folds = 2,
                                     pt_method   = sits_gam(),
                                     dist_method = sits_TWDTW_distances(),
                                     tr_method   = sits_lda())

# print the accuracy of the Linear Discriminant Analysis
print("==================================================")
print ("== Confusion Matrix = LDA =======================")
sits_accuracy(conf_lda.tb)

# "multinomial log-linear (mlr)
conf_mlr.tb <- sits_kfold_validate(embrapa.tb, folds = 2,
                                    pt_method   = sits_gam(),
                                    dist_method = sits_TWDTW_distances(),
                                    tr_method   = sits_mlr())

# print the accuracy of the Multinomial log-linear
print("===============================================")
print ("== Confusion Matrix = MLR =======================")
sits_accuracy(conf_mlr.tb)

# generalized liner model (glm)
conf_glm.tb <- sits_kfold_validate(embrapa.tb, folds = 2,
                                   pt_method   = sits_gam(),
                                   dist_method = sits_TWDTW_distances(),
                                   tr_method   = sits_glm())

# print the accuracy of the generalized liner model (glm)
print("===============================================")
print ("== Confusion Matrix = GLM  =======================")
sits_accuracy(conf_glm.tb)

# Random Forest (rfor)
conf_rfor.tb <- sits_kfold_validate(embrapa.tb, folds = 2,
                                   pt_method   = sits_gam(),
                                   dist_method = sits_TWDTW_distances(),
                                   tr_method   = sits_rfor())

# Random Forest (rfor)
print("===============================================")
print ("== Confusion Matrix = RFOR  =======================")
sits_accuracy(conf_rfor.tb)
