library (sits)
#load a data set for with samples for EMBRAPA data set
embrapa.tb <- sits_getdata(file = "./inst/extdata/samples/embrapa_10classes.json.gz")

# test accuracy of TWDTW to measure distances
conf_glm.tb <- sits_kfold_validate(embrapa.tb, folds = 5,
                                   pt_method   = sits_gam(),
                                   dist_method = sits_TWDTW_dist_bands(),
                                   tr_method   = sits_glm ())
print("==================================================")
print ("== Confusion Matrix = GLM =======================")
glm.mx <- sits_accuracy(conf_glm.tb)

sits_accuracy_save(glm.mx, "./inst/extdata/results/embrapa_10classes_lasso_multiple_bands")

