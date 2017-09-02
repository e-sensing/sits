library (sits)
#load a data set for with samples for EMBRAPA data set
embrapa.tb <- sits_getdata(file = "./inst/extdata/samples/dados_matogrosso_alex_v2.json.gz")

# test accuracy of TWDTW to measure distances
conf_glm.tb <- sits_kfold_validate(embrapa.tb, folds = 2,
                                   pt_method   = sits_gam(),
                                   dist_method = sits_TWDTW_dist_bands(),
                                   tr_method   = sits_glm ())
print("==================================================")
print ("== Confusion Matrix = GLM =======================")
glm.mx <- sits_accuracy(conf_glm.tb)

sits_accuracy_save(glm.mx, "./inst/extdata/results/embrapa_9classes_lasso_multiple_bands")


# test accuracy of TWDTW to measure distances
conf_svm.tb <- sits_kfold_validate(embrapa.tb, folds = 5,
                                   pt_method   = sits_gam(),
                                   dist_method = sits_TWDTW_dist_bands(),
                                   tr_method   = sits_svm(cost = 10, kernel = "radial",
                                                          tolerance = 0.001, epsilon = 0.1))
print("==================================================")
print ("== Confusion Matrix = GVM =======================")
conf_svm.mx <- sits_accuracy(conf_svm.tb)

sits_accuracy_save(conf_svm.mx, "./inst/extdata/results/embrapa_9classes_svm_radial_cost10_distance_bands")

# test accuracy of TWDTW to measure distances
conf_svm2.tb <- sits_kfold_validate(embrapa.tb, folds = 5,
                                   pt_method   = sits_gam(),
                                   dist_method = sits_TWDTW_dist_bands(),
                                   tr_method   = sits_svm(cost = 100, kernel = "radial",
                                                          tolerance = 0.001, epsilon = 0.1))
print("==================================================")
print ("== Confusion Matrix = GVM =======================")
conf_svm2.mx <- sits_accuracy(conf_svm2.tb)

sits_accuracy_save(conf_svm2.mx, "./inst/extdata/results/svm_radial_cost100_distance_bands")

# test accuracy of TWDTW to measure distances
conf_svm.tb <- sits_kfold_validate(embrapa.tb, folds = 5,
                                   pt_method   = sits_gam(),
                                   dist_method = sits_TWDTW_dist_bands(),
                                   tr_method   = sits_svm(cost = 10, kernel = "radial",
                                                          tolerance = 0.001, epsilon = 0.1))
print("==================================================")
print ("== Confusion Matrix = GVM =======================")
conf_svm.mx <- sits_accuracy(conf_svm.tb)

sits_accuracy_save(conf_svm.mx, "./inst/extdata/results/embrapa_10classes_svm_multiple_bands_c10000_radial_eps_0.1")



# =======================================================

patterns.tb <- sits_gam (embrapa.tb)
distances.tb <- sits_TWDTW_dist_bands(embrapa.tb, patterns.tb)

predictors_index <-  -2:0
# get predictors names
categories <- names(distances.tb)[c(predictors_index)]

# compute formula result
formula1 <- stats::as.formula(paste0("factor(reference)~", paste0(paste0('log(`', categories, '`)'), collapse = "+")))

svm.ml <-  e1071::svm (formula = formula1, data = distances.tb, kernel = "radial",
                     degree = 3, coef0 = 0, cost = 10,
                     tolerance = 0.001, epsilon = 0.1)

predict.tb <- stats::predict(svm.ml, newdata = distances.tb)

conf.tb <- tibble::tibble("predicted" = predict.tb, "reference" = distances.tb$reference)

sits_accuracy(conf.tb)

tuneResult <- e1071::tune(svm, train.x = formula1,  data = distances.tb,
                   kernel = "radial", degree = 3, coef0 = 0, tolerance = 0.001,
                   ranges = list(epsilon = seq(0,1,0.2), cost = 10^(1:4)))

print(tuneResult)
# Draw the tuning graph
plot(tuneResult)

tuneResult2 <- e1071::tune(svm, train.x = formula1,  data = distances.tb,
                          kernel = "linear", degree = 3, coef0 = 0, tolerance = 0.001,
                          ranges = list(epsilon = seq(0,0.1,0.02), cost = seq(0.5,4,0.5)))


# test accuracy of TWDTW to measure distances
conf_svm.tb <- sits_kfold_validate(embrapa.tb, folds = 5,
                                   pt_method   = sits_gam(),
                                   dist_method = sits_TWDTW_dist_bands(),
                                   tr_method   = sits_svm(cost = 100, kernel = "radial"))
print("==================================================")
print ("== Confusion Matrix = GVM =======================")
conf_svm.mx <- sits_accuracy(conf_svm.tb)

sits_accuracy_save(conf_svm.mx, "./inst/extdata/results/embrapa_10classes_svm_multiple_bands_c10000_radial_eps_0.1")

# test accuracy of TWDTW to measure distances
conf_svm_1.tb <- sits_kfold_validate(embrapa.tb, folds = 2,
                                   pt_method   = sits_gam(),
                                   dist_method = sits_TWDTW_dist_bands(),
                                   tr_method   = sits_svm(cost = 1, kernel = "radial"))
print("==================================================")
print ("== Confusion Matrix = GVM =======================")
conf_svm_1.mx <- sits_accuracy(conf_svm_1.tb)

sits_accuracy_save(conf_svm.mx, "./inst/extdata/results/embrapa_10classes_svm_multiple_bands_c10000_radial_eps_0.1")


# test accuracy of TWDTW to measure distances
conf_glm.tb <- sits_kfold_validate(embrapa.tb, folds = 5,
                                   pt_method   = sits_gam(),
                                   dist_method = sits_TWDTW_dist_bands(),
                                   tr_method   = sits_glm ())
print("==================================================")
print ("== Confusion Matrix = GLM =======================")
glm.mx <- sits_accuracy(conf_glm.tb)

sits_accuracy_save(gm.mx, "./inst/extdata/results/embrapa_10classes_lasso_multiple_bands")

