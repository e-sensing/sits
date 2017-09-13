library(wtss)
library(sits)

prodes.tb <- sits_getdata(system.file("extdata/samples/prodes_226_064.json.gz", package = "sits"))
prodes.tb <- prodes.tb %>% sits_linear_interp()

sits_summary(prodes.tb)

prodes.tb %>% sits_kfold_validate(folds = 5,
                                  pt_method = function(...) NULL,
                                  dist_method = sits_spread_time_series(),
                                  tr_method = sits_svm(formula = sits_formula_linear(),
                                                       kernel = "radial"),
                                  multicores = 5) %>% sits_accuracy()
# Confusion Matrix and Statistics
#
#           Reference
# Prediction ClearCut Forest Pasture
#   ClearCut      308      9      19
#   Forest         11    113       2
#   Pasture        18      2     124
#
# Overall Statistics
#
#  Accuracy : 0.8993
#    95% CI : (0.8726, 0.9221)
#
#     Kappa : 0.83
#
# Statistics by Class:
#
#                           Class: ClearCut Class: Forest Class: Pasture
# Prod Acc (Sensitivity)             0.9139        0.9113         0.8552
# Specificity                        0.8959        0.9730         0.9566
# User Acc (Pos Pred Value)          0.9167        0.8968         0.8611
# Neg Pred Value                     0.8926        0.9771         0.9545

patt_prodes.tb <- prodes.tb %>% sits_gam()
sits_plot_patterns(patt_prodes.tb)

dist_prodes.tb <- prodes.tb %>% sits_distances(patt_prodes.tb)


#    ___ __   __ __  __
#   / __|\ \ / /|  \/  |
#   \__ \ \ V / | |\/| |
#   |___/  \_/  |_|  |_|
#
model.svm <- sits_train(dist_prodes.tb, sits_svm(formula = sits_formula_linear(),
                                                 kernel = "radial"))
sits_predict(prodes.tb, dist_prodes.tb, model.svm) %>%
    dplyr::mutate(reference = label) %>% sits_accuracy()
# Confusion Matrix and Statistics
#
#           Reference
# Prediction ClearCut Forest Pasture
#   ClearCut      328      9      44
#   Forest          2    113       4
#   Pasture         7      2      97
#
# Overall Statistics
#
#  Accuracy : 0.8878
#    95% CI : (0.8599, 0.9118)
#
#     Kappa : 0.8026
#
# Statistics by Class:
#
#                           Class: ClearCut Class: Forest Class: Pasture
# Prod Acc (Sensitivity)             0.9733        0.9113         0.6690
# Specificity                        0.8030        0.9876         0.9805
# User Acc (Pos Pred Value)          0.8609        0.9496         0.9151
# Neg Pred Value                     0.9600        0.9774         0.9040
#


#    _     ___    _
#   | |   |   \  /_\
#   | |__ | |) |/ _ \
#   |____||___//_/ \_\
#
model.lda <- sits_train(dist_prodes.tb, sits_lda(formula = sits_formula_linear()))
sits_predict(prodes.tb, dist_prodes.tb, model.lda) %>%
    dplyr::mutate(reference = label) %>% sits_accuracy()
