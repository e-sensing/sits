library(wtss)
library(sits)

# load data and fill NAs with linear interpolation
prodes.tb <- sits_getdata(system.file("extdata/samples/prodes_226_064.json.gz", package = "sits"))
prodes.tb <- prodes.tb %>% sits_select_bands(c("ndvi", "evi", "nir"))
prodes.tb <- prodes.tb %>% sits_linear_interp()

# view a summary of the data
sits_summary(prodes.tb)

# create list of label conversion
conv.lst <- list("deforestation2014" = "clear_cut",
                 "deforestation2015" = "clear_cut",
                 "primary_forest" = "forest")

# create patterns and plot them
patt_prodes.tb <- prodes.tb %>% sits_gam()
sits_plot_patterns(patt_prodes.tb)

# create distances
dist_prodes.tb <- prodes.tb %>% sits_distances(patt_prodes.tb)


#    _____       _______  _______   _____  _____   _____  _______
#   |  __ \  /\ |__   __||__   __| |  __ \|_   _| / ____||__   __|
#   | |__) |/  \   | |      | |    | |  | | | |  | (___     | |
#   |  ___// /\ \  | |      | |    | |  | | | |   \___ \    | |
#   | |   / ____ \ | |      | | _  | |__| |_| |_  ____) |   | | _
#   |_|  /_/    \_\|_|      |_|(_) |_____/|_____||_____/    |_|(_)
#
#

#
#
#  ###################################################################



#    _____  _____  ____  _____  ____
#   |_   _|| ____|/ ___||_   _|/ ___|
#     | |  |  _|  \___ \  | |  \___ \
#     | |  | |___  ___) | | |   ___) |
#     |_|  |_____||____/  |_|  |____/
#
#

# start "one-shot" tests.


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
#                    Reference
# Prediction          deforestation2014 deforestation2015 primary_forest
#   deforestation2014               129                10              3
#   deforestation2015                16               176              3
#   primary_forest                    1                 5             68
#
# Overall Statistics
#
#  Accuracy : 0.9075
#    95% CI : (0.8753, 0.9337)
#
#     Kappa : 0.8519
#
# Statistics by Class:
#
#                           Class: deforestation2014 Class: deforestation2015 Class: primary_forest
# Prod Acc (Sensitivity)                      0.8836                   0.9215                0.9189
# Specificity                                 0.9509                   0.9136                0.9822
# User Acc (Pos Pred Value)                   0.9085                   0.9026                0.9189
# Neg Pred Value                              0.9368                   0.9306                0.9822

sits_predict(prodes.tb, dist_prodes.tb, model.svm) %>%
    dplyr::mutate(reference = label) %>% sits_accuracy(conv.lst)
# Confusion Matrix and Statistics
#
#            Reference
# Prediction  clear_cut forest
#   clear_cut       331      6
#   forest            6     68
#
#             Accuracy : 0.9708
#               95% CI : (0.9496, 0.9848)
#
#                Kappa : 0.9011
#
#  Prod Acc  clear_cut : 0.9822
#     Prod Acc  forest : 0.9189
#  User Acc  clear_cut : 0.9822
#     User Acc  forest : 0.9189


#    _     ___    _
#   | |   |   \  /_\
#   | |__ | |) |/ _ \
#   |____||___//_/ \_\
#
model.lda <- sits_train(dist_prodes.tb, sits_lda(formula = sits_formula_linear()))
sits_predict(prodes.tb, dist_prodes.tb, model.lda) %>%
    dplyr::mutate(reference = label) %>% sits_accuracy()
# Confusion Matrix and Statistics
#
#                    Reference
# Prediction          deforestation2014 deforestation2015 primary_forest
#   deforestation2014               103                30             11
#   deforestation2015                34               149              6
#   primary_forest                    9                12             57
#
# Overall Statistics
#
#  Accuracy : 0.7518
#    95% CI : (0.7071, 0.7929)
#
#     Kappa : 0.6046
#
# Statistics by Class:
#
#                           Class: deforestation2014 Class: deforestation2015 Class: primary_forest
# Prod Acc (Sensitivity)                      0.7055                   0.7801                0.7703
# Specificity                                 0.8453                   0.8182                0.9377
# User Acc (Pos Pred Value)                   0.7153                   0.7884                0.7308
# Neg Pred Value                              0.8390                   0.8108                0.9489

sits_predict(prodes.tb, dist_prodes.tb, model.lda) %>%
    dplyr::mutate(reference = label) %>% sits_accuracy(conv.lst)
# Confusion Matrix and Statistics
#
#            Reference
# Prediction  clear_cut forest
#   clear_cut       316     17
#   forest           21     57
#
#             Accuracy : 0.9075
#               95% CI : (0.8753, 0.9337)
#
#                Kappa : 0.6933
#
#  Prod Acc  clear_cut : 0.9377
#     Prod Acc  forest : 0.7703
#  User Acc  clear_cut : 0.9489
#     User Acc  forest : 0.7308


#     ___   ___    _
#    / _ \ |   \  /_\
#   | (_) || |) |/ _ \
#    \__\_\|___//_/ \_\
#
model.qda <- sits_train(dist_prodes.tb, sits_qda(formula = sits_formula_logref()))
sits_predict(prodes.tb, dist_prodes.tb, model.qda) %>%
    dplyr::mutate(reference = label) %>% sits_accuracy()
# Confusion Matrix and Statistics
#
#                    Reference
# Prediction          deforestation2014 deforestation2015 primary_forest
#   deforestation2014               113                20              7
#   deforestation2015                30               164              7
#   primary_forest                    3                 7             60
#
# Overall Statistics
#
#  Accuracy : 0.82
#    95% CI : (0.7793, 0.8559)
#
#     Kappa : 0.7101
#
# Statistics by Class:
#
#                           Class: deforestation2014 Class: deforestation2015 Class: primary_forest
# Prod Acc (Sensitivity)                      0.7740                   0.8586                0.8108
# Specificity                                 0.8981                   0.8318                0.9703
# User Acc (Pos Pred Value)                   0.8071                   0.8159                0.8571
# Neg Pred Value                              0.8782                   0.8714                0.9589

sits_predict(prodes.tb, dist_prodes.tb, model.qda) %>%
    dplyr::mutate(reference = label) %>% sits_accuracy(conv.lst)
# Confusion Matrix and Statistics
#
#            Reference
# Prediction  clear_cut forest
#   clear_cut       327     14
#   forest           10     60
#
#             Accuracy : 0.9416
#               95% CI : (0.9144, 0.9622)
#
#                Kappa : 0.798
#
#  Prod Acc  clear_cut : 0.9703
#     Prod Acc  forest : 0.8108
#  User Acc  clear_cut : 0.9589
#     User Acc  forest : 0.8571


#    __  __  _     ___
#   |  \/  || |   | _ \
#   | |\/| || |__ |   /
#   |_|  |_||____||_|_\
#
model.mlr <- sits_train(dist_prodes.tb, sits_mlr(formula = sits_formula_linear()))
sits_predict(prodes.tb, dist_prodes.tb, model.mlr) %>%
    dplyr::mutate(reference = label) %>% sits_accuracy()
# Confusion Matrix and Statistics
#
#                    Reference
# Prediction          deforestation2014 deforestation2015 primary_forest
#   deforestation2014               104                32              9
#   deforestation2015                37               150              7
#   primary_forest                    5                 9             58
#
# Overall Statistics
#
#  Accuracy : 0.7591
#    95% CI : (0.7148, 0.7997)
#
#     Kappa : 0.6138
#
# Statistics by Class:
#
#                           Class: deforestation2014 Class: deforestation2015 Class: primary_forest
# Prod Acc (Sensitivity)                      0.7123                   0.7853                0.7838
# Specificity                                 0.8453                   0.8000                0.9585
# User Acc (Pos Pred Value)                   0.7172                   0.7732                0.8056
# Neg Pred Value                              0.8421                   0.8111                0.9528

sits_predict(prodes.tb, dist_prodes.tb, model.mlr) %>%
    dplyr::mutate(reference = label) %>% sits_accuracy(conv.lst)
# Confusion Matrix and Statistics
#
#            Reference
# Prediction  clear_cut forest
#   clear_cut       323     16
#   forest           14     58
#
#             Accuracy : 0.927
#               95% CI : (0.8974, 0.9502)
#
#                Kappa : 0.7502
#
#  Prod Acc  clear_cut : 0.9585
#     Prod Acc  forest : 0.7838
#  User Acc  clear_cut : 0.9528
#     User Acc  forest : 0.8056


#    _       _    ___  ___   ___
#   | |     /_\  / __|/ __| / _ \
#   | |__  / _ \ \__ \\__ \| (_) |
#   |____|/_/ \_\|___/|___/ \___/
#
model.glm1 <- sits_train(dist_prodes.tb, sits_glm(alpha = 1))
get("result_glm", envir = environment(model.glm1))$lambda.min
# [1] 0.008022274

sits_predict(prodes.tb, dist_prodes.tb, model.glm1) %>%
    dplyr::mutate(reference = label) %>% sits_accuracy()
# Confusion Matrix and Statistics
#
#                    Reference
# Prediction          deforestation2014 deforestation2015 primary_forest
#   deforestation2014               101                27              8
#   deforestation2015                39               158              9
#   primary_forest                    6                 6             57
#
# Overall Statistics
#
#  Accuracy : 0.7689
#    95% CI : (0.725, 0.8088)
#
#     Kappa : 0.6268
#
# Statistics by Class:
#
#                           Class: deforestation2014 Class: deforestation2015 Class: primary_forest
# Prod Acc (Sensitivity)                      0.6918                   0.8272                0.7703
# Specificity                                 0.8679                   0.7818                0.9644
# User Acc (Pos Pred Value)                   0.7426                   0.7670                0.8261
# Neg Pred Value                              0.8364                   0.8390                0.9503

sits_predict(prodes.tb, dist_prodes.tb, model.glm1) %>%
    dplyr::mutate(reference = label) %>% sits_accuracy(conv.lst)
# Confusion Matrix and Statistics
#
#            Reference
# Prediction  clear_cut forest
#   clear_cut       325     17
#   forest           12     57
#
#             Accuracy : 0.9294
#               95% CI : (0.9002, 0.9522)
#
#                Kappa : 0.7546
#
#  Prod Acc  clear_cut : 0.9644
#     Prod Acc  forest : 0.7703
#  User Acc  clear_cut : 0.9503
#     User Acc  forest : 0.8261


#    ___  ___  ___    ___  ___
#   | _ \|_ _||   \  / __|| __|
#   |   / | | | |) || (_ || _|
#   |_|_\|___||___/  \___||___|
#
model.glm2 <- sits_train(dist_prodes.tb, sits_glm(alpha = 0))
get("result_glm", envir = environment(model.glm2))$lambda.min
# [1] 0.02752016

sits_predict(prodes.tb, dist_prodes.tb, model.glm2) %>%
    dplyr::mutate(reference = label) %>% sits_accuracy()
# Confusion Matrix and Statistics
#
#                    Reference
# Prediction          deforestation2014 deforestation2015 primary_forest
#   deforestation2014                99                27              7
#   deforestation2015                40               157             10
#   primary_forest                    7                 7             57
#
# Overall Statistics
#
#  Accuracy : 0.7616
#    95% CI : (0.7173, 0.802)
#
#     Kappa : 0.6153
#
# Statistics by Class:
#
#                           Class: deforestation2014 Class: deforestation2015 Class: primary_forest
# Prod Acc (Sensitivity)                      0.6781                   0.8220                0.7703
# Specificity                                 0.8717                   0.7727                0.9585
# User Acc (Pos Pred Value)                   0.7444                   0.7585                0.8028
# Neg Pred Value                              0.8309                   0.8333                0.9500

sits_predict(prodes.tb, dist_prodes.tb, model.glm2) %>%
    dplyr::mutate(reference = label) %>% sits_accuracy(conv.lst)
# Confusion Matrix and Statistics
#
#            Reference
# Prediction  clear_cut forest
#   clear_cut       323     17
#   forest           14     57
#
#             Accuracy : 0.9246
#               95% CI : (0.8946, 0.9482)
#
#                Kappa : 0.7404
#
#  Prod Acc  clear_cut : 0.9585
#     Prod Acc  forest : 0.7703
#  User Acc  clear_cut : 0.9500
#     User Acc  forest : 0.8028


#    ___  _  _  ___     ___  ___   ___  ___  ___  _____
#   | _ \| \| ||   \   | __|/ _ \ | _ \| __|/ __||_   _|
#   |   /| .` || |) |_ | _|| (_) ||   /| _| \__ \  | |
#   |_|_\|_|\_||___/(_)|_|  \___/ |_|_\|___||___/  |_|
#

model.rfor <- dist_prodes.tb %>% sits_train(sits_rfor())
sits_predict(prodes.tb, dist_prodes.tb, model.rfor) %>%
    dplyr::mutate(reference = label) %>% sits_accuracy()
# Confusion Matrix and Statistics
#
#                    Reference
# Prediction          deforestation2014 deforestation2015 primary_forest
#   deforestation2014               146                 0              0
#   deforestation2015                 0               191              0
#   primary_forest                    0                 0             74
#
# Overall Statistics
#
#  Accuracy : 1
#    95% CI : (0.9911, 1)
#
#     Kappa : 1
#
# Statistics by Class:
#
#                           Class: deforestation2014 Class: deforestation2015 Class: primary_forest
# Prod Acc (Sensitivity)                           1                        1                     1
# Specificity                                      1                        1                     1
# User Acc (Pos Pred Value)                        1                        1                     1
# Neg Pred Value                                   1                        1                     1

# better to use an 80%/20% approach?
model.rfor2 <-
    prodes.tb %>% sits_sample(frac = 0.8) %>% sits_distances(patt_prodes.tb) %>%
    sits_train(sits_rfor())
sits_predict(prodes.tb, dist_prodes.tb, model.rfor2) %>%
    dplyr::mutate(reference = label) %>% sits_accuracy(conv.lst)
# Confusion Matrix and Statistics
#
#            Reference
# Prediction  clear_cut forest
#   clear_cut       324     19
#   forest           13     55
#
#             Accuracy : 0.9221
#               95% CI : (0.8919, 0.9461)
#
#                Kappa : 0.7277
#
#  Prod Acc  clear_cut : 0.9614
#     Prod Acc  forest : 0.7432
#  User Acc  clear_cut : 0.9446
#     User Acc  forest : 0.8088


#
#
#  ###################################################################



#    _  __       _____  ___   _      ____
#   | |/ /      |  ___|/ _ \ | |    |  _ \
#   | ' / _____ | |_  | | | || |    | | | |
#   | . \|_____||  _| | |_| || |___ | |_| |
#   |_|\_\      |_|    \___/ |_____||____/
#

# The best "one-shot" results were: SVM and Random Forest.
# Only both methods were used to proceed with k-fold validation

#    ___ __   __ __  __
#   / __|\ \ / /|  \/  |
#   \__ \ \ V / | |\/| |
#   |___/  \_/  |_|  |_|
#
svm.kfold <-
    prodes.tb %>% sits_kfold_validate(folds = 5,
                                      pt_method = sits_gam(),
                                      dist_method = sits_TWDTW_distances(multicores = 5),
                                      tr_method = sits_svm(formula = sits_formula_linear(),
                                                           kernel = "radial"),
                                      multicores = 2)
svm.kfold %>% sits_accuracy()
# Confusion Matrix and Statistics
#
#                    Reference
# Prediction          deforestation2014 deforestation2015 primary_forest
#   deforestation2014               104                31              9
#   deforestation2015                36               148             13
#   primary_forest                    6                12             52
#
# Overall Statistics
#
#  Accuracy : 0.7397
#    95% CI : (0.6944, 0.7814)
#
#     Kappa : 0.5815
#
# Statistics by Class:
#
#                           Class: deforestation2014 Class: deforestation2015 Class: primary_forest
# Prod Acc (Sensitivity)                      0.7123                   0.7749                0.7027
# Specificity                                 0.8491                   0.7773                0.9466
# User Acc (Pos Pred Value)                   0.7222                   0.7513                0.7429
# Neg Pred Value                              0.8427                   0.7991                0.9355

svm.kfold %>% sits_accuracy(conv.lst)
# Confusion Matrix and Statistics
#
#            Reference
# Prediction  clear_cut forest
#   clear_cut       319     22
#   forest           18     52
#
#             Accuracy : 0.9027
#               95% CI : (0.8698, 0.9296)
#
#                Kappa : 0.6633
#
#  Prod Acc  clear_cut : 0.9466
#     Prod Acc  forest : 0.7027
#  User Acc  clear_cut : 0.9355
#     User Acc  forest : 0.7429


#    ___  _  _  ___     ___  ___   ___  ___  ___  _____
#   | _ \| \| ||   \   | __|/ _ \ | _ \| __|/ __||_   _|
#   |   /| .` || |) |_ | _|| (_) ||   /| _| \__ \  | |
#   |_|_\|_|\_||___/(_)|_|  \___/ |_|_\|___||___/  |_|
#
rfor.kfold <-
    prodes.tb %>% sits_kfold_validate(folds = 5,
                                      pt_method = sits_gam(),
                                      dist_method = sits_TWDTW_distances(multicores = 5),
                                      tr_method = sits_rfor(),
                                      multicores = 2)
rfor.kfold %>% sits_accuracy()
# Confusion Matrix and Statistics
#
#                    Reference
# Prediction          deforestation2014 deforestation2015 primary_forest
#   deforestation2014               100                34             12
#   deforestation2015                38               148             11
#   primary_forest                    8                 9             51
#
# Overall Statistics
#
#  Accuracy : 0.7275
#    95% CI : (0.6817, 0.77)
#
#     Kappa : 0.5614
#
# Statistics by Class:
#
#                           Class: deforestation2014 Class: deforestation2015 Class: primary_forest
# Prod Acc (Sensitivity)                      0.6849                   0.7749                0.6892
# Specificity                                 0.8264                   0.7773                0.9496
# User Acc (Pos Pred Value)                   0.6849                   0.7513                0.7500
# Neg Pred Value                              0.8264                   0.7991                0.9329

rfor.kfold %>% sits_accuracy(conv.lst)
# Confusion Matrix and Statistics
#
#            Reference
# Prediction  clear_cut forest
#   clear_cut       320     23
#   forest           17     51
#
#             Accuracy : 0.9027
#               95% CI : (0.8698, 0.9296)
#
#                Kappa : 0.6596
#
#  Prod Acc  clear_cut : 0.9496
#     Prod Acc  forest : 0.6892
#  User Acc  clear_cut : 0.9329
#     User Acc  forest : 0.7500


# SVM produced the best accuracy: 0.7397



#
#
#  ###################################################################
#  ###################################################################
#  ###################################################################



#    _______  _____      _____  _____   _____   ______            _____
#   |__   __|/ ____|    / ____||  __ \ |  __ \ |  ____|    /\    |  __ \
#      | |  | (___     | (___  | |__) || |__) || |__      /  \   | |  | |
#      | |   \___ \     \___ \ |  ___/ |  _  / |  __|    / /\ \  | |  | |
#      | |   ____) |_   ____) || |     | | \ \ | |____  / ____ \ | |__| |
#      |_|  |_____/(_) |_____/ |_|     |_|  \_\|______|/_/    \_\|_____/
#
# This method use Machine Learning directly over time series data.
# The function called to spread time series as atributes in
# SITS distance format is `sits_spread_time_series()`.
#
# No pattern needs to be created. The attributes are the time series itself.

tssp_prodes.tb <- prodes.tb %>% sits_spread_time_series()


#
#
#  ###################################################################


#    _____  _____  ____  _____  ____
#   |_   _|| ____|/ ___||_   _|/ ___|
#     | |  |  _|  \___ \  | |  \___ \
#     | |  | |___  ___) | | |   ___) |
#     |_|  |_____||____/  |_|  |____/
#


# start of "one-shot" tests


#    ___ __   __ __  __
#   / __|\ \ / /|  \/  |
#   \__ \ \ V / | |\/| |
#   |___/  \_/  |_|  |_|
#
tssp.svm <- sits_train(tssp_prodes.tb, sits_svm(formula = sits_formula_linear(),
                                                kernel = "radial"))
sits_predict(prodes.tb, tssp_prodes.tb, tssp.svm) %>%
    dplyr::mutate(reference = label) %>% sits_accuracy()
# Confusion Matrix and Statistics
#
#                    Reference
# Prediction          deforestation2014 deforestation2015 primary_forest
#   deforestation2014               146                 0              0
#   deforestation2015                 0               191              0
#   primary_forest                    0                 0             74
#
# Overall Statistics
#
#  Accuracy : 1
#    95% CI : (0.9911, 1)
#
#     Kappa : 1
#
# Statistics by Class:
#
#                           Class: deforestation2014 Class: deforestation2015 Class: primary_forest
# Prod Acc (Sensitivity)                           1                        1                     1
# Specificity                                      1                        1                     1
# User Acc (Pos Pred Value)                        1                        1                     1
# Neg Pred Value                                   1                        1                     1


#    _     ___    _
#   | |   |   \  /_\
#   | |__ | |) |/ _ \
#   |____||___//_/ \_\
#
tssp.lda <- sits_train(tssp_prodes.tb, sits_lda(formula = sits_formula_linear()))
sits_predict(prodes.tb, tssp_prodes.tb, tssp.lda) %>%
    dplyr::mutate(reference = label) %>% sits_accuracy()
# Confusion Matrix and Statistics
#
#                    Reference
# Prediction          deforestation2014 deforestation2015 primary_forest
#   deforestation2014               141                 5              3
#   deforestation2015                 3               182              0
#   primary_forest                    2                 4             71
#
# Overall Statistics
#
#  Accuracy : 0.9586
#    95% CI : (0.9346, 0.9757)
#
#     Kappa : 0.9342
#
# Statistics by Class:
#
#                           Class: deforestation2014 Class: deforestation2015 Class: primary_forest
# Prod Acc (Sensitivity)                      0.9658                   0.9529                0.9595
# Specificity                                 0.9698                   0.9864                0.9822
# User Acc (Pos Pred Value)                   0.9463                   0.9838                0.9221
# Neg Pred Value                              0.9809                   0.9602                0.9910


#     ___   ___    _
#    / _ \ |   \  /_\
#   | (_) || |) |/ _ \
#    \__\_\|___//_/ \_\
#
tssp.qda <- sits_train(tssp_prodes.tb, sits_qda(formula = sits_formula_linear()))
sits_predict(prodes.tb, tssp_prodes.tb, tssp.qda) %>%
    dplyr::mutate(reference = label) %>% sits_accuracy()
# Confusion Matrix and Statistics
#
#                    Reference
# Prediction          deforestation2014 deforestation2015 primary_forest
#   deforestation2014               146                 0              0
#   deforestation2015                 0               191              0
#   primary_forest                    0                 0             74
#
# Overall Statistics
#
#  Accuracy : 1
#    95% CI : (0.9911, 1)
#
#     Kappa : 1
#
# Statistics by Class:
#
#                           Class: deforestation2014 Class: deforestation2015 Class: primary_forest
# Prod Acc (Sensitivity)                           1                        1                     1
# Specificity                                      1                        1                     1
# User Acc (Pos Pred Value)                        1                        1                     1
# Neg Pred Value                                   1                        1                     1


#    __  __  _     ___
#   |  \/  || |   | _ \
#   | |\/| || |__ |   /
#   |_|  |_||____||_|_\
#
tssp.mlr <- sits_train(tssp_prodes.tb, sits_mlr(formula = sits_formula_linear()))
sits_predict(prodes.tb, tssp_prodes.tb, tssp.mlr) %>%
    dplyr::mutate(reference = label) %>% sits_accuracy()
# Confusion Matrix and Statistics
#
#                    Reference
# Prediction          deforestation2014 deforestation2015 primary_forest
#   deforestation2014               146                 0              0
#   deforestation2015                 0               191              0
#   primary_forest                    0                 0             74
#
# Overall Statistics
#
#  Accuracy : 1
#    95% CI : (0.9911, 1)
#
#     Kappa : 1
#
# Statistics by Class:
#
#                           Class: deforestation2014 Class: deforestation2015 Class: primary_forest
# Prod Acc (Sensitivity)                           1                        1                     1
# Specificity                                      1                        1                     1
# User Acc (Pos Pred Value)                        1                        1                     1
# Neg Pred Value                                   1                        1                     1


#    _       _    ___  ___   ___
#   | |     /_\  / __|/ __| / _ \
#   | |__  / _ \ \__ \\__ \| (_) |
#   |____|/_/ \_\|___/|___/ \___/
#

# glm use log function so translate time series values to strictly positive numbers:
positive_prodes.tb <- prodes.tb %>% sits_apply(function(band) (1 + 9 * band) ** 2)
positive_tssp_prodes.tb <- positive_prodes.tb %>% sits_spread_time_series()

tssp.glm1 <- sits_train(positive_tssp_prodes.tb, sits_glm(alpha = 1))
get("result_glm", envir = environment(tssp.glm1))$lambda.min
# [1] 0.009024855

sits_predict(prodes.tb, positive_tssp_prodes.tb, tssp.glm1) %>%
    dplyr::mutate(reference = label) %>% sits_accuracy()
# Confusion Matrix and Statistics
#
#                    Reference
# Prediction          deforestation2014 deforestation2015 primary_forest
#   deforestation2014               136                10              2
#   deforestation2015                 7               179              1
#   primary_forest                    3                 2             71
#
# Overall Statistics
#
#  Accuracy : 0.9392
#    95% CI : (0.9115, 0.9603)
#
#     Kappa : 0.903
#
# Statistics by Class:
#
#                           Class: deforestation2014 Class: deforestation2015 Class: primary_forest
# Prod Acc (Sensitivity)                      0.9315                   0.9372                0.9595
# Specificity                                 0.9547                   0.9636                0.9852
# User Acc (Pos Pred Value)                   0.9189                   0.9572                0.9342
# Neg Pred Value                              0.9620                   0.9464                0.9910


#    ___  ___  ___    ___  ___
#   | _ \|_ _||   \  / __|| __|
#   |   / | | | |) || (_ || _|
#   |_|_\|___||___/  \___||___|
#

# glm use log function so translate time series values to strictly positive numbers:
positive_prodes.tb <- prodes.tb %>% sits_apply(function(band) (1 + 9 * band) ** 2)
positive_tssp_prodes.tb <- positive_prodes.tb %>% sits_spread_time_series()

tssp.glm2 <- sits_train(positive_tssp_prodes.tb, sits_glm(alpha = 0))
get("result_glm", envir = environment(tssp.glm2))$lambda.min
# [1] 0.03095949

sits_predict(prodes.tb, positive_tssp_prodes.tb, tssp.glm2) %>%
    dplyr::mutate(reference = label) %>% sits_accuracy()
# Confusion Matrix and Statistics
#
#                    Reference
# Prediction          deforestation2014 deforestation2015 primary_forest
#   deforestation2014               139                 8              0
#   deforestation2015                 5               182              0
#   primary_forest                    2                 1             74
#
# Overall Statistics
#
#  Accuracy : 0.9611
#    95% CI : (0.9375, 0.9776)
#
#     Kappa : 0.938
#
# Statistics by Class:
#
#                           Class: deforestation2014 Class: deforestation2015 Class: primary_forest
# Prod Acc (Sensitivity)                      0.9521                   0.9529                1.0000
# Specificity                                 0.9698                   0.9773                0.9911
# User Acc (Pos Pred Value)                   0.9456                   0.9733                0.9610
# Neg Pred Value                              0.9735                   0.9598                1.0000


#    ___  _  _  ___     ___  ___   ___  ___  ___  _____
#   | _ \| \| ||   \   | __|/ _ \ | _ \| __|/ __||_   _|
#   |   /| .` || |) |_ | _|| (_) ||   /| _| \__ \  | |
#   |_|_\|_|\_||___/(_)|_|  \___/ |_|_\|___||___/  |_|
#
tssp.rfor <- tssp_prodes.tb %>% sits_train(sits_rfor())
sits_predict(prodes.tb, tssp_prodes.tb, tssp.rfor) %>%
    dplyr::mutate(reference = label) %>% sits_accuracy()
# Confusion Matrix and Statistics
#
#                    Reference
# Prediction          deforestation2014 deforestation2015 primary_forest
#   deforestation2014               146                 0              0
#   deforestation2015                 0               191              0
#   primary_forest                    0                 0             74
#
# Overall Statistics
#
#  Accuracy : 1
#    95% CI : (0.9911, 1)
#
#     Kappa : 1
#
# Statistics by Class:
#
#                           Class: deforestation2014 Class: deforestation2015 Class: primary_forest
# Prod Acc (Sensitivity)                           1                        1                     1
# Specificity                                      1                        1                     1
# User Acc (Pos Pred Value)                        1                        1                     1
# Neg Pred Value                                   1                        1                     1


#
#
#  ###################################################################


#    _  __       _____  ___   _      ____
#   | |/ /      |  ___|/ _ \ | |    |  _ \
#   | ' / _____ | |_  | | | || |    | | | |
#   | . \|_____||  _| | |_| || |___ | |_| |
#   |_|\_\      |_|    \___/ |_____||____/
#
#
# All "one-shot" results were above 0.93.
# All methods were used to proceed with k-fold validation


#    ___ __   __ __  __
#   / __|\ \ / /|  \/  |
#   \__ \ \ V / | |\/| |
#   |___/  \_/  |_|  |_|
#

prodes.tb %>% sits_kfold_validate(folds = 5,
                                  pt_method = function(...) NULL,
                                  dist_method = sits_spread_time_series(),
                                  tr_method = sits_svm(formula = sits_formula_linear(),
                                                       kernel = "radial"),
                                  multicores = 5) %>% sits_accuracy()
# Confusion Matrix and Statistics
#
#                    Reference
# Prediction          deforestation2014 deforestation2015 primary_forest
#   deforestation2014               129                14              2
#   deforestation2015                13               172              2
#   primary_forest                    4                 5             70
#
# Overall Statistics
#
#  Accuracy : 0.9027
#    95% CI : (0.8698, 0.9296)
#
#     Kappa : 0.8452
#
# Statistics by Class:
#
#                           Class: deforestation2014 Class: deforestation2015 Class: primary_forest
# Prod Acc (Sensitivity)                      0.8836                   0.9005                0.9459
# Specificity                                 0.9396                   0.9318                0.9733
# User Acc (Pos Pred Value)                   0.8897                   0.9198                0.8861
# Neg Pred Value                              0.9361                   0.9152                0.9880


#    _     ___    _
#   | |   |   \  /_\
#   | |__ | |) |/ _ \
#   |____||___//_/ \_\
#

prodes.tb %>% sits_kfold_validate(folds = 5,
                                  pt_method = function(...) NULL,
                                  dist_method = sits_spread_time_series(),
                                  tr_method = sits_lda(formula = sits_formula_linear()),
                                  multicores = 5) %>% sits_accuracy()
# Confusion Matrix and Statistics
#
#                    Reference
# Prediction          deforestation2014 deforestation2015 primary_forest
#   deforestation2014               130                 9              4
#   deforestation2015                13               172              4
#   primary_forest                    3                10             66
#
# Overall Statistics
#
#  Accuracy : 0.8954
#    95% CI : (0.8617, 0.9232)
#
#     Kappa : 0.8334
#
# Statistics by Class:
#
#                           Class: deforestation2014 Class: deforestation2015 Class: primary_forest
# Prod Acc (Sensitivity)                      0.8904                   0.9005                0.8919
# Specificity                                 0.9509                   0.9227                0.9614
# User Acc (Pos Pred Value)                   0.9091                   0.9101                0.8354
# Neg Pred Value                              0.9403                   0.9144                0.9759


#     ___   ___    _
#    / _ \ |   \  /_\
#   | (_) || |) |/ _ \
#    \__\_\|___//_/ \_\
#

# qda needs more cases than attributes, so we will select only two bands to produce the spreaded time series distance table
prodes.tb %>% sits_select_bands(c("ndvi", "evi")) %>%
    sits_kfold_validate(folds = 5,
                        pt_method = function(...) NULL,
                        dist_method = sits_spread_time_series(),
                        tr_method = sits_qda(formula = sits_formula_linear()),
                        multicores = 1) %>% sits_accuracy()
# Confusion Matrix and Statistics
#
#                    Reference
# Prediction          deforestation2014 deforestation2015 primary_forest
#   deforestation2014               131                 8             17
#   deforestation2015                15               183             47
#   primary_forest                    0                 0             10
#
# Overall Statistics
#
#  Accuracy : 0.7883
#    95% CI : (0.7456, 0.8268)
#
#     Kappa : 0.6374
#
# Statistics by Class:
#
#                           Class: deforestation2014 Class: deforestation2015 Class: primary_forest
# Prod Acc (Sensitivity)                      0.8973                   0.9581                0.1351
# Specificity                                 0.9057                   0.7182                1.0000
# User Acc (Pos Pred Value)                   0.8397                   0.7469                1.0000
# Neg Pred Value                              0.9412                   0.9518                0.8404



#    __  __  _     ___
#   |  \/  || |   | _ \
#   | |\/| || |__ |   /
#   |_|  |_||____||_|_\
#
prodes.tb %>% sits_kfold_validate(folds = 5,
                                  pt_method = function(...) NULL,
                                  dist_method = sits_spread_time_series(),
                                  tr_method = sits_mlr(formula = sits_formula_linear()),
                                  multicores = 5) %>% sits_accuracy()
# Confusion Matrix and Statistics
#
#                    Reference
# Prediction          deforestation2014 deforestation2015 primary_forest
#   deforestation2014               129                13              1
#   deforestation2015                12               170             12
#   primary_forest                    5                 8             61
#
# Overall Statistics
#
#  Accuracy : 0.8759
#    95% CI : (0.8401, 0.9062)
#
#     Kappa : 0.8013
#
# Statistics by Class:
#
#                           Class: deforestation2014 Class: deforestation2015 Class: primary_forest
# Prod Acc (Sensitivity)                      0.8836                   0.8901                0.8243
# Specificity                                 0.9472                   0.8909                0.9614
# User Acc (Pos Pred Value)                   0.9021                   0.8763                0.8243
# Neg Pred Value                              0.9366                   0.9032                0.9614



#    _       _    ___  ___   ___
#   | |     /_\  / __|/ __| / _ \
#   | |__  / _ \ \__ \\__ \| (_) |
#   |____|/_/ \_\|___/|___/ \___/
#

# glm use log function so translate time series values to strictly positive numbers:
positive_prodes.tb %>% sits_kfold_validate(folds = 5,
                                           pt_method = function(...) NULL,
                                           dist_method = sits_spread_time_series(),
                                           tr_method = sits_glm(alpha = 1),
                                           multicores = 5) %>% sits_accuracy()
# Confusion Matrix and Statistics
#
#                    Reference
# Prediction          deforestation2014 deforestation2015 primary_forest
#   deforestation2014               132                17              4
#   deforestation2015                11               168              2
#   primary_forest                    3                 6             68
#
# Overall Statistics
#
#  Accuracy : 0.8954
#    95% CI : (0.8617, 0.9232)
#
#     Kappa : 0.8338
#
# Statistics by Class:
#
#                           Class: deforestation2014 Class: deforestation2015 Class: primary_forest
# Prod Acc (Sensitivity)                      0.9041                   0.8796                0.9189
# Specificity                                 0.9208                   0.9409                0.9733
# User Acc (Pos Pred Value)                   0.8627                   0.9282                0.8831
# Neg Pred Value                              0.9457                   0.9000                0.9820


#    ___  ___  ___    ___  ___
#   | _ \|_ _||   \  / __|| __|
#   |   / | | | |) || (_ || _|
#   |_|_\|___||___/  \___||___|
#

# glm use log function so translate time series values to strictly positive numbers:
positive_prodes.tb %>% sits_kfold_validate(folds = 5,
                                           pt_method = function(...) NULL,
                                           dist_method = sits_spread_time_series(),
                                           tr_method = sits_glm(alpha = 0),
                                           multicores = 5) %>% sits_accuracy()
# Confusion Matrix and Statistics
#
#                    Reference
# Prediction          deforestation2014 deforestation2015 primary_forest
#   deforestation2014               127                19              3
#   deforestation2015                15               167              6
#   primary_forest                    4                 5             65
#
# Overall Statistics
#
#  Accuracy : 0.8735
#    95% CI : (0.8374, 0.904)
#
#     Kappa : 0.798
#
# Statistics by Class:
#
#                           Class: deforestation2014 Class: deforestation2015 Class: primary_forest
# Prod Acc (Sensitivity)                      0.8699                   0.8743                0.8784
# Specificity                                 0.9170                   0.9045                0.9733
# User Acc (Pos Pred Value)                   0.8523                   0.8883                0.8784
# Neg Pred Value                              0.9275                   0.8924                0.9733




#    ___  _  _  ___     ___  ___   ___  ___  ___  _____
#   | _ \| \| ||   \   | __|/ _ \ | _ \| __|/ __||_   _|
#   |   /| .` || |) |_ | _|| (_) ||   /| _| \__ \  | |
#   |_|_\|_|\_||___/(_)|_|  \___/ |_|_\|___||___/  |_|
#
prodes.tb %>% sits_kfold_validate(folds = 5,
                                  pt_method = function(...) NULL,
                                  dist_method = sits_spread_time_series(),
                                  tr_method = sits_rfor(),
                                  multicores = 5) %>% sits_accuracy()
# Confusion Matrix and Statistics
#
#                    Reference
# Prediction          deforestation2014 deforestation2015 primary_forest
#   deforestation2014               135                10              1
#   deforestation2015                 9               180              4
#   primary_forest                    2                 1             69
#
# Overall Statistics
#
#  Accuracy : 0.9343
#    95% CI : (0.9059, 0.9563)
#
#     Kappa : 0.8947
#
# Statistics by Class:
#
#                           Class: deforestation2014 Class: deforestation2015 Class: primary_forest
# Prod Acc (Sensitivity)                      0.9247                   0.9424                0.9324
# Specificity                                 0.9585                   0.9409                0.9911
# User Acc (Pos Pred Value)                   0.9247                   0.9326                0.9583
# Neg Pred Value                              0.9585                   0.9495                0.9853




# Random Forest produced the best accuracy: 0.9343



#
#
#  ###################################################################
#  ###################################################################
#  ###################################################################



