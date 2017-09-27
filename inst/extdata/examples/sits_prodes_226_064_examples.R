library(wtss)
library(sits)


#    _____    _____     ____    _____    ______    _____
#   |  __ \  |  __ \   / __ \  |  __ \  |  ____|  / ____|
#   | |__) | | |__) | | |  | | | |  | | | |__    | (___
#   |  ___/  |  _  /  | |  | | | |  | | |  __|    \___ \
#   | |      | | \ \  | |__| | | |__| | | |____   ____) |
#   |_|      |_|  \_\  \____/  |_____/  |______| |_____/
#
#
########################################################################################


# load data and fill NAs with linear interpolation
prodes.tb <- sits_getdata(system.file("extdata/samples/prodes_226_064.json.gz", package = "sits")) %>%
    sits_select_bands(c("ndvi", "evi", "nir")) %>% dplyr::filter(label != "primary_forest") %>% sits_linear_interp()

prodes2.tb <- sits_getdata(system.file("extdata/samples/prodes_samples_226_64_v1.json.gz", package = "sits")) %>%
    sits_select_bands(c("ndvi", "evi", "nir")) %>% dplyr::filter(label != "ClearCut") %>% sits_linear_interp()

prodes.tb <- prodes.tb %>% dplyr::bind_rows(prodes2.tb)
rm(prodes2.tb)

# view a summary of the data
sits_summary(prodes.tb)

# create list of label conversion
conv.lst <- list("deforestation2014" = "Clear_Cut",
                 "deforestation2015" = "Clear_Cut",
                 "Forest" = "Forest",
                 "Pasture" = "Pasture")

# create list for non_forest/forest classes
conv2.lst <- list("deforestation2014" = "Non_Forest",
                  "deforestation2015" = "Non_Forest",
                  "Forest" = "Forest",
                  "Pasture" = "Non_Forest")

# create patterns and plot them
patt_prodes.tb <- prodes.tb %>% sits_gam()
sits_plot_patterns(patt_prodes.tb)

# create distances
dist_prodes.tb <- prodes.tb %>% sits_distances(patt_prodes.tb)

#######################################################################################
#    _____       _______  _______   _____  _____   _____  _______
#   |  __ \  /\ |__   __||__   __| |  __ \|_   _| / ____||__   __|
#   | |__) |/  \   | |      | |    | |  | | | |  | (___     | |
#   |  ___// /\ \  | |      | |    | |  | | | |   \___ \    | |
#   | |   / ____ \ | |      | | _  | |__| |_| |_  ____) |   | | _
#   |_|  /_/    \_\|_|      |_|(_) |_____/|_____||_____/    |_|(_)
#
#


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

prodes.tb$predicted <- sits_predict(dist_prodes.tb, model.svm)
prodes.tb %>%
    dplyr::mutate(reference = label) %>%
    sits_accuracy(conv.lst)
# Confusion Matrix and Statistics
#
#            Reference
# Prediction  Clear_Cut Forest Pasture
#   Clear_Cut       317      7      18
#   Forest            4    117       3
#   Pasture          16      0     124
#
# Overall Statistics
#
#  Accuracy : 0.9208
#    95% CI : (0.8964, 0.941)
#
#     Kappa : 0.8655
#
# Statistics by Class:
#
#                           Class: Clear_Cut Class: Forest Class: Pasture
# Prod Acc (Sensitivity)              0.9407        0.9435         0.8552
# Specificity                         0.9071        0.9855         0.9653
# User Acc (Pos Pred Value)           0.9269        0.9435         0.8857
# Neg Pred Value                      0.9242        0.9855         0.9549

prodes.tb$predicted <- sits_predict(dist_prodes.tb, model.svm)
prodes.tb %>% dplyr::mutate(reference = label) %>% sits_accuracy(conv2.lst)
# Confusion Matrix and Statistics
#
#             Reference
# Prediction   Forest Non_Forest
#   Forest        117          7
#   Non_Forest      7        475
#
#              Accuracy : 0.9769
#                95% CI : (0.9615, 0.9873)
#
#                 Kappa : 0.929
#
#      Prod Acc  Forest : 0.9435
#  Prod Acc  Non_Forest : 0.9855
#      User Acc  Forest : 0.9435
#  User Acc  Non_Forest : 0.9855
#


#    _     ___    _
#   | |   |   \  /_\
#   | |__ | |) |/ _ \
#   |____||___//_/ \_\
#
model.lda <- sits_train(dist_prodes.tb, sits_lda(formula = sits_formula_linear()))
prodes.tb$predicted <- sits_predict(dist_prodes.tb, model.lda)
prodes.tb %>% dplyr::mutate(reference = label) %>% sits_accuracy(conv.lst)
# Confusion Matrix and Statistics
#
#            Reference
# Prediction  Clear_Cut Forest Pasture
#   Clear_Cut       272     13      47
#   Forest           28    107      10
#   Pasture          37      4      88
#
# Overall Statistics
#
#  Accuracy : 0.7706
#    95% CI : (0.7351, 0.8035)
#
#     Kappa : 0.6148
#
# Statistics by Class:
#
#                           Class: Clear_Cut Class: Forest Class: Pasture
# Prod Acc (Sensitivity)              0.8071        0.8629         0.6069
# Specificity                         0.7770        0.9212         0.9111
# User Acc (Pos Pred Value)           0.8193        0.7379         0.6822
# Neg Pred Value                      0.7628        0.9631         0.8805

prodes.tb$predicted <- sits_predict(dist_prodes.tb, model.lda)
prodes.tb %>% dplyr::mutate(reference = label) %>% sits_accuracy(conv2.lst)
# Confusion Matrix and Statistics
#
#             Reference
# Prediction   Forest Non_Forest
#   Forest        107         38
#   Non_Forest     17        444
#
#              Accuracy : 0.9092
#                95% CI : (0.8835, 0.9309)
#
#                 Kappa : 0.7377
#
#      Prod Acc  Forest : 0.8629
#  Prod Acc  Non_Forest : 0.9212
#      User Acc  Forest : 0.7379
#  User Acc  Non_Forest : 0.9631


#     ___   ___    _
#    / _ \ |   \  /_\
#   | (_) || |) |/ _ \
#    \__\_\|___//_/ \_\
#
model.qda <- sits_train(dist_prodes.tb, sits_qda(formula = sits_formula_linear()))
prodes.tb$predicted <- sits_predict(dist_prodes.tb, model.qda)
prodes.tb %>% dplyr::mutate(reference = label) %>% sits_accuracy(conv.lst)
# Confusion Matrix and Statistics
#
#            Reference
# Prediction  Clear_Cut Forest Pasture
#   Clear_Cut       278      9      36
#   Forest           21    113       9
#   Pasture          38      2     100
#
# Overall Statistics
#
#  Accuracy : 0.8102
#    95% CI : (0.7767, 0.8407)
#
#     Kappa : 0.6837
#
# Statistics by Class:
#
#                           Class: Clear_Cut Class: Forest Class: Pasture
# Prod Acc (Sensitivity)              0.8249        0.9113         0.6897
# Specificity                         0.8327        0.9378         0.9132
# User Acc (Pos Pred Value)           0.8607        0.7902         0.7143
# Neg Pred Value                      0.7915        0.9762         0.9034

prodes.tb$predicted <- sits_predict(dist_prodes.tb, model.qda)
prodes.tb %>% dplyr::mutate(reference = label) %>% sits_accuracy(conv2.lst)
# Confusion Matrix and Statistics
#
#             Reference
# Prediction   Forest Non_Forest
#   Forest        113         30
#   Non_Forest     11        452
#
#              Accuracy : 0.9323
#                95% CI : (0.9093, 0.951)
#
#                 Kappa : 0.8033
#
#      Prod Acc  Forest : 0.9113
#  Prod Acc  Non_Forest : 0.9378
#      User Acc  Forest : 0.7902
#  User Acc  Non_Forest : 0.9762


#    __  __  _     ___
#   |  \/  || |   | _ \
#   | |\/| || |__ |   /
#   |_|  |_||____||_|_\
#
model.mlr <- sits_train(dist_prodes.tb, sits_mlr(formula = sits_formula_linear()))
prodes.tb$predicted <- sits_predict(dist_prodes.tb, model.mlr)
prodes.tb %>% dplyr::mutate(reference = label) %>% sits_accuracy(conv.lst)
# Confusion Matrix and Statistics
#
#            Reference
# Prediction  Clear_Cut Forest Pasture
#   Clear_Cut       279     14      50
#   Forest           23    107      10
#   Pasture          35      3      85
#
# Overall Statistics
#
#  Accuracy : 0.7772
#    95% CI : (0.742, 0.8098)
#
#     Kappa : 0.622
#
# Statistics by Class:
#
#                           Class: Clear_Cut Class: Forest Class: Pasture
# Prod Acc (Sensitivity)              0.8279        0.8629         0.5862
# Specificity                         0.7621        0.9315         0.9176
# User Acc (Pos Pred Value)           0.8134        0.7643         0.6911
# Neg Pred Value                      0.7795        0.9635         0.8758

prodes.tb$predicted <- sits_predict(dist_prodes.tb, model.mlr)
prodes.tb %>% dplyr::mutate(reference = label) %>% sits_accuracy(conv2.lst)
# Confusion Matrix and Statistics
#
#             Reference
# Prediction   Forest Non_Forest
#   Forest        107         33
#   Non_Forest     17        449
#
#              Accuracy : 0.9175
#                95% CI : (0.8927, 0.9381)
#
#                 Kappa : 0.7581
#
#      Prod Acc  Forest : 0.8629
#  Prod Acc  Non_Forest : 0.9315
#      User Acc  Forest : 0.7643
#  User Acc  Non_Forest : 0.9635

#    _       _    ___  ___   ___
#   | |     /_\  / __|/ __| / _ \
#   | |__  / _ \ \__ \\__ \| (_) |
#   |____|/_/ \_\|___/|___/ \___/
#
model.glm1 <- sits_train(dist_prodes.tb, sits_glm(alpha = 1))
get("result_glm", envir = environment(model.glm1))$lambda.min
# [1] 0.004700096

prodes.tb$predicted <- sits_predict(dist_prodes.tb, model.glm1)
prodes.tb %>% dplyr::mutate(reference = label) %>% sits_accuracy(conv.lst)
# Confusion Matrix and Statistics
#
#            Reference
# Prediction  Clear_Cut Forest Pasture
#   Clear_Cut       285     12      52
#   Forest           21    107      11
#   Pasture          31      5      82
#
# Overall Statistics
#
#  Accuracy : 0.7822
#    95% CI : (0.7472, 0.8144)
#
#     Kappa : 0.6284
#
# Statistics by Class:
#
#                           Class: Clear_Cut Class: Forest Class: Pasture
# Prod Acc (Sensitivity)              0.8457        0.8629         0.5655
# Specificity                         0.7621        0.9336         0.9219
# User Acc (Pos Pred Value)           0.8166        0.7698         0.6949
# Neg Pred Value                      0.7977        0.9636         0.8709

prodes.tb$predicted <- sits_predict(dist_prodes.tb, model.glm1)
prodes.tb %>% dplyr::mutate(reference = label) %>% sits_accuracy(conv2.lst)
# Confusion Matrix and Statistics
#
#             Reference
# Prediction   Forest Non_Forest
#   Forest        107         32
#   Non_Forest     17        450
#
#              Accuracy : 0.9191
#                95% CI : (0.8945, 0.9396)
#
#                 Kappa : 0.7623
#
#      Prod Acc  Forest : 0.8629
#  Prod Acc  Non_Forest : 0.9336
#      User Acc  Forest : 0.7698
#  User Acc  Non_Forest : 0.9636


#    ___  ___  ___    ___  ___
#   | _ \|_ _||   \  / __|| __|
#   |   / | | | |) || (_ || _|
#   |_|_\|___||___/  \___||___|
#
model.glm2 <- sits_train(dist_prodes.tb, sits_glm(alpha = 0))
get("result_glm", envir = environment(model.glm2))$lambda.min
# [1] 0.01942086

prodes.tb$predicted <- sits_predict(dist_prodes.tb, model.glm2)
prodes.tb %>% dplyr::mutate(reference = label) %>% sits_accuracy(conv.lst)
# Confusion Matrix and Statistics
#
#            Reference
# Prediction  Clear_Cut Forest Pasture
#   Clear_Cut       282     11      57
#   Forest           21    108       9
#   Pasture          34      5      79
#
# Overall Statistics
#
#  Accuracy : 0.7739
#    95% CI : (0.7385, 0.8067)
#
#     Kappa : 0.614
#
# Statistics by Class:
#
#                           Class: Clear_Cut Class: Forest Class: Pasture
# Prod Acc (Sensitivity)              0.8368        0.8710         0.5448
# Specificity                         0.7472        0.9378         0.9154
# User Acc (Pos Pred Value)           0.8057        0.7826         0.6695
# Neg Pred Value                      0.7852        0.9658         0.8648

prodes.tb$predicted <- sits_predict(dist_prodes.tb, model.glm2)
prodes.tb %>% dplyr::mutate(reference = label) %>% sits_accuracy(conv2.lst)
# Confusion Matrix and Statistics
#
#             Reference
# Prediction   Forest Non_Forest
#   Forest        108         30
#   Non_Forest     16        452
#
#              Accuracy : 0.9241
#                95% CI : (0.9, 0.9439)
#
#                 Kappa : 0.7762
#
#      Prod Acc  Forest : 0.8710
#  Prod Acc  Non_Forest : 0.9378
#      User Acc  Forest : 0.7826
#  User Acc  Non_Forest : 0.9658


#    ___  _  _  ___     ___  ___   ___  ___  ___  _____
#   | _ \| \| ||   \   | __|/ _ \ | _ \| __|/ __||_   _|
#   |   /| .` || |) |_ | _|| (_) ||   /| _| \__ \  | |
#   |_|_\|_|\_||___/(_)|_|  \___/ |_|_\|___||___/  |_|
#

# random forest returning 1.0 of accuracy. Reducing data training to 95%
model.rfor <- prodes.tb %>% sits_sample(frac = 0.95) %>%
    sits_distances(patt_prodes.tb) %>%
    sits_train(sits_rfor())

# test on all data set
prodes.tb$predicted <- sits_predict(dist_prodes.tb, model.rfor)
prodes.tb %>% dplyr::mutate(reference = label) %>% sits_accuracy(conv.lst)
# Confusion Matrix and Statistics
#
#            Reference
# Prediction  Clear_Cut Forest Pasture
#   Clear_Cut       288     13      54
#   Forest           14     94       9
#   Pasture          35     17      82
#
# Overall Statistics
#
#  Accuracy : 0.7657
#    95% CI : (0.7299, 0.7989)
#
#     Kappa : 0.5973
#
# Statistics by Class:
#
#                           Class: Clear_Cut Class: Forest Class: Pasture
# Prod Acc (Sensitivity)              0.8546        0.7581         0.5655
# Specificity                         0.7509        0.9523         0.8872
# User Acc (Pos Pred Value)           0.8113        0.8034         0.6119
# Neg Pred Value                      0.8048        0.9387         0.8665

prodes.tb$predicted <- sits_predict(dist_prodes.tb, model.rfor)
prodes.tb %>% dplyr::mutate(reference = label) %>% sits_accuracy(conv2.lst)
# Confusion Matrix and Statistics
#
#             Reference
# Prediction   Forest Non_Forest
#   Forest         94         23
#   Non_Forest     30        459
#
#              Accuracy : 0.9125
#                95% CI : (0.8872, 0.9338)
#
#                 Kappa : 0.7256
#
#      Prod Acc  Forest : 0.7581
#  Prod Acc  Non_Forest : 0.9523
#      User Acc  Forest : 0.8034
#  User Acc  Non_Forest : 0.9387



# result    PATT.DIST.
# - SVM:      0.9769 *
# - LDA:      0.9092
# - QDA:      0.9323 *
# - MLR:      0.9175
# - LASSO:    0.9191
# - RIDGE:    0.9241
# - RFOR:     0.9125

#
#
#  ###################################################################



#    _  __       _____  ___   _      ____
#   | |/ /      |  ___|/ _ \ | |    |  _ \
#   | ' / _____ | |_  | | | || |    | | | |
#   | . \|_____||  _| | |_| || |___ | |_| |
#   |_|\_\      |_|    \___/ |_____||____/
#

# The best "one-shot" results were: SVM and QDA.
# We processed k-fold validation for both methods

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
svm.kfold %>% sits_accuracy(conv.lst)
# Confusion Matrix and Statistics
#
#            Reference
# Prediction  Clear_Cut Forest Pasture
#   Clear_Cut       268     22      57
#   Forest           19     91      11
#   Pasture          50     11      77
#
# Overall Statistics
#
#  Accuracy : 0.7195
#    95% CI : (0.6819, 0.7549)
#
#     Kappa : 0.5215
#
# Statistics by Class:
#
#                           Class: Clear_Cut Class: Forest Class: Pasture
# Prod Acc (Sensitivity)              0.7953        0.7339         0.5310
# Specificity                         0.7063        0.9378         0.8677
# User Acc (Pos Pred Value)           0.7723        0.7521         0.5580
# Neg Pred Value                      0.7336        0.9320         0.8547

svm.kfold %>% sits_accuracy(conv2.lst)
# Confusion Matrix and Statistics
#
#             Reference
# Prediction   Forest Non_Forest
#   Forest         91         30
#   Non_Forest     33        452
#
#              Accuracy : 0.896
#                95% CI : (0.869, 0.9192)
#
#                 Kappa : 0.6777
#
#      Prod Acc  Forest : 0.7339
#  Prod Acc  Non_Forest : 0.9378
#      User Acc  Forest : 0.7521
#  User Acc  Non_Forest : 0.9320



#     ___    ___      _
#    / _ \  |   \    /_\
#   | (_) | | |) |  / _ \
#    \__\_\ |___/  /_/ \_\
#
qda.kfold <-
    prodes.tb %>% sits_kfold_validate(folds = 5,
                                      pt_method = sits_gam(),
                                      dist_method = sits_TWDTW_distances(multicores = 5),
                                      tr_method = sits_qda(formula = sits_formula_linear()),
                                      multicores = 2)
qda.kfold %>% sits_accuracy(conv.lst)
# Confusion Matrix and Statistics
#
#            Reference
# Prediction  Clear_Cut Forest Pasture
#   Clear_Cut       264     18      53
#   Forest           22     98      11
#   Pasture          51      8      81
#
# Overall Statistics
#
#  Accuracy : 0.731
#    95% CI : (0.6938, 0.766)
#
#     Kappa : 0.5465
#
# Statistics by Class:
#
#                           Class: Clear_Cut Class: Forest Class: Pasture
# Prod Acc (Sensitivity)              0.7834        0.7903         0.5586
# Specificity                         0.7361        0.9315         0.8720
# User Acc (Pos Pred Value)           0.7881        0.7481         0.5786
# Neg Pred Value                      0.7306        0.9453         0.8627

qda.kfold %>% sits_accuracy(conv2.lst)
# Confusion Matrix and Statistics
#
#             Reference
# Prediction   Forest Non_Forest
#   Forest         98         33
#   Non_Forest     26        449
#
#              Accuracy : 0.9026
#                95% CI : (0.8762, 0.9251)
#
#                 Kappa : 0.707
#
#      Prod Acc  Forest : 0.7903
#  Prod Acc  Non_Forest : 0.9315
#      User Acc  Forest : 0.7481
#  User Acc  Non_Forest : 0.9453


# result           K-FOLD PATT.DIST.
#  - SVM:  0.8960 (0.8690, 0.9192)
#  - QDA:  0.9026 (0.8762, 0.9251) *


#
#
#  ###################################################################
#  ###################################################################
#  ###################################################################



#    _         _                 _____  _      _    _   _____  _______  ______  _____
#   (_)       | |               / ____|| |    | |  | | / ____||__   __||  ____||  __ \
#    _  _ __  | |_  _ __  __ _ | |     | |    | |  | || (___     | |   | |__   | |__) |
#   | || '_ \ | __|| '__|/ _` || |     | |    | |  | | \___ \    | |   |  __|  |  _  /
#   | || | | || |_ | |  | (_| || |____ | |____| |__| | ____) |   | |   | |____ | | \ \
#   |_||_| |_| \__||_|   \__,_| \_____||______|\____/ |_____/    |_|   |______||_|  \_\
#
#

# make intra-clusters
library(dtwclust)

sits_intracluster_patterns <- function(data.tb = NULL, k = 3){

    result_fun1 <- function(data.tb){
        dendro.tb <- data.tb %>% dplyr::group_by(label) %>%
            dplyr::do("data" = .data, "clusters" = .data %>% (function(tb) tb %>% sits_dendrogram()))

        dendro.tb %>%
            dplyr::rowwise() %>%
            dplyr::do(sits_cluster(.data$data, .data$clusters, k) %>%
                          sits_foreach(cluster, fun = function(tb) tb %>% sits_gam())) %>%
            dplyr::mutate(label = paste0(label, ".", cluster))
    }

    result_fun2 <- function(k = k){
        dendro.tb %>%
            dplyr::rowwise() %>%
            dplyr::do(sits_cluster(.data$data, .data$clusters, k) %>%
                          sits_foreach(cluster, fun = function(tb) tb %>% sits_gam())) %>%
            dplyr::mutate(label = paste0(label, ".", cluster))
    }

    dendro.tb <- NULL
    if (!is.null(data.tb))
        dendro.tb <- data.tb %>% dplyr::group_by(label) %>%
        dplyr::do("data" = .data, "clusters" = .data %>% (function(tb) tb %>% sits_dendrogram()))

    if (is.null(dendro.tb))
        return(result_fun1)

    return(result_fun2)
}

# create patterns and plot them
patt_intracluster_gen <- prodes.tb %>% sits_intracluster_patterns()
patt_prodes.tb <- patt_intracluster_gen(3)
sits_plot_patterns(patt_prodes.tb)

# create distances
dist_prodes.tb <- prodes.tb %>% sits_distances(patt_prodes.tb, dist_method = sits_TWDTW_distances(multicores = 10))


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

prodes.tb$predicted <- sits_predict(dist_prodes.tb, model.svm)
prodes.tb %>% dplyr::mutate(reference = label) %>% sits_accuracy(conv.lst)
# Confusion Matrix and Statistics
#
#            Reference
# Prediction  Clear_Cut Forest Pasture
#   Clear_Cut       332      1       7
#   Forest            1    122       0
#   Pasture           4      1     138
#
# Overall Statistics
#
#  Accuracy : 0.9769
#    95% CI : (0.9615, 0.9873)
#
#     Kappa : 0.9608
#
# Statistics by Class:
#
#                           Class: Clear_Cut Class: Forest Class: Pasture
# Prod Acc (Sensitivity)              0.9852        0.9839         0.9517
# Specificity                         0.9703        0.9979         0.9892
# User Acc (Pos Pred Value)           0.9765        0.9919         0.9650
# Neg Pred Value                      0.9812        0.9959         0.9849

prodes.tb$predicted <- sits_predict(dist_prodes.tb, model.svm)
prodes.tb %>% dplyr::mutate(reference = label) %>% sits_accuracy(conv2.lst)
# Confusion Matrix and Statistics
#
#             Reference
# Prediction   Forest Non_Forest
#   Forest        122          1
#   Non_Forest      2        481
#
#              Accuracy : 0.995
#                95% CI : (0.9856, 0.999)
#
#                 Kappa : 0.9847
#
#      Prod Acc  Forest : 0.9839
#  Prod Acc  Non_Forest : 0.9979
#      User Acc  Forest : 0.9919
#  User Acc  Non_Forest : 0.9959


#    _     ___    _
#   | |   |   \  /_\
#   | |__ | |) |/ _ \
#   |____||___//_/ \_\
#
model.lda <- sits_train(dist_prodes.tb, sits_lda(formula = sits_formula_linear()))
prodes.tb$predicted <- sits_predict(dist_prodes.tb, model.lda)
prodes.tb %>% dplyr::mutate(reference = label) %>% sits_accuracy(conv.lst)
# Confusion Matrix and Statistics
#
#            Reference
# Prediction  Clear_Cut Forest Pasture
#   Clear_Cut       284     11      29
#   Forest           17    110      11
#   Pasture          36      3     105
#
# Overall Statistics
#
#  Accuracy : 0.8234
#    95% CI : (0.7907, 0.853)
#
#     Kappa : 0.7053
#
# Statistics by Class:
#
#                           Class: Clear_Cut Class: Forest Class: Pasture
# Prod Acc (Sensitivity)              0.8427        0.8871         0.7241
# Specificity                         0.8513        0.9419         0.9154
# User Acc (Pos Pred Value)           0.8765        0.7971         0.7292
# Neg Pred Value                      0.8121        0.9701         0.9134

prodes.tb$predicted <- sits_predict(dist_prodes.tb, model.lda)
prodes.tb %>% dplyr::mutate(reference = label) %>% sits_accuracy(conv2.lst)
# Confusion Matrix and Statistics
#
#             Reference
# Prediction   Forest Non_Forest
#   Forest        110         28
#   Non_Forest     14        454
#
#              Accuracy : 0.9307
#                95% CI : (0.9075, 0.9496)
#
#                 Kappa : 0.7956
#
#      Prod Acc  Forest : 0.8871
#  Prod Acc  Non_Forest : 0.9419
#      User Acc  Forest : 0.7971
#  User Acc  Non_Forest : 0.9701


#     ___   ___    _
#    / _ \ |   \  /_\
#   | (_) || |) |/ _ \
#    \__\_\|___//_/ \_\
#
model.qda <- sits_train(dist_prodes.tb, sits_qda(formula = sits_formula_linear()))
prodes.tb$predicted <- sits_predict(dist_prodes.tb, model.qda)
prodes.tb %>% dplyr::mutate(reference = label) %>% sits_accuracy(conv.lst)
# Confusion Matrix and Statistics
#
#            Reference
# Prediction  Clear_Cut Forest Pasture
#   Clear_Cut       320      3       6
#   Forest            7    121       2
#   Pasture          10      0     137
#
# Overall Statistics
#
#  Accuracy : 0.9538
#    95% CI : (0.9339, 0.9691)
#
#     Kappa : 0.9225
#
# Statistics by Class:
#
#                           Class: Clear_Cut Class: Forest Class: Pasture
# Prod Acc (Sensitivity)              0.9496        0.9758         0.9448
# Specificity                         0.9665        0.9813         0.9783
# User Acc (Pos Pred Value)           0.9726        0.9308         0.9320
# Neg Pred Value                      0.9386        0.9937         0.9826

prodes.tb$predicted <- sits_predict(dist_prodes.tb, model.qda)
prodes.tb %>% dplyr::mutate(reference = label) %>% sits_accuracy(conv2.lst)
# Confusion Matrix and Statistics
#
#             Reference
# Prediction   Forest Non_Forest
#   Forest        121          9
#   Non_Forest      3        473
#
#              Accuracy : 0.9802
#                95% CI : (0.9657, 0.9897)
#
#                 Kappa : 0.9402
#
#      Prod Acc  Forest : 0.9758
#  Prod Acc  Non_Forest : 0.9813
#      User Acc  Forest : 0.9308
#  User Acc  Non_Forest : 0.9937


#    __  __  _     ___
#   |  \/  || |   | _ \
#   | |\/| || |__ |   /
#   |_|  |_||____||_|_\
#
model.mlr <- sits_train(dist_prodes.tb, sits_mlr(formula = sits_formula_linear()))
prodes.tb$predicted <- sits_predict(dist_prodes.tb, model.mlr)
prodes.tb %>% dplyr::mutate(reference = label) %>% sits_accuracy(conv.lst)
# Confusion Matrix and Statistics
#
#            Reference
# Prediction  Clear_Cut Forest Pasture
#   Clear_Cut       294     11      35
#   Forest           13    110       9
#   Pasture          30      3     101
#
# Overall Statistics
#
#  Accuracy : 0.8333
#    95% CI : (0.8012, 0.8621)
#
#     Kappa : 0.7178
#
# Statistics by Class:
#
#                           Class: Clear_Cut Class: Forest Class: Pasture
# Prod Acc (Sensitivity)              0.8724        0.8871         0.6966
# Specificity                         0.8290        0.9544         0.9284
# User Acc (Pos Pred Value)           0.8647        0.8333         0.7537
# Neg Pred Value                      0.8383        0.9705         0.9068

prodes.tb$predicted <- sits_predict(dist_prodes.tb, model.mlr)
prodes.tb %>% dplyr::mutate(reference = label) %>% sits_accuracy(conv2.lst)
# Confusion Matrix and Statistics
#
#             Reference
# Prediction   Forest Non_Forest
#   Forest        110         22
#   Non_Forest     14        460
#
#              Accuracy : 0.9406
#                95% CI : (0.9187, 0.958)
#
#                 Kappa : 0.8218
#
#      Prod Acc  Forest : 0.8871
#  Prod Acc  Non_Forest : 0.9544
#      User Acc  Forest : 0.8333
#  User Acc  Non_Forest : 0.9705


#    _       _    ___  ___   ___
#   | |     /_\  / __|/ __| / _ \
#   | |__  / _ \ \__ \\__ \| (_) |
#   |____|/_/ \_\|___/|___/ \___/
#
model.glm1 <- sits_train(dist_prodes.tb, sits_glm(alpha = 1))
get("result_glm", envir = environment(model.glm1))$lambda.min
# [1] 0.003934933

prodes.tb$predicted <- sits_predict(dist_prodes.tb, model.glm1)
prodes.tb %>% dplyr::mutate(reference = label) %>% sits_accuracy(conv.lst)
# Confusion Matrix and Statistics
#
#            Reference
# Prediction  Clear_Cut Forest Pasture
#   Clear_Cut       291     10      40
#   Forest           14    109      12
#   Pasture          32      5      93
#
# Overall Statistics
#
#  Accuracy : 0.8135
#    95% CI : (0.7802, 0.8438)
#
#     Kappa : 0.684
#
# Statistics by Class:
#
#                           Class: Clear_Cut Class: Forest Class: Pasture
# Prod Acc (Sensitivity)              0.8635        0.8790         0.6414
# Specificity                         0.8141        0.9461         0.9197
# User Acc (Pos Pred Value)           0.8534        0.8074         0.7154
# Neg Pred Value                      0.8264        0.9682         0.8908

prodes.tb$predicted <- sits_predict(dist_prodes.tb, model.glm1)
prodes.tb %>% dplyr::mutate(reference = label) %>% sits_accuracy(conv2.lst)
# Confusion Matrix and Statistics
#
#             Reference
# Prediction   Forest Non_Forest
#   Forest        109         26
#   Non_Forest     15        456
#
#              Accuracy : 0.9323
#                95% CI : (0.9093, 0.951)
#
#                 Kappa : 0.7988
#
#      Prod Acc  Forest : 0.8790
#  Prod Acc  Non_Forest : 0.9461
#      User Acc  Forest : 0.8074
#  User Acc  Non_Forest : 0.9682


#    ___  ___  ___    ___  ___
#   | _ \|_ _||   \  / __|| __|
#   |   / | | | |) || (_ || _|
#   |_|_\|___||___/  \___||___|
#
model.glm2 <- sits_train(dist_prodes.tb, sits_glm(alpha = 0))
get("result_glm", envir = environment(model.glm2))$lambda.min
# [1] 0.02149369

prodes.tb$predicted <- sits_predict(dist_prodes.tb, model.glm2)
prodes.tb %>% dplyr::mutate(reference = label) %>% sits_accuracy(conv.lst)
# Confusion Matrix and Statistics
#
#            Reference
# Prediction  Clear_Cut Forest Pasture
#   Clear_Cut       288     13      39
#   Forest           17    107      13
#   Pasture          32      4      93
#
# Overall Statistics
#
#  Accuracy : 0.8053
#    95% CI : (0.7715, 0.8361)
#
#     Kappa : 0.6704
#
# Statistics by Class:
#
#                           Class: Clear_Cut Class: Forest Class: Pasture
# Prod Acc (Sensitivity)              0.8546        0.8629         0.6414
# Specificity                         0.8067        0.9378         0.9219
# User Acc (Pos Pred Value)           0.8471        0.7810         0.7209
# Neg Pred Value                      0.8158        0.9638         0.8910

prodes.tb$predicted <- sits_predict(dist_prodes.tb, model.glm2)
prodes.tb %>% dplyr::mutate(reference = label) %>% sits_accuracy(conv2.lst)
# Confusion Matrix and Statistics
#
#             Reference
# Prediction   Forest Non_Forest
#   Forest        107         30
#   Non_Forest     17        452
#
#              Accuracy : 0.9224
#                95% CI : (0.8982, 0.9425)
#
#                 Kappa : 0.7707
#
#      Prod Acc  Forest : 0.8629
#  Prod Acc  Non_Forest : 0.9378
#      User Acc  Forest : 0.7810
#  User Acc  Non_Forest : 0.9638


#    ___  _  _  ___     ___  ___   ___  ___  ___  _____
#   | _ \| \| ||   \   | __|/ _ \ | _ \| __|/ __||_   _|
#   |   /| .` || |) |_ | _|| (_) ||   /| _| \__ \  | |
#   |_|_\|_|\_||___/(_)|_|  \___/ |_|_\|___||___/  |_|
#

# random forest returns 1.0 of accuracy if all data is used for training. Reducing data training to 95%
model.rfor <- prodes.tb %>% sits_sample(frac = 0.95) %>%
    sits_distances(patt_prodes.tb, dist_method = sits_TWDTW_distances(multicores = 10)) %>%
    sits_train(sits_rfor())

# test on all data set
prodes.tb$predicted <- sits_predict(dist_prodes.tb, model.rfor)
prodes.tb %>% dplyr::mutate(reference = label) %>% sits_accuracy(conv.lst)
# Confusion Matrix and Statistics
#
#            Reference
# Prediction  Clear_Cut Forest Pasture
#   Clear_Cut       304     14      46
#   Forest           14     95       5
#   Pasture          19     15      94
#
# Overall Statistics
#
#  Accuracy : 0.8135
#    95% CI : (0.7802, 0.8438)
#
#     Kappa : 0.6768
#
# Statistics by Class:
#
#                           Class: Clear_Cut Class: Forest Class: Pasture
# Prod Acc (Sensitivity)              0.9021        0.7661         0.6483
# Specificity                         0.7770        0.9606         0.9262
# User Acc (Pos Pred Value)           0.8352        0.8333         0.7344
# Neg Pred Value                      0.8636        0.9411         0.8933

prodes.tb$predicted <- sits_predict(dist_prodes.tb, model.rfor)
prodes.tb %>% dplyr::mutate(reference = label) %>% sits_accuracy(conv2.lst)
# Confusion Matrix and Statistics
#
#             Reference
# Prediction   Forest Non_Forest
#   Forest         95         19
#   Non_Forest     29        463
#
#              Accuracy : 0.9208
#                95% CI : (0.8964, 0.941)
#
#                 Kappa : 0.7491
#
#      Prod Acc  Forest : 0.7661
#  Prod Acc  Non_Forest : 0.9606
#      User Acc  Forest : 0.8333
#  User Acc  Non_Forest : 0.9411



# result    PATT.DIST.   intraCLUST.
# - SVM:      0.9769 *      0.9950 *
# - LDA:      0.9092        0.9307
# - QDA:      0.9323 *      0.9802 *
# - MLR:      0.9175        0.9406
# - LASSO:    0.9191        0.9323
# - RIDGE:    0.9241        0.9224
# - RFOR:     0.9125        0.9208

#
#
#  ###################################################################


#    _  __       _____  ___   _      ____
#   | |/ /      |  ___|/ _ \ | |    |  _ \
#   | ' / _____ | |_  | | | || |    | | | |
#   | . \|_____||  _| | |_| || |___ | |_| |
#   |_|\_\      |_|    \___/ |_____||____/
#

# The best "one-shot" results were: SVM and QDA.
# We processed k-fold validation for both methods

#    ___ __   __ __  __
#   / __|\ \ / /|  \/  |
#   \__ \ \ V / | |\/| |
#   |___/  \_/  |_|  |_|
#
svm.kfold <-
    prodes.tb %>% sits_kfold_validate(folds = 5,
                                      pt_method = sits_intracluster_patterns(k = 3),
                                      dist_method = sits_TWDTW_distances(multicores = 5),
                                      tr_method = sits_svm(formula = sits_formula_linear(),
                                                           kernel = "radial"),
                                      multicores = 2)
svm.kfold %>% sits_accuracy(conv.lst)
# Confusion Matrix and Statistics
#
#            Reference
# Prediction  Clear_Cut Forest Pasture
#   Clear_Cut       278     14      43
#   Forest           19    100      14
#   Pasture          40     10      88
#
# Overall Statistics
#
#  Accuracy : 0.769
#    95% CI : (0.7333, 0.802)
#
#     Kappa : 0.6105
#
# Statistics by Class:
#
#                           Class: Clear_Cut Class: Forest Class: Pasture
# Prod Acc (Sensitivity)              0.8249        0.8065         0.6069
# Specificity                         0.7881        0.9315         0.8915
# User Acc (Pos Pred Value)           0.8299        0.7519         0.6377
# Neg Pred Value                      0.7823        0.9493         0.8782

svm.kfold %>% sits_accuracy(conv2.lst)
# Confusion Matrix and Statistics
#
#             Reference
# Prediction   Forest Non_Forest
#   Forest        100         33
#   Non_Forest     24        449
#
#              Accuracy : 0.9059
#                95% CI : (0.8799, 0.928)
#
#                 Kappa : 0.7186
#
#      Prod Acc  Forest : 0.8065
#  Prod Acc  Non_Forest : 0.9315
#      User Acc  Forest : 0.7519
#  User Acc  Non_Forest : 0.9493



#     ___    ___      _
#    / _ \  |   \    /_\
#   | (_) | | |) |  / _ \
#    \__\_\ |___/  /_/ \_\
#
qda.kfold <-
    prodes.tb %>% sits_kfold_validate(folds = 5,
                                      pt_method = sits_intracluster_patterns(k = 3),
                                      dist_method = sits_TWDTW_distances(multicores = 5),
                                      tr_method = sits_qda(formula = sits_formula_linear()),
                                      multicores = 2)
qda.kfold %>% sits_accuracy(conv.lst)
# Confusion Matrix and Statistics
#
#            Reference
# Prediction  Clear_Cut Forest Pasture
#   Clear_Cut       270     35      77
#   Forest           20     72       8
#   Pasture          47     17      60
#
# Overall Statistics
#
#  Accuracy : 0.6634
#    95% CI : (0.6242, 0.7009)
#
#     Kappa : 0.406
#
# Statistics by Class:
#
#                           Class: Clear_Cut Class: Forest Class: Pasture
# Prod Acc (Sensitivity)              0.8012        0.5806         0.4138
# Specificity                         0.5836        0.9419         0.8612
# User Acc (Pos Pred Value)           0.7068        0.7200         0.4839
# Neg Pred Value                      0.7009        0.8972         0.8237

qda.kfold %>% sits_accuracy(conv2.lst)
# Confusion Matrix and Statistics
#
#             Reference
# Prediction   Forest Non_Forest
#   Forest         72         28
#   Non_Forest     52        454
#
#              Accuracy : 0.868
#                95% CI : (0.8384, 0.8939)
#
#                 Kappa : 0.563
#
#      Prod Acc  Forest : 0.5806
#  Prod Acc  Non_Forest : 0.9419
#      User Acc  Forest : 0.7200
#  User Acc  Non_Forest : 0.8972


# result           K-FOLD PATT.DIST.          K-FOLD intraCLUST
#  - SVM:  0.8960 (0.8690, 0.9192)      0.9059 (0.8799, 0.9280) *
#  - QDA:  0.9026 (0.8762, 0.9251) *    0.8680 (0.8384, 0.8939)


#
#
#  ###################################################################
#  ###################################################################
#  ###################################################################




#     _____  _      _    _   _____  _______  ______  _____
#    / ____|| |    | |  | | / ____||__   __||  ____||  __ \
#   | |     | |    | |  | || (___     | |   | |__   | |__) |
#   | |     | |    | |  | | \___ \    | |   |  __|  |  _  /
#   | |____ | |____| |__| | ____) |   | |   | |____ | | \ \
#    \_____||______|\____/ |_____/    |_|   |______||_|  \_\
#
#

# make clusters
library(dtwclust)

sits_cluster_patterns <- function(data.tb = NULL, k = 3){

    result_fun1 <- function(data.tb){
        dendro.obj <- data.tb %>% sits_dendrogram()

        sits_cluster(data.tb, dendro.obj, k) %>%
            dplyr::mutate(label = paste0("Cluster", cluster)) %>%
            sits_foreach(cluster, fun = function(tb) tb %>% sits_gam())
    }

    result_fun2 <- function(k = k){
        sits_cluster(data.tb, dendro.obj, k) %>%
            dplyr::mutate(label = paste0("Cluster", cluster)) %>%
            sits_foreach(cluster, fun = function(tb) tb %>% sits_gam())
    }

    dendro.obj <- NULL
    if (!is.null(data.tb)){
        dendro.obj <- data.tb %>% sits_dendrogram()
        return(result_fun2)
    }
    return(result_fun1)
}

# create patterns and plot them
patt_cluster_gen <- prodes.tb %>% sits_cluster_patterns()
patt_prodes.tb <- patt_cluster_gen(12)
sits_plot_patterns(patt_prodes.tb)

# create distances
dist_prodes.tb <- prodes.tb %>% sits_distances(patt_prodes.tb, dist_method = sits_TWDTW_distances(multicores = 10))


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
prodes.tb$predicted <- sits_predict(dist_prodes.tb, model.svm)
prodes.tb %>% dplyr::mutate(reference = label) %>% sits_accuracy(conv.lst)
# Confusion Matrix and Statistics
#
#            Reference
# Prediction  Clear_Cut Forest Pasture
#   Clear_Cut       333      2       3
#   Forest            1    122       0
#   Pasture           3      0     142
#
# Overall Statistics
#
#  Accuracy : 0.9851
#    95% CI : (0.972, 0.9932)
#
#     Kappa : 0.9749
#
# Statistics by Class:
#
#                           Class: Clear_Cut Class: Forest Class: Pasture
# Prod Acc (Sensitivity)              0.9881        0.9839         0.9793
# Specificity                         0.9814        0.9979         0.9935
# User Acc (Pos Pred Value)           0.9852        0.9919         0.9793
# Neg Pred Value                      0.9851        0.9959         0.9935

prodes.tb$predicted <- sits_predict(dist_prodes.tb, model.svm)
prodes.tb %>% dplyr::mutate(reference = label) %>% sits_accuracy(conv2.lst)
# Confusion Matrix and Statistics
#
#             Reference
# Prediction   Forest Non_Forest
#   Forest        122          1
#   Non_Forest      2        481
#
#              Accuracy : 0.995
#                95% CI : (0.9856, 0.999)
#
#                 Kappa : 0.9847
#
#      Prod Acc  Forest : 0.9839
#  Prod Acc  Non_Forest : 0.9979
#      User Acc  Forest : 0.9919
#  User Acc  Non_Forest : 0.9959


#    _     ___    _
#   | |   |   \  /_\
#   | |__ | |) |/ _ \
#   |____||___//_/ \_\
#
model.lda <- sits_train(dist_prodes.tb, sits_lda(formula = sits_formula_linear()))
prodes.tb$predicted <- sits_predict(dist_prodes.tb, model.lda)
prodes.tb %>% dplyr::mutate(reference = label) %>% sits_accuracy(conv.lst)
# Confusion Matrix and Statistics
#
#            Reference
# Prediction  Clear_Cut Forest Pasture
#   Clear_Cut       279     11      36
#   Forest           17    106      12
#   Pasture          41      7      97
#
# Overall Statistics
#
#  Accuracy : 0.7954
#    95% CI : (0.761, 0.8268)
#
#     Kappa : 0.6578
#
# Statistics by Class:
#
#                           Class: Clear_Cut Class: Forest Class: Pasture
# Prod Acc (Sensitivity)              0.8279        0.8548         0.6690
# Specificity                         0.8253        0.9398         0.8959
# User Acc (Pos Pred Value)           0.8558        0.7852         0.6690
# Neg Pred Value                      0.7929        0.9618         0.8959

prodes.tb$predicted <- sits_predict(dist_prodes.tb, model.lda)
prodes.tb %>% dplyr::mutate(reference = label) %>% sits_accuracy(conv2.lst)
# Confusion Matrix and Statistics
#
#             Reference
# Prediction   Forest Non_Forest
#   Forest        106         29
#   Non_Forest     18        453
#
#              Accuracy : 0.9224
#                95% CI : (0.8982, 0.9425)
#
#                 Kappa : 0.7693
#
#      Prod Acc  Forest : 0.8548
#  Prod Acc  Non_Forest : 0.9398
#      User Acc  Forest : 0.7852
#  User Acc  Non_Forest : 0.9618


#     ___   ___    _
#    / _ \ |   \  /_\
#   | (_) || |) |/ _ \
#    \__\_\|___//_/ \_\
#
model.qda <- sits_train(dist_prodes.tb, sits_qda(formula = sits_formula_linear()))
prodes.tb$predicted <- sits_predict(dist_prodes.tb, model.qda)
prodes.tb %>% dplyr::mutate(reference = label) %>% sits_accuracy(conv.lst)
# Confusion Matrix and Statistics
#
#            Reference
# Prediction  Clear_Cut Forest Pasture
#   Clear_Cut       325      3       3
#   Forest            4    120       4
#   Pasture           8      1     138
#
# Overall Statistics
#
#  Accuracy : 0.962
#    95% CI : (0.9436, 0.9758)
#
#     Kappa : 0.9362
#
# Statistics by Class:
#
#                           Class: Clear_Cut Class: Forest Class: Pasture
# Prod Acc (Sensitivity)              0.9644        0.9677         0.9517
# Specificity                         0.9777        0.9834         0.9805
# User Acc (Pos Pred Value)           0.9819        0.9375         0.9388
# Neg Pred Value                      0.9564        0.9916         0.9847

prodes.tb$predicted <- sits_predict(dist_prodes.tb, model.qda)
prodes.tb %>% dplyr::mutate(reference = label) %>% sits_accuracy(conv2.lst)
# Confusion Matrix and Statistics
#
#             Reference
# Prediction   Forest Non_Forest
#   Forest        120          8
#   Non_Forest      4        474
#
#              Accuracy : 0.9802
#                95% CI : (0.9657, 0.9897)
#
#                 Kappa : 0.9399
#
#      Prod Acc  Forest : 0.9677
#  Prod Acc  Non_Forest : 0.9834
#      User Acc  Forest : 0.9375
#  User Acc  Non_Forest : 0.9916


#    __  __  _     ___
#   |  \/  || |   | _ \
#   | |\/| || |__ |   /
#   |_|  |_||____||_|_\
#
model.mlr <- sits_train(dist_prodes.tb, sits_mlr(formula = sits_formula_linear()))
prodes.tb$predicted <- sits_predict(dist_prodes.tb, model.mlr)
prodes.tb %>% dplyr::mutate(reference = label) %>% sits_accuracy(conv.lst)
# Confusion Matrix and Statistics
#
#            Reference
# Prediction  Clear_Cut Forest Pasture
#   Clear_Cut       290     12      31
#   Forest           14    106      11
#   Pasture          33      6     103
#
# Overall Statistics
#
#  Accuracy : 0.8234
#    95% CI : (0.7907, 0.853)
#
#     Kappa : 0.7028
#
# Statistics by Class:
#
#                           Class: Clear_Cut Class: Forest Class: Pasture
# Prod Acc (Sensitivity)              0.8605        0.8548         0.7103
# Specificity                         0.8401        0.9481         0.9154
# User Acc (Pos Pred Value)           0.8709        0.8092         0.7254
# Neg Pred Value                      0.8278        0.9621         0.9095

prodes.tb$predicted <- sits_predict(dist_prodes.tb, model.mlr)
prodes.tb %>% dplyr::mutate(reference = label) %>% sits_accuracy(conv2.lst)
# Confusion Matrix and Statistics
#
#             Reference
# Prediction   Forest Non_Forest
#   Forest        106         25
#   Non_Forest     18        457
#
#              Accuracy : 0.929
#                95% CI : (0.9056, 0.9482)
#
#                 Kappa : 0.7865
#
#      Prod Acc  Forest : 0.8548
#  Prod Acc  Non_Forest : 0.9481
#      User Acc  Forest : 0.8092
#  User Acc  Non_Forest : 0.9621


#    _       _    ___  ___   ___
#   | |     /_\  / __|/ __| / _ \
#   | |__  / _ \ \__ \\__ \| (_) |
#   |____|/_/ \_\|___/|___/ \___/
#
model.glm1 <- sits_train(dist_prodes.tb, sits_glm(alpha = 1))
get("result_glm", envir = environment(model.glm1))$lambda.min
# [1] 0.004342837

prodes.tb$predicted <- sits_predict(dist_prodes.tb, model.glm1)
prodes.tb %>% dplyr::mutate(reference = label) %>% sits_accuracy(conv.lst)
# Confusion Matrix and Statistics
#
#            Reference
# Prediction  Clear_Cut Forest Pasture
#   Clear_Cut       281     12      36
#   Forest           15    106      14
#   Pasture          41      6      95
#
# Overall Statistics
#
#  Accuracy : 0.7954
#    95% CI : (0.761, 0.8268)
#
#     Kappa : 0.6569
#
# Statistics by Class:
#
#                           Class: Clear_Cut Class: Forest Class: Pasture
# Prod Acc (Sensitivity)              0.8338        0.8548         0.6552
# Specificity                         0.8216        0.9398         0.8980
# User Acc (Pos Pred Value)           0.8541        0.7852         0.6690
# Neg Pred Value                      0.7978        0.9618         0.8922

prodes.tb$predicted <- sits_predict(dist_prodes.tb, model.glm1)
prodes.tb %>% dplyr::mutate(reference = label) %>% sits_accuracy(conv2.lst))
# Confusion Matrix and Statistics
#
#             Reference
# Prediction   Forest Non_Forest
#   Forest        106         29
#   Non_Forest     18        453
#
#              Accuracy : 0.9224
#                95% CI : (0.8982, 0.9425)
#
#                 Kappa : 0.7693
#
#      Prod Acc  Forest : 0.8548
#  Prod Acc  Non_Forest : 0.9398
#      User Acc  Forest : 0.7852
#  User Acc  Non_Forest : 0.9618


#    ___  ___  ___    ___  ___
#   | _ \|_ _||   \  / __|| __|
#   |   / | | | |) || (_ || _|
#   |_|_\|___||___/  \___||___|
#
model.glm2 <- sits_train(dist_prodes.tb, sits_glm(alpha = 0))
get("result_glm", envir = environment(model.glm2))$lambda.min
# [1] 0.02372178

prodes.tb$predicted <- sits_predict(dist_prodes.tb, model.glm2)
prodes.tb %>% dplyr::mutate(reference = label) %>% sits_accuracy(conv.lst)
# Confusion Matrix and Statistics
#
#            Reference
# Prediction  Clear_Cut Forest Pasture
#   Clear_Cut       282      9      35
#   Forest           18    109      14
#   Pasture          37      6      96
#
# Overall Statistics
#
#  Accuracy : 0.8036
#    95% CI : (0.7697, 0.8345)
#
#     Kappa : 0.6718
#
# Statistics by Class:
#
#                           Class: Clear_Cut Class: Forest Class: Pasture
# Prod Acc (Sensitivity)              0.8368        0.8790         0.6621
# Specificity                         0.8364        0.9336         0.9067
# User Acc (Pos Pred Value)           0.8650        0.7730         0.6906
# Neg Pred Value                      0.8036        0.9677         0.8951

prodes.tb$predicted <- sits_predict(dist_prodes.tb, model.glm2)
prodes.tb %>% dplyr::mutate(reference = label) %>% sits_accuracy(conv2.lst)
# Confusion Matrix and Statistics
#
#             Reference
# Prediction   Forest Non_Forest
#   Forest        109         32
#   Non_Forest     15        450
#
#              Accuracy : 0.9224
#                95% CI : (0.8982, 0.9425)
#
#                 Kappa : 0.7733
#
#      Prod Acc  Forest : 0.8790
#  Prod Acc  Non_Forest : 0.9336
#      User Acc  Forest : 0.7730
#  User Acc  Non_Forest : 0.9677


#    ___  _  _  ___     ___  ___   ___  ___  ___  _____
#   | _ \| \| ||   \   | __|/ _ \ | _ \| __|/ __||_   _|
#   |   /| .` || |) |_ | _|| (_) ||   /| _| \__ \  | |
#   |_|_\|_|\_||___/(_)|_|  \___/ |_|_\|___||___/  |_|
#

# random forest returns 1.0 of accuracy if all data is used for training. Reducing data training to 95%
model.rfor <- prodes.tb %>% sits_sample(frac = 0.95) %>%
    sits_distances(patt_prodes.tb, dist_method = sits_TWDTW_distances(multicores = 10)) %>%
    sits_train(sits_rfor())

# test on all data set
prodes.tb$predicted <- sits_predict(dist_prodes.tb, model.rfor)
prodes.tb %>% dplyr::mutate(reference = label) %>% sits_accuracy(conv.lst)
# Confusion Matrix and Statistics
#
#            Reference
# Prediction  Clear_Cut Forest Pasture
#   Clear_Cut       300     12      43
#   Forest           14    101      12
#   Pasture          23     11      90
#
# Overall Statistics
#
#  Accuracy : 0.8102
#    95% CI : (0.7767, 0.8407)
#
#     Kappa : 0.6742
#
# Statistics by Class:
#
#                           Class: Clear_Cut Class: Forest Class: Pasture
# Prod Acc (Sensitivity)              0.8902        0.8145         0.6207
# Specificity                         0.7955        0.9461         0.9262
# User Acc (Pos Pred Value)           0.8451        0.7953         0.7258
# Neg Pred Value                      0.8526        0.9520         0.8859

prodes.tb$predicted <- sits_predict(dist_prodes.tb, model.rfor)
prodes.tb %>% dplyr::mutate(reference = label) %>% sits_accuracy(conv2.lst)
# Confusion Matrix and Statistics
#
#             Reference
# Prediction   Forest Non_Forest
#   Forest        101         26
#   Non_Forest     23        456
#
#              Accuracy : 0.9191
#                95% CI : (0.8945, 0.9396)
#
#                 Kappa : 0.7538
#
#      Prod Acc  Forest : 0.8145
#  Prod Acc  Non_Forest : 0.9461
#      User Acc  Forest : 0.7953
#  User Acc  Non_Forest : 0.9520



# result    PATT.DIST.   intraCLUST.     CLUST
# - SVM:      0.9769 *      0.9950 *     0.9950 *
# - LDA:      0.9092        0.9307       0.9224
# - QDA:      0.9323 *      0.9802 *     0.9802 *
# - MLR:      0.9175        0.9406       0.9290
# - LASSO:    0.9191        0.9323       0.9224
# - RIDGE:    0.9241        0.9224       0.9224
# - RFOR:     0.9125        0.9208       0.9191

#
#
#  ###################################################################


#    _  __       _____  ___   _      ____
#   | |/ /      |  ___|/ _ \ | |    |  _ \
#   | ' / _____ | |_  | | | || |    | | | |
#   | . \|_____||  _| | |_| || |___ | |_| |
#   |_|\_\      |_|    \___/ |_____||____/
#

# The best "one-shot" results were: SVM and QDA.
# We processed k-fold validation for both methods

#    ___ __   __ __  __
#   / __|\ \ / /|  \/  |
#   \__ \ \ V / | |\/| |
#   |___/  \_/  |_|  |_|
#
svm.kfold <-
    prodes.tb %>% sits_kfold_validate(folds = 5,
                                      pt_method = sits_cluster_patterns(k = 12),
                                      dist_method = sits_TWDTW_distances(multicores = 4),
                                      tr_method = sits_svm(formula = sits_formula_linear(),
                                                           kernel = "radial"),
                                      multicores = 4)
svm.kfold %>% sits_accuracy(conv.lst)
# Confusion Matrix and Statistics
#
#            Reference
# Prediction  Clear_Cut Forest Pasture
#   Clear_Cut       289     14      48
#   Forest           14     96      11
#   Pasture          34     14      86
#
# Overall Statistics
#
#  Accuracy : 0.7772
#    95% CI : (0.742, 0.8098)
#
#     Kappa : 0.6186
#
# Statistics by Class:
#
#                           Class: Clear_Cut Class: Forest Class: Pasture
# Prod Acc (Sensitivity)              0.8576        0.7742         0.5931
# Specificity                         0.7695        0.9481         0.8959
# User Acc (Pos Pred Value)           0.8234        0.7934         0.6418
# Neg Pred Value                      0.8118        0.9423         0.8750

svm.kfold %>% sits_accuracy(conv2.lst)
# Confusion Matrix and Statistics
#
#             Reference
# Prediction   Forest Non_Forest
#   Forest         96         25
#   Non_Forest     28        457
#
#              Accuracy : 0.9125
#                95% CI : (0.8872, 0.9338)
#
#                 Kappa : 0.7289
#
#      Prod Acc  Forest : 0.7742
#  Prod Acc  Non_Forest : 0.9481
#      User Acc  Forest : 0.7934
#  User Acc  Non_Forest : 0.9423



#     ___    ___      _
#    / _ \  |   \    /_\
#   | (_) | | |) |  / _ \
#    \__\_\ |___/  /_/ \_\
#
qda.kfold <-
    prodes.tb %>% sits_kfold_validate(folds = 5,
                                      pt_method = sits_cluster_patterns(k = 12),
                                      dist_method = sits_TWDTW_distances(multicores = 4),
                                      tr_method = sits_qda(formula = sits_formula_linear()),
                                      multicores = 4)
qda.kfold %>% sits_accuracy(conv.lst)
# Confusion Matrix and Statistics
#
#            Reference
# Prediction  Clear_Cut Forest Pasture
#   Clear_Cut       281     33      59
#   Forest           20     79       7
#   Pasture          36     12      79
#
# Overall Statistics
#
#  Accuracy : 0.7244
#    95% CI : (0.687, 0.7597)
#
#     Kappa : 0.518
#
# Statistics by Class:
#
#                           Class: Clear_Cut Class: Forest Class: Pasture
# Prod Acc (Sensitivity)              0.8338        0.6371         0.5448
# Specificity                         0.6580        0.9440         0.8959
# User Acc (Pos Pred Value)           0.7534        0.7453         0.6220
# Neg Pred Value                      0.7597        0.9100         0.8622

qda.kfold %>% sits_accuracy(conv2.lst)
# Confusion Matrix and Statistics
#
#             Reference
# Prediction   Forest Non_Forest
#   Forest         79         27
#   Non_Forest     45        455
#
#              Accuracy : 0.8812
#                95% CI : (0.8527, 0.9059)
#
#                 Kappa : 0.6142
#
#      Prod Acc  Forest : 0.6371
#  Prod Acc  Non_Forest : 0.9440
#      User Acc  Forest : 0.7453
#  User Acc  Non_Forest : 0.9100


# result           K-FOLD PATT.DIST.            K-FOLD intraCLUST                 K-FOLD CLUST
#  - SVM:  0.8960 (0.8690, 0.9192)      0.9059 (0.8799, 0.9280) *    0.9125 (0.8872, 0.9338) *
#  - QDA:  0.9026 (0.8762, 0.9251) *    0.8680 (0.8384, 0.8939)      0.8812 (0.8527, 0.9059)

#
#
#  ###################################################################
#  ###################################################################
#  ###################################################################





#    _____               _   _   _____        _____               _______   _______
#   |  __ \      /\     | \ | | |  __ \      |  __ \      /\     |__   __| |__   __|
#   | |__) |    /  \    |  \| | | |  | |     | |__) |    /  \       | |       | |
#   |  _  /    / /\ \   | . ` | | |  | |     |  ___/    / /\ \      | |       | |
#   | | \ \   / ____ \  | |\  | | |__| |  _  | |       / ____ \     | |       | |     _
#   |_|  \_\ /_/    \_\ |_| \_| |_____/  (_) |_|      /_/    \_\    |_|       |_|    (_)
#
#

sits_random_patterns <- function(data.tb = NULL, n){

    result_fun <- function(data.tb){
        values <- seq(n) %>% purrr::map(function(i){
            ts.tb <- prodes.tb$time_series[[1]]
            ts.tb[2:NCOL(ts.tb)] <- ts.tb[2:NCOL(ts.tb)] %>%
                purrr::map(function(band){
                    rnd <- cumsum(runif(n = 5 + length(band), min = -0.4, max = 0.4)[-5:0])
                })
            ts.tb
        })
        sits_tibble() %>%
            tibble::add_row(longitude = 0.0,
                            latitude = 0.0,
                            start_date = data.tb$start_date[[1]],
                            end_date = data.tb$end_date[[1]],
                            label = paste0("Random.", seq(n)),
                            coverage = "Random",
                            time_series = values)
    }

    if (is.null(data.tb))
        return(result_fun)
    return(result_fun(data.tb))
}

# create patterns and plot them
patt_random_gen <- sits_random_patterns(n = 2)
patt_prodes.tb <- prodes.tb %>% patt_random_gen()
sits_plot_patterns(patt_prodes.tb)

# create distances
dist_prodes.tb <- prodes.tb %>% sits_distances(patt_prodes.tb, dist_method = sits_TWDTW_distances(multicores = 10))


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
prodes.tb$predicted <- sits_predict(dist_prodes.tb, model.svm)
prodes.tb %>% dplyr::mutate(reference = label) %>% sits_accuracy(conv.lst)
# Confusion Matrix and Statistics
#
#            Reference
# Prediction  Clear_Cut Forest Pasture
#   Clear_Cut       326      3      11
#   Forest           10    120       0
#   Pasture           1      1     134
#
# Overall Statistics
#
#  Accuracy : 0.9571
#    95% CI : (0.9378, 0.9718)
#
#     Kappa : 0.9273
#
# Statistics by Class:
#
#                           Class: Clear_Cut Class: Forest Class: Pasture
# Prod Acc (Sensitivity)              0.9674        0.9677         0.9241
# Specificity                         0.9480        0.9793         0.9957
# User Acc (Pos Pred Value)           0.9588        0.9231         0.9853
# Neg Pred Value                      0.9586        0.9916         0.9766

prodes.tb$predicted <- sits_predict(dist_prodes.tb, model.svm)
prodes.tb %>% dplyr::mutate(reference = label) %>% sits_accuracy(conv2.lst)
# Confusion Matrix and Statistics
#
#             Reference
# Prediction   Forest Non_Forest
#   Forest        120         10
#   Non_Forest      4        472
#
#              Accuracy : 0.9769
#                95% CI : (0.9615, 0.9873)
#
#                 Kappa : 0.9303
#
#      Prod Acc  Forest : 0.9677
#  Prod Acc  Non_Forest : 0.9793
#      User Acc  Forest : 0.9231
#  User Acc  Non_Forest : 0.9916


#    _     ___    _
#   | |   |   \  /_\
#   | |__ | |) |/ _ \
#   |____||___//_/ \_\
#
model.lda <- sits_train(dist_prodes.tb, sits_lda(formula = sits_formula_linear()))
prodes.tb$predicted <- sits_predict(dist_prodes.tb, model.lda)
prodes.tb %>% dplyr::mutate(reference = label) %>% sits_accuracy(conv.lst)
# Confusion Matrix and Statistics
#
#            Reference
# Prediction  Clear_Cut Forest Pasture
#   Clear_Cut       279     11      36
#   Forest           17    106      12
#   Pasture          41      7      97
#
# Overall Statistics
#
#  Accuracy : 0.7954
#    95% CI : (0.761, 0.8268)
#
#     Kappa : 0.6578
#
# Statistics by Class:
#
#                           Class: Clear_Cut Class: Forest Class: Pasture
# Prod Acc (Sensitivity)              0.8279        0.8548         0.6690
# Specificity                         0.8253        0.9398         0.8959
# User Acc (Pos Pred Value)           0.8558        0.7852         0.6690
# Neg Pred Value                      0.7929        0.9618         0.8959

prodes.tb$predicted <- sits_predict(dist_prodes.tb, model.lda)
prodes.tb %>% dplyr::mutate(reference = label) %>% sits_accuracy(conv2.lst)
# Confusion Matrix and Statistics
#
#             Reference
# Prediction   Forest Non_Forest
#   Forest        106         29
#   Non_Forest     18        453
#
#              Accuracy : 0.9224
#                95% CI : (0.8982, 0.9425)
#
#                 Kappa : 0.7693
#
#      Prod Acc  Forest : 0.8548
#  Prod Acc  Non_Forest : 0.9398
#      User Acc  Forest : 0.7852
#  User Acc  Non_Forest : 0.9618


#     ___   ___    _
#    / _ \ |   \  /_\
#   | (_) || |) |/ _ \
#    \__\_\|___//_/ \_\
#
model.qda <- sits_train(dist_prodes.tb, sits_qda(formula = sits_formula_linear()))
prodes.tb$predicted <- sits_predict(dist_prodes.tb, model.qda)
prodes.tb %>% dplyr::mutate(reference = label) %>% sits_accuracy(conv.lst)
# Confusion Matrix and Statistics
#
#            Reference
# Prediction  Clear_Cut Forest Pasture
#   Clear_Cut       325      3       3
#   Forest            4    120       4
#   Pasture           8      1     138
#
# Overall Statistics
#
#  Accuracy : 0.962
#    95% CI : (0.9436, 0.9758)
#
#     Kappa : 0.9362
#
# Statistics by Class:
#
#                           Class: Clear_Cut Class: Forest Class: Pasture
# Prod Acc (Sensitivity)              0.9644        0.9677         0.9517
# Specificity                         0.9777        0.9834         0.9805
# User Acc (Pos Pred Value)           0.9819        0.9375         0.9388
# Neg Pred Value                      0.9564        0.9916         0.9847

prodes.tb$predicted <- sits_predict(dist_prodes.tb, model.qda)
prodes.tb %>% dplyr::mutate(reference = label) %>% sits_accuracy(conv2.lst)
# Confusion Matrix and Statistics
#
#             Reference
# Prediction   Forest Non_Forest
#   Forest        120          8
#   Non_Forest      4        474
#
#              Accuracy : 0.9802
#                95% CI : (0.9657, 0.9897)
#
#                 Kappa : 0.9399
#
#      Prod Acc  Forest : 0.9677
#  Prod Acc  Non_Forest : 0.9834
#      User Acc  Forest : 0.9375
#  User Acc  Non_Forest : 0.9916


#    __  __  _     ___
#   |  \/  || |   | _ \
#   | |\/| || |__ |   /
#   |_|  |_||____||_|_\
#
model.mlr <- sits_train(dist_prodes.tb, sits_mlr(formula = sits_formula_linear()))
prodes.tb$predicted <- sits_predict(dist_prodes.tb, model.mlr)
prodes.tb %>% dplyr::mutate(reference = label) %>% sits_accuracy(conv.lst)
# Confusion Matrix and Statistics
#
#            Reference
# Prediction  Clear_Cut Forest Pasture
#   Clear_Cut       290     12      31
#   Forest           14    106      11
#   Pasture          33      6     103
#
# Overall Statistics
#
#  Accuracy : 0.8234
#    95% CI : (0.7907, 0.853)
#
#     Kappa : 0.7028
#
# Statistics by Class:
#
#                           Class: Clear_Cut Class: Forest Class: Pasture
# Prod Acc (Sensitivity)              0.8605        0.8548         0.7103
# Specificity                         0.8401        0.9481         0.9154
# User Acc (Pos Pred Value)           0.8709        0.8092         0.7254
# Neg Pred Value                      0.8278        0.9621         0.9095

prodes.tb$predicted <- sits_predict(dist_prodes.tb, model.mlr)
prodes.tb %>% dplyr::mutate(reference = label) %>% sits_accuracy(conv2.lst)
# Confusion Matrix and Statistics
#
#             Reference
# Prediction   Forest Non_Forest
#   Forest        106         25
#   Non_Forest     18        457
#
#              Accuracy : 0.929
#                95% CI : (0.9056, 0.9482)
#
#                 Kappa : 0.7865
#
#      Prod Acc  Forest : 0.8548
#  Prod Acc  Non_Forest : 0.9481
#      User Acc  Forest : 0.8092
#  User Acc  Non_Forest : 0.9621


#    _       _    ___  ___   ___
#   | |     /_\  / __|/ __| / _ \
#   | |__  / _ \ \__ \\__ \| (_) |
#   |____|/_/ \_\|___/|___/ \___/
#
model.glm1 <- sits_train(dist_prodes.tb, sits_glm(alpha = 1))
get("result_glm", envir = environment(model.glm1))$lambda.min
# [1] 0.004342837

prodes.tb$predicted <- sits_predict(dist_prodes.tb, model.glm1)
prodes.tb %>% dplyr::mutate(reference = label) %>% sits_accuracy(conv.lst)
# Confusion Matrix and Statistics
#
#            Reference
# Prediction  Clear_Cut Forest Pasture
#   Clear_Cut       281     12      36
#   Forest           15    106      14
#   Pasture          41      6      95
#
# Overall Statistics
#
#  Accuracy : 0.7954
#    95% CI : (0.761, 0.8268)
#
#     Kappa : 0.6569
#
# Statistics by Class:
#
#                           Class: Clear_Cut Class: Forest Class: Pasture
# Prod Acc (Sensitivity)              0.8338        0.8548         0.6552
# Specificity                         0.8216        0.9398         0.8980
# User Acc (Pos Pred Value)           0.8541        0.7852         0.6690
# Neg Pred Value                      0.7978        0.9618         0.8922

prodes.tb$predicted <- sits_predict(dist_prodes.tb, model.glm1)
prodes.tb %>% dplyr::mutate(reference = label) %>% sits_accuracy(conv2.lst)
# Confusion Matrix and Statistics
#
#             Reference
# Prediction   Forest Non_Forest
#   Forest        106         29
#   Non_Forest     18        453
#
#              Accuracy : 0.9224
#                95% CI : (0.8982, 0.9425)
#
#                 Kappa : 0.7693
#
#      Prod Acc  Forest : 0.8548
#  Prod Acc  Non_Forest : 0.9398
#      User Acc  Forest : 0.7852
#  User Acc  Non_Forest : 0.9618


#    ___  ___  ___    ___  ___
#   | _ \|_ _||   \  / __|| __|
#   |   / | | | |) || (_ || _|
#   |_|_\|___||___/  \___||___|
#
model.glm2 <- sits_train(dist_prodes.tb, sits_glm(alpha = 0))
get("result_glm", envir = environment(model.glm2))$lambda.min
# [1] 0.02372178

prodes.tb$predicted <- sits_predict(dist_prodes.tb, model.glm2)
prodes.tb %>% dplyr::mutate(reference = label) %>% sits_accuracy(conv.lst)
# Confusion Matrix and Statistics
#
#            Reference
# Prediction  Clear_Cut Forest Pasture
#   Clear_Cut       282      9      35
#   Forest           18    109      14
#   Pasture          37      6      96
#
# Overall Statistics
#
#  Accuracy : 0.8036
#    95% CI : (0.7697, 0.8345)
#
#     Kappa : 0.6718
#
# Statistics by Class:
#
#                           Class: Clear_Cut Class: Forest Class: Pasture
# Prod Acc (Sensitivity)              0.8368        0.8790         0.6621
# Specificity                         0.8364        0.9336         0.9067
# User Acc (Pos Pred Value)           0.8650        0.7730         0.6906
# Neg Pred Value                      0.8036        0.9677         0.8951

prodes.tb$predicted <- sits_predict(dist_prodes.tb, model.glm2)
prodes.tb %>% dplyr::mutate(reference = label) %>% sits_accuracy(conv2.lst)
# Confusion Matrix and Statistics
#
#             Reference
# Prediction   Forest Non_Forest
#   Forest        109         32
#   Non_Forest     15        450
#
#              Accuracy : 0.9224
#                95% CI : (0.8982, 0.9425)
#
#                 Kappa : 0.7733
#
#      Prod Acc  Forest : 0.8790
#  Prod Acc  Non_Forest : 0.9336
#      User Acc  Forest : 0.7730
#  User Acc  Non_Forest : 0.9677


#    ___  _  _  ___     ___  ___   ___  ___  ___  _____
#   | _ \| \| ||   \   | __|/ _ \ | _ \| __|/ __||_   _|
#   |   /| .` || |) |_ | _|| (_) ||   /| _| \__ \  | |
#   |_|_\|_|\_||___/(_)|_|  \___/ |_|_\|___||___/  |_|
#

# random forest returns 1.0 of accuracy if all data is used for training. Reducing data training to 95%
model.rfor <- prodes.tb %>% sits_sample(frac = 0.95) %>%
    sits_distances(patt_prodes.tb, dist_method = sits_TWDTW_distances(multicores = 10)) %>%
    sits_train(sits_rfor())

# test on all data set
prodes.tb$predicted <- sits_predict(dist_prodes.tb, model.rfor)
prodes.tb %>% dplyr::mutate(reference = label) %>% sits_accuracy(conv.lst)
# Confusion Matrix and Statistics
#
#            Reference
# Prediction  Clear_Cut Forest Pasture
#   Clear_Cut       300     12      43
#   Forest           14    101      12
#   Pasture          23     11      90
#
# Overall Statistics
#
#  Accuracy : 0.8102
#    95% CI : (0.7767, 0.8407)
#
#     Kappa : 0.6742
#
# Statistics by Class:
#
#                           Class: Clear_Cut Class: Forest Class: Pasture
# Prod Acc (Sensitivity)              0.8902        0.8145         0.6207
# Specificity                         0.7955        0.9461         0.9262
# User Acc (Pos Pred Value)           0.8451        0.7953         0.7258
# Neg Pred Value                      0.8526        0.9520         0.8859

prodes.tb$predicted <- sits_predict(dist_prodes.tb, model.rfor)
prodes.tb %>% dplyr::mutate(reference = label) %>% sits_accuracy(conv2.lst)
# Confusion Matrix and Statistics
#
#             Reference
# Prediction   Forest Non_Forest
#   Forest        101         26
#   Non_Forest     23        456
#
#              Accuracy : 0.9191
#                95% CI : (0.8945, 0.9396)
#
#                 Kappa : 0.7538
#
#      Prod Acc  Forest : 0.8145
#  Prod Acc  Non_Forest : 0.9461
#      User Acc  Forest : 0.7953
#  User Acc  Non_Forest : 0.9520



# result    PATT.DIST.   intraCLUST.     CLUST
# - SVM:      0.9769 *      0.9950 *     0.9950 *
# - LDA:      0.9092        0.9307       0.9224
# - QDA:      0.9323 *      0.9802 *     0.9802 *
# - MLR:      0.9175        0.9406       0.9290
# - LASSO:    0.9191        0.9323       0.9224
# - RIDGE:    0.9241        0.9224       0.9224
# - RFOR:     0.9125        0.9208       0.9191

#
#
#  ###################################################################


#    _  __       _____  ___   _      ____
#   | |/ /      |  ___|/ _ \ | |    |  _ \
#   | ' / _____ | |_  | | | || |    | | | |
#   | . \|_____||  _| | |_| || |___ | |_| |
#   |_|\_\      |_|    \___/ |_____||____/
#

# The best "one-shot" results were: SVM and QDA.
# We processed k-fold validation for both methods

#    ___ __   __ __  __
#   / __|\ \ / /|  \/  |
#   \__ \ \ V / | |\/| |
#   |___/  \_/  |_|  |_|
#
svm.kfold <-
    prodes.tb %>% sits_kfold_validate(folds = 5,
                                      pt_method = sits_random_patterns(k = 12),
                                      dist_method = sits_TWDTW_distances(multicores = 4),
                                      tr_method = sits_svm(formula = sits_formula_linear(),
                                                           kernel = "radial"),
                                      multicores = 4)
svm.kfold %>% sits_accuracy(conv.lst)
# Confusion Matrix and Statistics
#
#            Reference
# Prediction  Clear_Cut Forest Pasture
#   Clear_Cut       289     14      48
#   Forest           14     96      11
#   Pasture          34     14      86
#
# Overall Statistics
#
#  Accuracy : 0.7772
#    95% CI : (0.742, 0.8098)
#
#     Kappa : 0.6186
#
# Statistics by Class:
#
#                           Class: Clear_Cut Class: Forest Class: Pasture
# Prod Acc (Sensitivity)              0.8576        0.7742         0.5931
# Specificity                         0.7695        0.9481         0.8959
# User Acc (Pos Pred Value)           0.8234        0.7934         0.6418
# Neg Pred Value                      0.8118        0.9423         0.8750

svm.kfold %>% sits_accuracy(conv2.lst)
# Confusion Matrix and Statistics
#
#             Reference
# Prediction   Forest Non_Forest
#   Forest         96         25
#   Non_Forest     28        457
#
#              Accuracy : 0.9125
#                95% CI : (0.8872, 0.9338)
#
#                 Kappa : 0.7289
#
#      Prod Acc  Forest : 0.7742
#  Prod Acc  Non_Forest : 0.9481
#      User Acc  Forest : 0.7934
#  User Acc  Non_Forest : 0.9423



#     ___    ___      _
#    / _ \  |   \    /_\
#   | (_) | | |) |  / _ \
#    \__\_\ |___/  /_/ \_\
#
qda.kfold <-
    prodes.tb %>% sits_kfold_validate(folds = 5,
                                      pt_method = sits_cluster_patterns(k = 12),
                                      dist_method = sits_TWDTW_distances(multicores = 4),
                                      tr_method = sits_qda(formula = sits_formula_linear()),
                                      multicores = 4)
qda.kfold %>% sits_accuracy(conv.lst)
# Confusion Matrix and Statistics
#
#            Reference
# Prediction  Clear_Cut Forest Pasture
#   Clear_Cut       281     33      59
#   Forest           20     79       7
#   Pasture          36     12      79
#
# Overall Statistics
#
#  Accuracy : 0.7244
#    95% CI : (0.687, 0.7597)
#
#     Kappa : 0.518
#
# Statistics by Class:
#
#                           Class: Clear_Cut Class: Forest Class: Pasture
# Prod Acc (Sensitivity)              0.8338        0.6371         0.5448
# Specificity                         0.6580        0.9440         0.8959
# User Acc (Pos Pred Value)           0.7534        0.7453         0.6220
# Neg Pred Value                      0.7597        0.9100         0.8622

qda.kfold %>% sits_accuracy(conv2.lst)
# Confusion Matrix and Statistics
#
#             Reference
# Prediction   Forest Non_Forest
#   Forest         79         27
#   Non_Forest     45        455
#
#              Accuracy : 0.8812
#                95% CI : (0.8527, 0.9059)
#
#                 Kappa : 0.6142
#
#      Prod Acc  Forest : 0.6371
#  Prod Acc  Non_Forest : 0.9440
#      User Acc  Forest : 0.7453
#  User Acc  Non_Forest : 0.9100


# result           K-FOLD PATT.DIST.            K-FOLD intraCLUST                 K-FOLD CLUST
#  - SVM:  0.8960 (0.8690, 0.9192)      0.9059 (0.8799, 0.9280) *    0.9125 (0.8872, 0.9338) *
#  - QDA:  0.9026 (0.8762, 0.9251) *    0.8680 (0.8384, 0.8939)      0.8812 (0.8527, 0.9059)

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
# SITS distance format is `sits_distances_from_data()`.
#
# No pattern needs to be created. The attributes are the time series itself.

tssp_prodes.tb <- prodes.tb %>% sits_distances_from_data()


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
prodes.tb$predicted <- sits_predict(tssp_prodes.tb, tssp.svm)
prodes.tb %>% dplyr::mutate(reference = label) %>% sits_accuracy()
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
prodes.tb$predicted <- sits_predict(tssp_prodes.tb, tssp.lda)
prodes.tb %>% dplyr::mutate(reference = label) %>% sits_accuracy()
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
prodes.tb$predicted <- sits_predict(tssp_prodes.tb, tssp.qda)
prodes.tb %>% dplyr::mutate(reference = label) %>% sits_accuracy()
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
prodes.tb$predicted <- sits_predict(tssp_prodes.tb, tssp.mlr)
prodes.tb %>% dplyr::mutate(reference = label) %>% sits_accuracy()
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
positive_tssp_prodes.tb <- positive_prodes.tb %>% sits_distances_from_data()

tssp.glm1 <- sits_train(positive_tssp_prodes.tb, sits_glm(alpha = 1))
get("result_glm", envir = environment(tssp.glm1))$lambda.min
# [1] 0.009024855

prodes.tb$predicted <- sits_predict(positive_tssp_prodes.tb, tssp.glm1)
prodes.tb %>% dplyr::mutate(reference = label) %>% sits_accuracy()
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
positive_tssp_prodes.tb <- positive_prodes.tb %>% sits_distances_from_data()

tssp.glm2 <- sits_train(positive_tssp_prodes.tb, sits_glm(alpha = 0))
get("result_glm", envir = environment(tssp.glm2))$lambda.min
# [1] 0.03095949

prodes.tb$predicted <- sits_predict(positive_tssp_prodes.tb, tssp.glm2)
prodes.tb %>% dplyr::mutate(reference = label) %>% sits_accuracy()
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
prodes.tb$predicted <- sits_predict(positive_tssp_prodes.tb, tssp.rfor)
prodes.tb %>% dplyr::mutate(reference = label) %>% sits_accuracy()
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
                                  dist_method = sits_distances_from_data(),
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
                                  dist_method = sits_distances_from_data(),
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
                        dist_method = sits_distances_from_data(),
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
                                  dist_method = sits_distances_from_data(),
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
                                           dist_method = sits_distances_from_data(),
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
                                           dist_method = sits_distances_from_data(),
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
                                  dist_method = sits_distances_from_data(),
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


