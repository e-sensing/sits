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
prodes_all.tb <- sits_getdata(system.file("extdata/time_series/prodes_226_064.json.gz", package = "sits")) %>%
    sits_linear_interp()

# view a summary of the data
sits_summary(prodes_all.tb)

# create list of label conversion
conv.lst <- list("Deforestation_2014" = "Clear_Cut",
                 "Deforestation_2015" = "Clear_Cut",
                 "Forest" = "Forest",
                 "Pasture" = "Pasture")

# create list for non_forest/forest classes
conv2.lst <- list("Deforestation_2014" = "Non_Forest",
                  "Deforestation_2015" = "Non_Forest",
                  "Forest" = "Forest",
                  "Pasture" = "Non_Forest")

prodes_all.tb <- prodes_all.tb %>% sits_create_folds(5)
prodes.tb <- prodes_all.tb[prodes_all.tb$folds == 1,]

# create patterns and plot them
patt_prodes.tb <- prodes_all.tb[prodes_all.tb$folds != 1,] %>% sits_gam()
sits_plot_patterns(patt_prodes.tb)

# create distances to train
dist_prodes_train.tb <- prodes_all.tb[prodes_all.tb$folds != 1,] %>%
    sits_distances(patt_prodes.tb, dist_method = sits_TWDTW_distances(multicores = 5))

# create distances to test
dist_prodes.tb <- prodes.tb %>%
    sits_distances(patt_prodes.tb, dist_method = sits_TWDTW_distances(multicores = 5))

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
model.svm <- sits_train(dist_prodes_train.tb, sits_svm(formula = sits_formula_linear(),
                                                       kernel = "radial"))

sits_predict(prodes.tb, dist_prodes.tb, model.svm) %>%
    dplyr::mutate(reference = label) %>% sits_accuracy(conv.lst)

sits_predict(prodes.tb, dist_prodes.tb, model.svm) %>%
    dplyr::mutate(reference = label) %>% sits_accuracy(conv2.lst)
# Confusion Matrix and Statistics
#
#             Reference
# Prediction   Forest Non_Forest
#   Forest         20          5
#   Non_Forest      5         94
#
#              Accuracy : 0.9194
#                95% CI : (0.8567, 0.9606)
#
#                 Kappa : 0.7495
#
#      Prod Acc  Forest : 0.8000
#  Prod Acc  Non_Forest : 0.9495
#      User Acc  Forest : 0.8000
#  User Acc  Non_Forest : 0.9495
#

#    _     ___    _
#   | |   |   \  /_\
#   | |__ | |) |/ _ \
#   |____||___//_/ \_\
#
model.lda <- sits_train(dist_prodes_train.tb, sits_lda(formula = sits_formula_linear()))

sits_predict(prodes.tb, dist_prodes.tb, model.lda) %>%
    dplyr::mutate(reference = label) %>% sits_accuracy(conv.lst)

sits_predict(prodes.tb, dist_prodes.tb, model.lda) %>%
    dplyr::mutate(reference = label) %>% sits_accuracy(conv2.lst)
# Confusion Matrix and Statistics
#
#             Reference
# Prediction   Forest Non_Forest
#   Forest         20          9
#   Non_Forest      5         90
#
#              Accuracy : 0.8871
#                95% CI : (0.8178, 0.9369)
#
#                 Kappa : 0.6691
#
#      Prod Acc  Forest : 0.8000
#  Prod Acc  Non_Forest : 0.9091
#      User Acc  Forest : 0.6897
#  User Acc  Non_Forest : 0.9474


#     ___   ___    _
#    / _ \ |   \  /_\
#   | (_) || |) |/ _ \
#    \__\_\|___//_/ \_\
#
model.qda <- sits_train(dist_prodes_train.tb, sits_qda(formula = sits_formula_linear()))

sits_predict(prodes.tb, dist_prodes.tb, model.qda) %>%
    dplyr::mutate(reference = label) %>% sits_accuracy(conv.lst)

sits_predict(prodes.tb, dist_prodes.tb, model.qda) %>%
    dplyr::mutate(reference = label) %>% sits_accuracy(conv2.lst)
# Confusion Matrix and Statistics
#
#             Reference
# Prediction   Forest Non_Forest
#   Forest         20          4
#   Non_Forest      5         95
#
#              Accuracy : 0.9274
#                95% CI : (0.8667, 0.9663)
#
#                 Kappa : 0.7711
#
#      Prod Acc  Forest : 0.8000
#  Prod Acc  Non_Forest : 0.9596
#      User Acc  Forest : 0.8333
#  User Acc  Non_Forest : 0.9500


#    __  __  _     ___
#   |  \/  || |   | _ \
#   | |\/| || |__ |   /
#   |_|  |_||____||_|_\
#
model.mlr <- sits_train(dist_prodes_train.tb, sits_mlr(formula = sits_formula_linear()))

sits_predict(prodes.tb, dist_prodes.tb, model.mlr) %>%
    dplyr::mutate(reference = label) %>% sits_accuracy(conv.lst)

sits_predict(prodes.tb, dist_prodes.tb, model.mlr) %>%
    dplyr::mutate(reference = label) %>% sits_accuracy(conv2.lst)
# Confusion Matrix and Statistics
#
#             Reference
# Prediction   Forest Non_Forest
#   Forest         20          7
#   Non_Forest      5         92
#
#              Accuracy : 0.9032
#                95% CI : (0.8371, 0.949)
#
#                 Kappa : 0.7081
#
#      Prod Acc  Forest : 0.8000
#  Prod Acc  Non_Forest : 0.9293
#      User Acc  Forest : 0.7407
#  User Acc  Non_Forest : 0.9485


#    _       _    ___  ___   ___
#   | |     /_\  / __|/ __| / _ \
#   | |__  / _ \ \__ \\__ \| (_) |
#   |____|/_/ \_\|___/|___/ \___/
#
model.glm1 <- sits_train(dist_prodes_train.tb, sits_glm(alpha = 1))
get("result_glm", envir = environment(model.glm1))$lambda.min
# [1] 0.005159049

sits_predict(prodes.tb, dist_prodes.tb, model.glm1) %>%
    dplyr::mutate(reference = label) %>% sits_accuracy(conv.lst)

sits_predict(prodes.tb, dist_prodes.tb, model.glm1) %>%
    dplyr::mutate(reference = label) %>% sits_accuracy(conv2.lst)
# Confusion Matrix and Statistics
#
#             Reference
# Prediction   Forest Non_Forest
#   Forest         19          7
#   Non_Forest      6         92
#
#              Accuracy : 0.8952
#                95% CI : (0.8274, 0.943)
#
#                 Kappa : 0.6791
#
#      Prod Acc  Forest : 0.7600
#  Prod Acc  Non_Forest : 0.9293
#      User Acc  Forest : 0.7308
#  User Acc  Non_Forest : 0.9388


#    ___  ___  ___    ___  ___
#   | _ \|_ _||   \  / __|| __|
#   |   / | | | |) || (_ || _|
#   |_|_\|___||___/  \___||___|
#
model.glm2 <- sits_train(dist_prodes.tb, sits_glm(alpha = 0))
get("result_glm", envir = environment(model.glm2))$lambda.min
# [1] 0.04289516

sits_predict(prodes.tb, dist_prodes.tb, model.glm2) %>%
    dplyr::mutate(reference = label) %>% sits_accuracy(conv.lst)

sits_predict(prodes.tb, dist_prodes.tb, model.glm2) %>%
    dplyr::mutate(reference = label) %>% sits_accuracy(conv2.lst)
# Confusion Matrix and Statistics
#
#             Reference
# Prediction   Forest Non_Forest
#   Forest         21          8
#   Non_Forest      4         91
#
#              Accuracy : 0.9032
#                95% CI : (0.8371, 0.949)
#
#                 Kappa : 0.7164
#
#      Prod Acc  Forest : 0.8400
#  Prod Acc  Non_Forest : 0.9192
#      User Acc  Forest : 0.7241
#  User Acc  Non_Forest : 0.9579


#    ___  _  _  ___     ___  ___   ___  ___  ___  _____
#   | _ \| \| ||   \   | __|/ _ \ | _ \| __|/ __||_   _|
#   |   /| .` || |) |_ | _|| (_) ||   /| _| \__ \  | |
#   |_|_\|_|\_||___/(_)|_|  \___/ |_|_\|___||___/  |_|
#

model.rfor <- sits_train(dist_prodes_train.tb, sits_rfor())

sits_predict(prodes.tb, dist_prodes.tb, model.rfor) %>%
    dplyr::mutate(reference = label) %>% sits_accuracy(conv.lst)

sits_predict(prodes.tb, dist_prodes.tb, model.rfor) %>%
    dplyr::mutate(reference = label) %>% sits_accuracy(conv2.lst)
# Confusion Matrix and Statistics
#
#             Reference
# Prediction   Forest Non_Forest
#   Forest         19          6
#   Non_Forest      6         93
#
#              Accuracy : 0.9032
#                95% CI : (0.8371, 0.949)
#
#                 Kappa : 0.6994
#
#      Prod Acc  Forest : 0.7600
#  Prod Acc  Non_Forest : 0.9394
#      User Acc  Forest : 0.7600
#  User Acc  Non_Forest : 0.9394
#


# result    PATT.DIST.
# - SVM:      0.9194 *
# - LDA:      0.8871
# - QDA:      0.9274 *
# - MLR:      0.9032
# - LASSO:    0.8952
# - RIDGE:    0.9032
# - RFOR:     0.9032

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
    prodes_all.tb %>% sits_kfold_validate(folds = 5,
                                          pt_method = sits_gam(),
                                          dist_method = sits_TWDTW_distances(multicores = 5),
                                          tr_method = sits_svm(formula = sits_formula_linear(),
                                                               kernel = "radial"),
                                          multicores = 2)
svm.kfold %>% sits_accuracy(conv.lst)

svm.kfold %>% sits_accuracy(conv2.lst)
# Confusion Matrix and Statistics
#
#             Reference
# Prediction   Forest Non_Forest
#   Forest         95         31
#   Non_Forest     33        458
#
#              Accuracy : 0.8963
#                95% CI : (0.8695, 0.9192)
#
#                 Kappa : 0.6827
#
#      Prod Acc  Forest : 0.7422
#  Prod Acc  Non_Forest : 0.9366
#      User Acc  Forest : 0.7540
#  User Acc  Non_Forest : 0.9328



#     ___    ___      _
#    / _ \  |   \    /_\
#   | (_) | | |) |  / _ \
#    \__\_\ |___/  /_/ \_\
#
qda.kfold <-
    prodes_all.tb %>% sits_kfold_validate(folds = 5,
                                          pt_method = sits_gam(),
                                          dist_method = sits_TWDTW_distances(multicores = 5),
                                          tr_method = sits_qda(formula = sits_formula_linear()),
                                          multicores = 2)
qda.kfold %>% sits_accuracy(conv.lst)

qda.kfold %>% sits_accuracy(conv2.lst)
# Confusion Matrix and Statistics
#
#             Reference
# Prediction   Forest Non_Forest
#   Forest        104         37
#   Non_Forest     24        452
#
#              Accuracy : 0.9011
#                95% CI : (0.8748, 0.9235)
#
#                 Kappa : 0.7102
#
#      Prod Acc  Forest : 0.8125
#  Prod Acc  Non_Forest : 0.9243
#      User Acc  Forest : 0.7376
#  User Acc  Non_Forest : 0.9496


# result           K-FOLD PATT.DIST.
#  - SVM:  0.8963 (0.8695, 0.9192)
#  - QDA:  0.9011 (0.8748, 0.9235) *


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
prodes_intracluster_gen <- prodes_all.tb[prodes_all.tb$folds != 1,] %>% sits_intracluster_patterns()
patt_prodes.tb <- prodes_intracluster_gen(k = 3)
sits_plot_patterns(patt_prodes.tb)

# create distances for train
dist_prodes_train.tb <-
    prodes_all.tb[prodes_all.tb$folds != 1,] %>% sits_distances(patt_prodes.tb, dist_method = sits_TWDTW_distances(multicores = 10))

# create distances for test
dist_prodes.tb <-
    prodes.tb %>% sits_distances(patt_prodes.tb, dist_method = sits_TWDTW_distances(multicores = 5))


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
model.svm <- sits_train(dist_prodes_train.tb, sits_svm(formula = sits_formula_linear(),
                                                       kernel = "radial"))
sits_predict(prodes.tb, dist_prodes.tb, model.svm) %>%
    dplyr::mutate(reference = label) %>% sits_accuracy(conv.lst)

sits_predict(prodes.tb, dist_prodes.tb, model.svm) %>%
    dplyr::mutate(reference = label) %>% sits_accuracy(conv2.lst)
# Confusion Matrix and Statistics
#
#             Reference
# Prediction   Forest Non_Forest
#   Forest         20          5
#   Non_Forest      5         94
#
#              Accuracy : 0.9194
#                95% CI : (0.8567, 0.9606)
#
#                 Kappa : 0.7495
#
#      Prod Acc  Forest : 0.8000
#  Prod Acc  Non_Forest : 0.9495
#      User Acc  Forest : 0.8000
#  User Acc  Non_Forest : 0.9495


#    _     ___    _
#   | |   |   \  /_\
#   | |__ | |) |/ _ \
#   |____||___//_/ \_\
#
model.lda <- sits_train(dist_prodes_train.tb, sits_lda(formula = sits_formula_linear()))

sits_predict(prodes.tb, dist_prodes.tb, model.lda) %>%
    dplyr::mutate(reference = label) %>% sits_accuracy(conv.lst)

sits_predict(prodes.tb, dist_prodes.tb, model.lda) %>%
    dplyr::mutate(reference = label) %>% sits_accuracy(conv2.lst)
# Confusion Matrix and Statistics
#
#             Reference
# Prediction   Forest Non_Forest
#   Forest         18          6
#   Non_Forest      7         93
#
#              Accuracy : 0.8952
#                95% CI : (0.8274, 0.943)
#
#                 Kappa : 0.6694
#
#      Prod Acc  Forest : 0.7200
#  Prod Acc  Non_Forest : 0.9394
#      User Acc  Forest : 0.7500
#  User Acc  Non_Forest : 0.9300


#     ___   ___    _
#    / _ \ |   \  /_\
#   | (_) || |) |/ _ \
#    \__\_\|___//_/ \_\
#
model.qda <- sits_train(dist_prodes_train.tb, sits_qda(formula = sits_formula_linear()))

sits_predict(prodes.tb, dist_prodes.tb, model.qda) %>%
    dplyr::mutate(reference = label) %>% sits_accuracy(conv.lst)

sits_predict(prodes.tb, dist_prodes.tb, model.qda) %>%
    dplyr::mutate(reference = label) %>% sits_accuracy(conv2.lst)
# Confusion Matrix and Statistics
#
#             Reference
# Prediction   Forest Non_Forest
#   Forest         15          2
#   Non_Forest     10         97
#
#              Accuracy : 0.9032
#                95% CI : (0.8371, 0.949)
#
#                 Kappa : 0.6586
#
#      Prod Acc  Forest : 0.6000
#  Prod Acc  Non_Forest : 0.9798
#      User Acc  Forest : 0.8824
#  User Acc  Non_Forest : 0.9065


#    __  __  _     ___
#   |  \/  || |   | _ \
#   | |\/| || |__ |   /
#   |_|  |_||____||_|_\
#
model.mlr <- sits_train(dist_prodes_train.tb, sits_mlr(formula = sits_formula_linear()))

sits_predict(prodes.tb, dist_prodes.tb, model.mlr) %>%
    dplyr::mutate(reference = label) %>% sits_accuracy(conv.lst)

sits_predict(prodes.tb, dist_prodes.tb, model.mlr) %>%
    dplyr::mutate(reference = label) %>% sits_accuracy(conv2.lst)
# Confusion Matrix and Statistics
#
#             Reference
# Prediction   Forest Non_Forest
#   Forest         15          9
#   Non_Forest     10         90
#
#              Accuracy : 0.8468
#                95% CI : (0.7711, 0.9052)
#
#                 Kappa : 0.5168
#
#      Prod Acc  Forest : 0.6000
#  Prod Acc  Non_Forest : 0.9091
#      User Acc  Forest : 0.6250
#  User Acc  Non_Forest : 0.9000


#    _       _    ___  ___   ___
#   | |     /_\  / __|/ __| / _ \
#   | |__  / _ \ \__ \\__ \| (_) |
#   |____|/_/ \_\|___/|___/ \___/
#
model.glm1 <- sits_train(dist_prodes_train.tb, sits_glm(alpha = 1))

get("result_glm", envir = environment(model.glm1))$lambda.min
# [1] 0.007586427

sits_predict(prodes.tb, dist_prodes.tb, model.glm1) %>%
    dplyr::mutate(reference = label) %>% sits_accuracy(conv.lst)

sits_predict(prodes.tb, dist_prodes.tb, model.glm1) %>%
    dplyr::mutate(reference = label) %>% sits_accuracy(conv2.lst)
# Confusion Matrix and Statistics
#
#             Reference
# Prediction   Forest Non_Forest
#   Forest         22          6
#   Non_Forest      3         93
#
#              Accuracy : 0.9274
#                95% CI : (0.8667, 0.9663)
#
#                 Kappa : 0.7842
#
#      Prod Acc  Forest : 0.8800
#  Prod Acc  Non_Forest : 0.9394
#      User Acc  Forest : 0.7857
#  User Acc  Non_Forest : 0.9688


#    ___  ___  ___    ___  ___
#   | _ \|_ _||   \  / __|| __|
#   |   / | | | |) || (_ || _|
#   |_|_\|___||___/  \___||___|
#
model.glm2 <- sits_train(dist_prodes_train.tb, sits_glm(alpha = 0))

get("result_glm", envir = environment(model.glm2))$lambda.min
# [1] 0.02160642

sits_predict(prodes.tb, dist_prodes.tb, model.glm2) %>%
    dplyr::mutate(reference = label) %>% sits_accuracy(conv.lst)

sits_predict(prodes.tb, dist_prodes.tb, model.glm2) %>%
    dplyr::mutate(reference = label) %>% sits_accuracy(conv2.lst)
# Confusion Matrix and Statistics
#
#             Reference
# Prediction   Forest Non_Forest
#   Forest         21          6
#   Non_Forest      4         93
#
#              Accuracy : 0.9194
#                95% CI : (0.8567, 0.9606)
#
#                 Kappa : 0.7568
#
#      Prod Acc  Forest : 0.8400
#  Prod Acc  Non_Forest : 0.9394
#      User Acc  Forest : 0.7778
#  User Acc  Non_Forest : 0.9588


#    ___  _  _  ___     ___  ___   ___  ___  ___  _____
#   | _ \| \| ||   \   | __|/ _ \ | _ \| __|/ __||_   _|
#   |   /| .` || |) |_ | _|| (_) ||   /| _| \__ \  | |
#   |_|_\|_|\_||___/(_)|_|  \___/ |_|_\|___||___/  |_|
#

model.rfor <- dist_prodes_train.tb %>% sits_train(sits_rfor())

sits_predict(prodes.tb, dist_prodes.tb, model.rfor) %>%
    dplyr::mutate(reference = label) %>% sits_accuracy(conv.lst)

sits_predict(prodes.tb, dist_prodes.tb, model.rfor) %>%
    dplyr::mutate(reference = label) %>% sits_accuracy(conv2.lst)
# Confusion Matrix and Statistics
#
#             Reference
# Prediction   Forest Non_Forest
#   Forest         20          7
#   Non_Forest      5         92
#
#              Accuracy : 0.9032
#                95% CI : (0.8371, 0.949)
#
#                 Kappa : 0.7081
#
#      Prod Acc  Forest : 0.8000
#  Prod Acc  Non_Forest : 0.9293
#      User Acc  Forest : 0.7407
#  User Acc  Non_Forest : 0.9485

# =============================================

# result    PATT.DIST.   intraCLUST.
# - SVM:      0.9769 *      0.9194 *
# - LDA:      0.9092        0.8952
# - QDA:      0.9323 *      0.9032 *
# - MLR:      0.9175        0.8468
# - LASSO:    0.9191        0.9274
# - RIDGE:    0.9241        0.9194
# - RFOR:     0.9125        0.9032

#
#
#  ###################################################################


#    _  __       _____  ___   _      ____
#   | |/ /      |  ___|/ _ \ | |    |  _ \
#   | ' / _____ | |_  | | | || |    | | | |
#   | . \|_____||  _| | |_| || |___ | |_| |
#   |_|\_\      |_|    \___/ |_____||____/
#

# The best "one-shot" results were: SVM, LASSO, and QDA.
# We processed k-fold validation for both methods

#    ___ __   __ __  __
#   / __|\ \ / /|  \/  |
#   \__ \ \ V / | |\/| |
#   |___/  \_/  |_|  |_|
#
svm.kfold <-
    prodes_all.tb %>% sits_kfold_validate(folds = 5,
                                          pt_method = sits_intracluster_patterns(k = 3),
                                          dist_method = sits_TWDTW_distances(multicores = 3),
                                          tr_method = sits_svm(formula = sits_formula_linear(),
                                                               kernel = "radial"),
                                          multicores = 5)
svm.kfold %>% sits_accuracy(conv.lst)

svm.kfold %>% sits_accuracy(conv2.lst)
# Confusion Matrix and Statistics
#
#             Reference
# Prediction   Forest Non_Forest
#   Forest        102         24
#   Non_Forest     26        465
#
#              Accuracy : 0.919
#                95% CI : (0.8946, 0.9393)
#
#                 Kappa : 0.7521
#
#      Prod Acc  Forest : 0.7969
#  Prod Acc  Non_Forest : 0.9509
#      User Acc  Forest : 0.8095
#  User Acc  Non_Forest : 0.9470



#    _        _     ___   ___    ___
#   | |      /_\   / __| / __|  / _ \
#   | |__   / _ \  \__ \ \__ \ | (_) |
#   |____| /_/ \_\ |___/ |___/  \___/
#
lasso.kfold <-
    prodes_all.tb %>% sits_kfold_validate(folds = 5,
                                          pt_method = sits_intracluster_patterns(k = 3),
                                          dist_method = sits_TWDTW_distances(multicores = 3),
                                          tr_method = sits_glm(alpha = 1),
                                          multicores = 5)
lasso.kfold %>% sits_accuracy(conv.lst)

lasso.kfold %>% sits_accuracy(conv2.lst)
# Confusion Matrix and Statistics
#
#             Reference
# Prediction   Forest Non_Forest
#   Forest        107         28
#   Non_Forest     21        461
#
#              Accuracy : 0.9206
#                95% CI : (0.8964, 0.9407)
#
#                 Kappa : 0.7633
#
#      Prod Acc  Forest : 0.8359
#  Prod Acc  Non_Forest : 0.9427
#      User Acc  Forest : 0.7926
#  User Acc  Non_Forest : 0.9564


#    ___   ___   ___     ___   ___
#   | _ \ |_ _| |   \   / __| | __|
#   |   /  | |  | |) | | (_ | | _|
#   |_|_\ |___| |___/   \___| |___|
#
ridge.kfold <-
    prodes_all.tb %>% sits_kfold_validate(folds = 5,
                                          pt_method = sits_intracluster_patterns(k = 3),
                                          dist_method = sits_TWDTW_distances(multicores = 3),
                                          tr_method = sits_glm(alpha = 0),
                                          multicores = 5)
ridge.kfold %>% sits_accuracy(conv.lst)

ridge.kfold %>% sits_accuracy(conv2.lst)
# Confusion Matrix and Statistics
#
#             Reference
# Prediction   Forest Non_Forest
#   Forest        108         36
#   Non_Forest     20        453
#
#              Accuracy : 0.9092
#                95% CI : (0.8838, 0.9307)
#
#                 Kappa : 0.7362
#
#      Prod Acc  Forest : 0.8438
#  Prod Acc  Non_Forest : 0.9264
#      User Acc  Forest : 0.7500
#  User Acc  Non_Forest : 0.9577
#

# ===========================================================

# result              K-FOLD PATT.DIST.            K-FOLD intraCLUST
#  - SVM:     0.8960 (0.8690, 0.9192)      0.9190 (0.8946, 0.9393)
#  - QDA:     0.9026 (0.8762, 0.9251) *                    -------
#  - LASSO:                   -------      0.9206 (0.8964, 0.9407) *
#  - RIDGE:                   -------      0.9092 (0.8838, 0.9307)

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
patt_cluster_gen <- prodes_all.tb[prodes_all.tb$folds != 1,] %>% sits_cluster_patterns()
patt_prodes.tb <- patt_cluster_gen(12)
sits_plot_patterns(patt_prodes.tb)

# create distances for training
dist_prodes_train.tb <- prodes_all.tb[prodes_all.tb$folds != 1,] %>%
    sits_distances(patt_prodes.tb, dist_method = sits_TWDTW_distances(multicores = 10))

# create distances for test
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
model.svm <- sits_train(dist_prodes_train.tb, sits_svm(formula = sits_formula_linear(),
                                                       kernel = "radial"))
sits_predict(prodes.tb, dist_prodes.tb, model.svm) %>%
    dplyr::mutate(reference = label) %>% sits_accuracy(conv.lst)

sits_predict(prodes.tb, dist_prodes.tb, model.svm) %>%
    dplyr::mutate(reference = label) %>% sits_accuracy(conv2.lst)
# Confusion Matrix and Statistics
#
#             Reference
# Prediction   Forest Non_Forest
#   Forest         18          4
#   Non_Forest      7         95
#
#              Accuracy : 0.9113
#                95% CI : (0.8468, 0.9549)
#
#                 Kappa : 0.7115
#
#      Prod Acc  Forest : 0.7200
#  Prod Acc  Non_Forest : 0.9596
#      User Acc  Forest : 0.8182
#  User Acc  Non_Forest : 0.9314


#    _     ___    _
#   | |   |   \  /_\
#   | |__ | |) |/ _ \
#   |____||___//_/ \_\
#
model.lda <- sits_train(dist_prodes_train.tb, sits_lda(formula = sits_formula_linear()))

sits_predict(prodes.tb, dist_prodes.tb, model.lda) %>%
    dplyr::mutate(reference = label) %>% sits_accuracy(conv.lst)

sits_predict(prodes.tb, dist_prodes.tb, model.lda) %>%
    dplyr::mutate(reference = label) %>% sits_accuracy(conv2.lst)
# Confusion Matrix and Statistics
#
#             Reference
# Prediction   Forest Non_Forest
#   Forest         18          8
#   Non_Forest      7         91
#
#              Accuracy : 0.879
#                95% CI : (0.8083, 0.9307)
#
#                 Kappa : 0.6298
#
#      Prod Acc  Forest : 0.7200
#  Prod Acc  Non_Forest : 0.9192
#      User Acc  Forest : 0.6923
#  User Acc  Non_Forest : 0.9286


#     ___   ___    _
#    / _ \ |   \  /_\
#   | (_) || |) |/ _ \
#    \__\_\|___//_/ \_\
#
model.qda <- sits_train(dist_prodes_train.tb, sits_qda(formula = sits_formula_linear()))

sits_predict(prodes.tb, dist_prodes.tb, model.qda) %>%
    dplyr::mutate(reference = label) %>% sits_accuracy(conv.lst)

sits_predict(prodes.tb, dist_prodes.tb, model.qda) %>%
    dplyr::mutate(reference = label) %>% sits_accuracy(conv2.lst)
# Confusion Matrix and Statistics
#
#             Reference
# Prediction   Forest Non_Forest
#   Forest         12          1
#   Non_Forest     13         98
#
#              Accuracy : 0.8871
#                95% CI : (0.8178, 0.9369)
#
#                 Kappa : 0.5726
#
#      Prod Acc  Forest : 0.4800
#  Prod Acc  Non_Forest : 0.9899
#      User Acc  Forest : 0.9231
#  User Acc  Non_Forest : 0.8829


#    __  __  _     ___
#   |  \/  || |   | _ \
#   | |\/| || |__ |   /
#   |_|  |_||____||_|_\
#
model.mlr <- sits_train(dist_prodes_train.tb, sits_mlr(formula = sits_formula_linear()))

sits_predict(prodes.tb, dist_prodes.tb, model.mlr) %>%
    dplyr::mutate(reference = label) %>% sits_accuracy(conv.lst)

sits_predict(prodes.tb, dist_prodes.tb, model.mlr) %>%
    dplyr::mutate(reference = label) %>% sits_accuracy(conv2.lst)
# Confusion Matrix and Statistics
#
#             Reference
# Prediction   Forest Non_Forest
#   Forest         17          6
#   Non_Forest      8         93
#
#              Accuracy : 0.8871
#                95% CI : (0.8178, 0.9369)
#
#                 Kappa : 0.6385
#
#      Prod Acc  Forest : 0.6800
#  Prod Acc  Non_Forest : 0.9394
#      User Acc  Forest : 0.7391
#  User Acc  Non_Forest : 0.9208


#    _       _    ___  ___   ___
#   | |     /_\  / __|/ __| / _ \
#   | |__  / _ \ \__ \\__ \| (_) |
#   |____|/_/ \_\|___/|___/ \___/
#
model.glm1 <- sits_train(dist_prodes_train.tb, sits_glm(alpha = 1))
get("result_glm", envir = environment(model.glm1))$lambda.min
# [1] 0.006506656

sits_predict(prodes.tb, dist_prodes.tb, model.glm1) %>%
    dplyr::mutate(reference = label) %>% sits_accuracy(conv.lst)

sits_predict(prodes.tb, dist_prodes.tb, model.glm1) %>%
    dplyr::mutate(reference = label) %>% sits_accuracy(conv2.lst)
# Confusion Matrix and Statistics
#
#             Reference
# Prediction   Forest Non_Forest
#   Forest         19          8
#   Non_Forest      6         91
#
#              Accuracy : 0.8871
#                95% CI : (0.8178, 0.9369)
#
#                 Kappa : 0.6595
#
#      Prod Acc  Forest : 0.7600
#  Prod Acc  Non_Forest : 0.9192
#      User Acc  Forest : 0.7037
#  User Acc  Non_Forest : 0.9381


#    ___  ___  ___    ___  ___
#   | _ \|_ _||   \  / __|| __|
#   |   / | | | |) || (_ || _|
#   |_|_\|___||___/  \___||___|
#
model.glm2 <- sits_train(dist_prodes_train.tb, sits_glm(alpha = 0))
get("result_glm", envir = environment(model.glm2))$lambda.min
# [1] 0.02449714

sits_predict(prodes.tb, dist_prodes.tb, model.glm2) %>%
    dplyr::mutate(reference = label) %>% sits_accuracy(conv.lst)

sits_predict(prodes.tb, dist_prodes.tb, model.glm2) %>%
    dplyr::mutate(reference = label) %>% sits_accuracy(conv2.lst)
# Confusion Matrix and Statistics
#
#             Reference
# Prediction   Forest Non_Forest
#   Forest         20          6
#   Non_Forest      5         93
#
#              Accuracy : 0.9113
#                95% CI : (0.8468, 0.9549)
#
#                 Kappa : 0.7285
#
#      Prod Acc  Forest : 0.8000
#  Prod Acc  Non_Forest : 0.9394
#      User Acc  Forest : 0.7692
#  User Acc  Non_Forest : 0.9490


#    ___  _  _  ___     ___  ___   ___  ___  ___  _____
#   | _ \| \| ||   \   | __|/ _ \ | _ \| __|/ __||_   _|
#   |   /| .` || |) |_ | _|| (_) ||   /| _| \__ \  | |
#   |_|_\|_|\_||___/(_)|_|  \___/ |_|_\|___||___/  |_|
#

model.rfor <- sits_train(dist_prodes_train.tb, sits_rfor())

sits_predict(prodes.tb, dist_prodes.tb, model.rfor) %>%
    dplyr::mutate(reference = label) %>% sits_accuracy(conv.lst)

sits_predict(prodes.tb, dist_prodes.tb, model.rfor) %>%
    dplyr::mutate(reference = label) %>% sits_accuracy(conv2.lst)
# Confusion Matrix and Statistics
#
#             Reference
# Prediction   Forest Non_Forest
#   Forest         20          5
#   Non_Forest      5         94
#
#              Accuracy : 0.9194
#                95% CI : (0.8567, 0.9606)
#
#                 Kappa : 0.7495
#
#      Prod Acc  Forest : 0.8000
#  Prod Acc  Non_Forest : 0.9495
#      User Acc  Forest : 0.8000
#  User Acc  Non_Forest : 0.9495

# =============================================

# result    PATT.DIST.    intraCLUST      CLUSTER
# - SVM:      0.9769 *      0.9194 *     0.9113 *
# - LDA:      0.9092        0.8952       0.8790
# - QDA:      0.9323 *      0.9032 *     0.8871
# - MLR:      0.9175        0.8468       0.8871
# - LASSO:    0.9191        0.9274       0.8871
# - RIDGE:    0.9241        0.9194       0.9113 *
# - RFOR:     0.9125        0.9032       0.9194 *

#
#
#  ###################################################################


#    _  __       _____  ___   _      ____
#   | |/ /      |  ___|/ _ \ | |    |  _ \
#   | ' / _____ | |_  | | | || |    | | | |
#   | . \|_____||  _| | |_| || |___ | |_| |
#   |_|\_\      |_|    \___/ |_____||____/
#

# The best "one-shot" results were: SVM, RIDGE, and RFOR.
# We processed k-fold validation for these methods

#    ___ __   __ __  __
#   / __|\ \ / /|  \/  |
#   \__ \ \ V / | |\/| |
#   |___/  \_/  |_|  |_|
#
svm.kfold <-
    prodes_all.tb %>% sits_kfold_validate(folds = 5,
                                          pt_method = sits_cluster_patterns(k = 12),
                                          dist_method = sits_TWDTW_distances(multicores = 4),
                                          tr_method = sits_svm(formula = sits_formula_linear(),
                                                               kernel = "radial"),
                                          multicores = 4)
svm.kfold %>% sits_accuracy(conv.lst)

svm.kfold %>% sits_accuracy(conv2.lst)
# Confusion Matrix and Statistics
#
#             Reference
# Prediction   Forest Non_Forest
#   Forest         99         43
#   Non_Forest     29        446
#
#              Accuracy : 0.8833
#                95% CI : (0.8553, 0.9076)
#
#                 Kappa : 0.6589
#
#      Prod Acc  Forest : 0.7734
#  Prod Acc  Non_Forest : 0.9121
#      User Acc  Forest : 0.6972
#  User Acc  Non_Forest : 0.9389



#    ___   ___   ___     ___   ___
#   | _ \ |_ _| |   \   / __| | __|
#   |   /  | |  | |) | | (_ | | _|
#   |_|_\ |___| |___/   \___| |___|
#
ridge.kfold <-
    prodes_all.tb %>% sits_kfold_validate(folds = 5,
                                          pt_method = sits_cluster_patterns(k = 12),
                                          dist_method = sits_TWDTW_distances(multicores = 4),
                                          tr_method = sits_glm(alpha = 0),
                                          multicores = 4)
ridge.kfold %>% sits_accuracy(conv.lst)

ridge.kfold %>% sits_accuracy(conv2.lst)
# Confusion Matrix and Statistics
#
#             Reference
# Prediction   Forest Non_Forest
#   Forest        109         38
#   Non_Forest     19        451
#
#              Accuracy : 0.9076
#                95% CI : (0.882, 0.9293)
#
#                 Kappa : 0.7337
#
#      Prod Acc  Forest : 0.8516
#  Prod Acc  Non_Forest : 0.9223
#      User Acc  Forest : 0.7415
#  User Acc  Non_Forest : 0.9596



#    ___   ___    ___    ___
#   | _ \ | __|  / _ \  | _ \
#   |   / | _|  | (_) | |   /
#   |_|_\ |_|    \___/  |_|_\
#
rfor.kfold <-
    prodes_all.tb %>% sits_kfold_validate(folds = 5,
                                          pt_method = sits_cluster_patterns(k = 12),
                                          dist_method = sits_TWDTW_distances(multicores = 4),
                                          tr_method = sits_rfor(),
                                          multicores = 4)
rfor.kfold %>% sits_accuracy(conv.lst)

rfor.kfold %>% sits_accuracy(conv2.lst)
# Confusion Matrix and Statistics
#
#             Reference
# Prediction   Forest Non_Forest
#   Forest        100         43
#   Non_Forest     28        446
#
#              Accuracy : 0.8849
#                95% CI : (0.8571, 0.909)
#
#                 Kappa : 0.6646
#
#      Prod Acc  Forest : 0.7812
#  Prod Acc  Non_Forest : 0.9121
#      User Acc  Forest : 0.6993
#  User Acc  Non_Forest : 0.9409


# ===========================================================

# result              K-FOLD PATT.DIST.            K-FOLD intraCLUST               K-FOLD CLUSTER
#  - SVM:     0.8960 (0.8690, 0.9192)      0.9190 (0.8946, 0.9393)      0.8833 (0.8553, 0.9076)
#  - QDA:     0.9026 (0.8762, 0.9251) *                    -------                      -------
#  - LASSO:                   -------      0.9206 (0.8964, 0.9407) *                    -------
#  - RIDGE:                   -------      0.9092 (0.8838, 0.9307)      0.9076 (0.8820, 0.9293) *
#  - RFOR:                    -------                      -------      0.8849 (0.8571, 0.9090)


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

# some ML functions computes log to perform classification. Hence, we must work on positive values.
# We transform the data time series values by the following function:
positive_prodes_all.tb <- prodes_all.tb %>% sits_apply(function(band) (1 + 9 * band) ** 2)

# distances for train
tssp_prodes_train.tb <- positive_prodes_all.tb[positive_prodes_all.tb$folds != 1,] %>% sits_spread_time_series()

# distances for test
positive_prodes.tb <- positive_prodes_all.tb[positive_prodes_all.tb$folds == 1,]
tssp_prodes.tb <- positive_prodes.tb %>% sits_spread_time_series()

#
#
#  ###################################################################


#    _____  _____  ____  _____  ____
#   |_   _|| ____|/ ___||_   _|/ ___|
#     | |  |  _|  \___ \  | |  \___ \
#     | |  | |___  ___) | | |   ___) |
#     |_|  |_____||____/  |_|  |____/
#


# as processing time is small we will proceed k-fold directly for all methods


#    ___ __   __ __  __
#   / __|\ \ / /|  \/  |
#   \__ \ \ V / | |\/| |
#   |___/  \_/  |_|  |_|
#
tssp.svm <- sits_train(tssp_prodes_train.tb, sits_svm(formula = sits_formula_linear(),
                                                      kernel = "radial"))

sits_predict(positive_prodes.tb, tssp_prodes.tb, tssp.svm) %>%
    dplyr::mutate(reference = label) %>% sits_accuracy(conv2.lst)


#    _     ___    _
#   | |   |   \  /_\
#   | |__ | |) |/ _ \
#   |____||___//_/ \_\
#
tssp.lda <- sits_train(tssp_prodes_train.tb, sits_lda(formula = sits_formula_linear()))

sits_predict(positive_prodes.tb, tssp_prodes.tb, tssp.lda) %>%
    dplyr::mutate(reference = label) %>% sits_accuracy(conv2.lst)


#     ___   ___    _
#    / _ \ |   \  /_\
#   | (_) || |) |/ _ \
#    \__\_\|___//_/ \_\
#
tssp.qda <- sits_train(tssp_prodes_train.tb, sits_qda(formula = sits_formula_linear()))

sits_predict(positive_prodes.tb, tssp_prodes.tb, tssp.qda) %>%
    dplyr::mutate(reference = label) %>% sits_accuracy(conv2.lst)


#    __  __  _     ___
#   |  \/  || |   | _ \
#   | |\/| || |__ |   /
#   |_|  |_||____||_|_\
#
tssp.mlr <- sits_train(tssp_prodes_train.tb, sits_mlr(formula = sits_formula_linear()))

sits_predict(positive_prodes.tb, tssp_prodes.tb, tssp.mlr) %>%
    dplyr::mutate(reference = label) %>% sits_accuracy(conv2.lst)


#    _       _    ___  ___   ___
#   | |     /_\  / __|/ __| / _ \
#   | |__  / _ \ \__ \\__ \| (_) |
#   |____|/_/ \_\|___/|___/ \___/
#

tssp.glm1 <- sits_train(tssp_prodes_train.tb, sits_glm(alpha = 1))
get("result_glm", envir = environment(tssp.glm1))$lambda.min
#

sits_predict(positive_prodes.tb, tssp_prodes.tb, tssp.glm1) %>%
    dplyr::mutate(reference = label) %>% sits_accuracy(conv2.lst)


#    ___  ___  ___    ___  ___
#   | _ \|_ _||   \  / __|| __|
#   |   / | | | |) || (_ || _|
#   |_|_\|___||___/  \___||___|
#

tssp.glm2 <- sits_train(tssp_prodes_train.tb, sits_glm(alpha = 0))
get("result_glm", envir = environment(tssp.glm2))$lambda.min
#

sits_predict(positive_prodes.tb, tssp_prodes.tb, tssp.glm2) %>%
    dplyr::mutate(reference = label) %>% sits_accuracy(conv2.lst)


#    ___  _  _  ___     ___  ___   ___  ___  ___  _____
#   | _ \| \| ||   \   | __|/ _ \ | _ \| __|/ __||_   _|
#   |   /| .` || |) |_ | _|| (_) ||   /| _| \__ \  | |
#   |_|_\|_|\_||___/(_)|_|  \___/ |_|_\|___||___/  |_|
#

tssp.rfor <- sits_train(tssp_prodes_train.tb, sits_rfor())

sits_predict(positive_prodes.tb, tssp_prodes.tb, tssp.rfor) %>%
    dplyr::mutate(reference = label) %>% sits_accuracy(conv2.lst)

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
# All methods were used to proceed with k-fold validation


#    ___ __   __ __  __
#   / __|\ \ / /|  \/  |
#   \__ \ \ V / | |\/| |
#   |___/  \_/  |_|  |_|
#

svm.kfold <-
    positive_prodes_all.tb %>% sits_kfold_validate(folds = 5,
                                                   pt_method = function(...) NULL,
                                                   dist_method = sits_spread_time_series(),
                                                   tr_method = sits_svm(formula = sits_formula_linear(),
                                                                        kernel = "radial"),
                                                   multicores = 5)
svm.kfold %>% sits_accuracy(conv2.lst)
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

lda.kfold <-
    positive_prodes_all.tb %>% sits_kfold_validate(folds = 5,
                                                   pt_method = function(...) NULL,
                                                   dist_method = sits_spread_time_series(),
                                                   tr_method = sits_lda(formula = sits_formula_linear()),
                                                   multicores = 5)
lda.kfold %>% sits_accuracy(conv2.lst)
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

qda.kfold <-
    positive_prodes_all.tb %>%
    sits_kfold_validate(folds = 5,
                        pt_method = function(...) NULL,
                        dist_method = sits_spread_time_series(),
                        tr_method = sits_qda(formula = sits_formula_linear()),
                        multicores = 5)
qda.kfold %>% sits_accuracy(conv2.lst)
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
mlr.kfold <-
    positive_prodes_all.tb %>%
    sits_kfold_validate(folds = 5,
                        pt_method = function(...) NULL,
                        dist_method = sits_spread_time_series(),
                        tr_method = sits_mlr(formula = sits_formula_linear()),
                        multicores = 5)
mlr.kfold %>% sits_accuracy(conv2.lst)
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

lasso.kfold <-
    positive_prodes_all.tb %>%
    sits_kfold_validate(folds = 5,
                        pt_method = function(...) NULL,
                        dist_method = sits_spread_time_series(),
                        tr_method = sits_glm(alpha = 1),
                        multicores = 5)
lasso.kfold %>% sits_accuracy(conv2.lst)
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
ridge.kfold <-
    positive_prodes_all.tb %>%
    sits_kfold_validate(folds = 5,
                        pt_method = function(...) NULL,
                        dist_method = sits_spread_time_series(),
                        tr_method = sits_glm(alpha = 0),
                        multicores = 5)
ridge.kfold %>% sits_accuracy(conv2.lst)
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
rfor.kfold <-
    positive_prodes_all.tb %>%
    sits_kfold_validate(folds = 5,
                        pt_method = function(...) NULL,
                        dist_method = sits_spread_time_series(),
                        tr_method = sits_rfor(),
                        multicores = 5)
rfor.kfold %>% sits_accuracy(conv2.lst)
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


