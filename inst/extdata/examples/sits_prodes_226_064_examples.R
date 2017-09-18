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
positive_prodes_all.tb <- prodes_all.tb %>% sits_apply(function(band) (3 + band))

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
    dplyr::mutate(reference = label) %>% sits_accuracy(conv.lst)

sits_predict(positive_prodes.tb, tssp_prodes.tb, tssp.svm) %>%
    dplyr::mutate(reference = label) %>% sits_accuracy(conv2.lst)

#    _     ___    _
#   | |   |   \  /_\
#   | |__ | |) |/ _ \
#   |____||___//_/ \_\
#
tssp.lda <- sits_train(tssp_prodes_train.tb, sits_lda(formula = sits_formula_linear()))

sits_predict(positive_prodes.tb, tssp_prodes.tb, tssp.lda) %>%
    dplyr::mutate(reference = label) %>% sits_accuracy(conv.lst)

sits_predict(positive_prodes.tb, tssp_prodes.tb, tssp.lda) %>%
    dplyr::mutate(reference = label) %>% sits_accuracy(conv2.lst)

#     ___   ___    _
#    / _ \ |   \  /_\
#   | (_) || |) |/ _ \
#    \__\_\|___//_/ \_\
#
tssp.qda <- sits_train(tssp_prodes_train.tb, sits_qda(formula = sits_formula_linear()))

sits_predict(positive_prodes.tb, tssp_prodes.tb, tssp.qda) %>%
    dplyr::mutate(reference = label) %>% sits_accuracy(conv.lst)

sits_predict(positive_prodes.tb, tssp_prodes.tb, tssp.qda) %>%
    dplyr::mutate(reference = label) %>% sits_accuracy(conv2.lst)

#    __  __  _     ___
#   |  \/  || |   | _ \
#   | |\/| || |__ |   /
#   |_|  |_||____||_|_\
#
tssp.mlr <- sits_train(tssp_prodes_train.tb, sits_mlr(formula = sits_formula_linear()))

sits_predict(positive_prodes.tb, tssp_prodes.tb, tssp.mlr) %>%
    dplyr::mutate(reference = label) %>% sits_accuracy(conv.lst)

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
    dplyr::mutate(reference = label) %>% sits_accuracy(conv.lst)

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
    dplyr::mutate(reference = label) %>% sits_accuracy(conv.lst)

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
svm.kfold %>% sits_accuracy(conv.lst)

svm.kfold %>% sits_accuracy(conv2.lst)
# Confusion Matrix and Statistics
#
#             Reference
# Prediction   Forest Non_Forest
#   Forest        117         12
#   Non_Forest     11        477
#
#              Accuracy : 0.9627
#                95% CI : (0.9446, 0.9762)
#
#                 Kappa : 0.887
#
#      Prod Acc  Forest : 0.9141
#  Prod Acc  Non_Forest : 0.9755
#      User Acc  Forest : 0.9070
#  User Acc  Non_Forest : 0.9775


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
lda.kfold %>% sits_accuracy(conv.lst)

lda.kfold %>% sits_accuracy(conv2.lst)
# Confusion Matrix and Statistics
#
#             Reference
# Prediction   Forest Non_Forest
#   Forest        117         12
#   Non_Forest     11        477
#
#              Accuracy : 0.9627
#                95% CI : (0.9446, 0.9762)
#
#                 Kappa : 0.887
#
#      Prod Acc  Forest : 0.9141
#  Prod Acc  Non_Forest : 0.9755
#      User Acc  Forest : 0.9070
#  User Acc  Non_Forest : 0.9775

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

qda.kfold %>% sits_accuracy(conv.lst)

qda.kfold %>% sits_accuracy(conv2.lst)
# Confusion Matrix and Statistics
#
#             Reference
# Prediction   Forest Non_Forest
#   Forest         42          1
#   Non_Forest     86        488
#
#              Accuracy : 0.859
#                95% CI : (0.829, 0.8855)
#
#                 Kappa : 0.432
#
#      Prod Acc  Forest : 0.3281
#  Prod Acc  Non_Forest : 0.9980
#      User Acc  Forest : 0.9767
#  User Acc  Non_Forest : 0.8502

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

mlr.kfold %>% sits_accuracy(conv.lst)

mlr.kfold %>% sits_accuracy(conv2.lst)
# Confusion Matrix and Statistics
#
#             Reference
# Prediction   Forest Non_Forest
#   Forest        108         22
#   Non_Forest     20        467
#
#              Accuracy : 0.9319
#                95% CI : (0.9091, 0.9505)
#
#                 Kappa : 0.7942
#
#      Prod Acc  Forest : 0.8438
#  Prod Acc  Non_Forest : 0.9550
#      User Acc  Forest : 0.8308
#  User Acc  Non_Forest : 0.9589

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

lasso.kfold %>% sits_accuracy(conv.lst)

lasso.kfold %>% sits_accuracy(conv2.lst)
# Confusion Matrix and Statistics
#
#             Reference
# Prediction   Forest Non_Forest
#   Forest        117         15
#   Non_Forest     11        474
#
#              Accuracy : 0.9579
#                95% CI : (0.9389, 0.9723)
#
#                 Kappa : 0.8733
#
#      Prod Acc  Forest : 0.9141
#  Prod Acc  Non_Forest : 0.9693
#      User Acc  Forest : 0.8864
#  User Acc  Non_Forest : 0.9773

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

ridge.kfold %>% sits_accuracy(conv.lst)

ridge.kfold %>% sits_accuracy(conv2.lst)
# Confusion Matrix and Statistics
#
#             Reference
# Prediction   Forest Non_Forest
#   Forest        119         13
#   Non_Forest      9        476
#
#              Accuracy : 0.9643
#                95% CI : (0.9465, 0.9775)
#
#                 Kappa : 0.8928
#
#      Prod Acc  Forest : 0.9297
#  Prod Acc  Non_Forest : 0.9734
#      User Acc  Forest : 0.9015
#  User Acc  Non_Forest : 0.9814

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

rfor.kfold %>% sits_accuracy(conv.lst)

rfor.kfold %>% sits_accuracy(conv2.lst)
# Confusion Matrix and Statistics
#
#             Reference
# Prediction   Forest Non_Forest
#   Forest        117          8
#   Non_Forest     11        481
#
#              Accuracy : 0.9692
#                95% CI : (0.9523, 0.9814)
#
#                 Kappa : 0.9055
#
#      Prod Acc  Forest : 0.9141
#  Prod Acc  Non_Forest : 0.9836
#      User Acc  Forest : 0.9360
#  User Acc  Non_Forest : 0.9776


# ===========================================================

# result              K-FOLD PATT.DIST.            K-FOLD intraCLUST               K-FOLD CLUSTER               K-FOLD TS.SPRD
#  - SVM:     0.8960 (0.8690, 0.9192)      0.9190 (0.8946, 0.9393)      0.8833 (0.8553, 0.9076)      0.9627 (0.9446, 0.9762)
#  - LDA:                     -------                      -------                      -------      0.9627 (0.9446, 0.9762)
#  - QDA:     0.9026 (0.8762, 0.9251) *                    -------                      -------      0.8590 (0.8290, 0.8855)
#  - MLR:                     -------                      -------                      -------      0.9319 (0.9091, 0.9505)
#  - LASSO:                   -------      0.9206 (0.8964, 0.9407) *                    -------      0.9579 (0.9389, 0.9723)
#  - RIDGE:                   -------      0.9092 (0.8838, 0.9307)      0.9076 (0.8820, 0.9293) *    0.9643 (0.9465, 0.9775)
#  - RFOR:                    -------                      -------      0.8849 (0.8571, 0.9090)      0.9692 (0.9523, 0.9814) *

#
#
#  ###################################################################
#  ###################################################################
#  ###################################################################


