devAskNewPage(ask = FALSE)


#  The data contain 1,892 time series samples for the Mato Grosso state in Brasil.
#  The time series come from MOD13Q1 collection 6 images, with 6 bands
#  ("blue", "red", "nir", "mir",  "evi",  and "ndvi")
#  The data set has the following classes:
#  Cerrado(379 samples), Fallow_Cotton (29 samples), Forest (131 samples),
#  Pasture (344 samples), Soy-Corn (364 samples),  Soy-Cotton (352 samples),
#  Soy_Fallow (87 samples), Soy_Millet (186 samples),
#  and Soy_Sunflower (26 samples).
#  The tibble has 7 variables:
#  (a) longitude: East-west coordinate of the time series sample (WGS 84);
#  (b) latitude (North-south coordinate of the time series sample in WGS 84),
#  (c) start_date (initial date of the time series),
#  (d) end_date (final date of the time series),
#  (e) label (the class label associated to the sample),
#  (f) cube (the name of the cube associated with the data),
#  (g) time_series (tibble with the values of the time series).



#load the sits library
library(sits)

# create a list to store the results
results <- list()


# Deep Learning - MLP
conf_dl.tb <- sits_kfold_validate(samples_mt_4bands, folds = 5, multicores = 1,
                                  ml_method = sits_deeplearning(
                                      layers = c(512,512,512),
                                      activation = "elu",
                                      dropout_rates = c(0.50, 0.40, 0.30),
                                      epochs = 300,
                                      batch_size = 128,
                                      validation_split = 0.2)
                                  )

print("== Confusion Matrix = DL =======================")
conf_dl.mx <- sits_conf_matrix(conf_dl.tb)

conf_dl.mx$name <- "mlp_default"

results[[length(results) + 1]] <- conf_dl.mx

# Deep Learning - FCN
conf_fcn853.tb <- sits_kfold_validate(samples_mt_4bands, folds = 5,
                        ml_method = sits_FCN(kernels = c(8, 5, 3),verbose = 1))

print("== Confusion Matrix = DL =======================")
conf_fcn853.mx <- sits_conf_matrix(conf_fcn853.tb)

conf_fcn853.mx$name <- "fcn_853"

results[[length(results) + 1]] <- conf_fcn853.mx

# Deep Learning - ResNet
conf_rn.tb <- sits_kfold_validate(samples_mt_4bands, folds = 5,
                                      ml_method = sits_ResNet(verbose = 0))

print("== Confusion Matrix = DL =======================")
conf_rn.mx <- sits_conf_matrix(conf_rn.tb)

conf_rn.mx$name <- "ResNet"

results[[length(results) + 1]] <- conf_rn.mx

# Deep Learning - TempCNN
conf_tc.tb <- sits_kfold_validate(samples_mt_4bands, folds = 5,
                                  ml_method = sits_TempCNN(verbose = 0))

print("== Confusion Matrix = DL =======================")
conf_tc.mx <- sits_conf_matrix(conf_tc.tb)

conf_tc.mx$name <- "TempCNN"

results[[length(results) + 1]] <- conf_tc.mx

WD = getwd()

# Deep Learning - LSTM
conf_lc.tb <- sits_kfold_validate(samples_mt_4bands, folds = 5, multicores = 32,
                                  ml_method = sits_LSTM_FCN(verbose = 0))

print("== Confusion Matrix = DL =======================")
conf_lc.mx <- sits_conf_matrix(conf_lc.tb)

conf_lc.mx$name <- "LSTM_FCN"

results[[length(results) + 1]] <- conf_lc.mx

WD = getwd()

sits_to_xlsx(results, file = paste0(WD, "/accuracy_mato_grosso.xlsx"))
