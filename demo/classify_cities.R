# install the SITS library (if not already done)
# devtools::install_github("e-sensing/sits")

#load the SITS library
library(sits)
library(rgdal)
library(raster)

message("Classification of time series for cities in the Brazilian Cerrado")

##
## Step 1. Select one municipality in the Brazilian Cerrado to process
##

cities <- vector()
evi <- vector()
ndvi <- vector()
nir <- vector()
mir <- vector()

## Data for Sao Felix do Araguaia

cities["SaoFelixDoAraguaia_MT"] <- "SFA"

# these are the links to access data via GDAL, using vsicurl and symbolic links for the files at dropbox
evi["SFA"]  <- paste0("/vsicurl/","https://www.dropbox.com/s/mtx4c024aspt1sv/SaoFelixDoAraguaia_MT_EVI.tif?raw=1")
mir["SFA"]  <- paste0("/vsicurl/","https://www.dropbox.com/s/5du4nxwatdvb6bt/SaoFelixDoAraguaia_MT_MIR_reflectance.tif?raw=1")
ndvi["SFA"] <- paste0("/vsicurl/","https://www.dropbox.com/s/z6dgfc48ezvfqv6/SaoFelixDoAraguaia_MT_NDVI.tif?raw=1")
nir["SFA"]  <- paste0("/vsicurl/","https://www.dropbox.com/s/dmvv33tqujoo39y/SaoFelixDoAraguaia_MT_NIR_reflectance.tif?raw=1")

## Data for Alvorada do Norte

cities["AlvoradaDoNorte_GO"] <- "ADN"
evi["ADN"]  <- paste0("/vsicurl/https://www.dropbox.com/s/q2iuzwr24l5nkjo/AlvoradaDoNorte_GO_EVI.tif?raw=1")
mir["ADN"]  <- paste0("/vsicurl/https://www.dropbox.com/s/kt4pmhe7jpwqt1b/AlvoradaDoNorte_GO_MIR_reflectance.tif?raw=1")
ndvi["ADN"] <- paste0("/vsicurl/https://www.dropbox.com/s/lei3ff1e09v7mvp/AlvoradaDoNorte_GO_NDVI.tif?raw=1")
nir["ADN"]  <- paste0("/vsicurl/https://www.dropbox.com/s/ib2vu1rzq24vf1f/AlvoradaDoNorte_GO_NIR_reflectance.tif?raw=1")

## Data for Wanderley

cities["Wanderley_BA"] <- "WAN"
evi["WAN"]  <- paste0("/vsicurl/","https://www.dropbox.com/s/1zkyxzfwoztxzoq/Wanderley_BA_EVI.tif?raw=1")
mir["WAN"]  <- paste0("/vsicurl/","https://www.dropbox.com/s/dnm33l1up36ao71/Wanderley_BA_MIR_reflectance.tif?raw=1")
ndvi["WAN"] <- paste0("/vsicurl/","https://www.dropbox.com/s/hja8vkrvcs7p2rr/Wanderley_BA_NDVI.tif?raw=1")
nir["WAN"]  <- paste0("/vsicurl/","https://www.dropbox.com/s/yqt67p74od9eqx9/Wanderley_BA_NIR_reflectance.tif?raw=1")

## Data for Luiz Eduardo Magalhaes
cities["LuizEduardoMagalhaes_BA"] <- "LEM"

evi["LEM"]  <- paste0("/vsicurl/","https://www.dropbox.com/s/o56a4bpkti1an09/LuisEduardoMagalhaes_BA_EVI.tif?raw=1")
mir["LEM"]  <- paste0("/vsicurl/","https://www.dropbox.com/s/1o3pihti03m5g9j/LuisEduardoMagalhaes_BA_MIR_reflectance.tif?raw=1")
ndvi["LEM"] <- paste0("/vsicurl/","https://www.dropbox.com/s/albixsodrvln0jv/LuisEduardoMagalhaes_BA_NDVI.tif?raw=1")
nir["LEM"]  <- paste0("/vsicurl/","https://www.dropbox.com/s/wsmnje5ueazou7f/LuisEduardoMagalhaes_BA_NIR_reflectance.tif?raw=1")

# Data for Sao Domingos

cities["SaoDomingos_GO"] <- "SDO"
evi["SDO"] <- paste0("/vsicurl/","https://www.dropbox.com/s/vkvmgd93d2gqx1r/SaoDomingos_GO_EVI.tif?raw=1")
mir["SDO"]  <- paste0("/vsicurl/","https://www.dropbox.com/s/he3lka7l40fb1p0/SaoDomingos_GO_MIR_reflectance.tif?raw=1")
ndvi["SDO"] <- paste0("/vsicurl/","https://www.dropbox.com/s/6g3sfipdtavt36g/SaoDomingos_GO_NDVI.tif?raw=1")
nir["SDO"]  <- paste0("/vsicurl/","https://www.dropbox.com/s/79syujlqj8l2v84/SaoDomingos_GO_NIR_reflectance.tif?raw=1")

# Placeholders for more data
evi_XXX  <- paste0("/vsicurl/","")
mir_XXX  <- paste0("/vsicurl/","")
ndvi_XXX <- paste0("/vsicurl/","")
nir_XXX  <- paste0("/vsicurl/","")

message("Step 1 - Please select a city to process")

for(i in 1:length(cities)) {
    print(paste0(i, ". ", names(cities[i])))
}
n <- as.integer(readline(prompt="Enter an integer value: "))

name <- names(cities[n])
code <- cities[n]

cat("\n")
message(paste0("Selected city - ", name," code - ", code))
cat("\n")

ndvi_file <- ndvi[code]
evi_file <-  evi[code]
nir_file <-  nir[code]
mir_file <-  mir[code]

files <- c(ndvi_file, evi_file, nir_file, mir_file)

## Step 2. Get the timeline (the same works for all data sets)

message("Step 2 - Download a valid timeline")

timeline_dropbox <- paste0("https://www.dropbox.com/s/j1jbx1rozettwmo/SaoFelixDoAraguaia_MT_timeline.csv?raw=1")
download.file(timeline_dropbox, destfile = "./timeline.csv")
timeline.csv <- read.csv("./timeline.csv", header = FALSE)
timeline <- lubridate::as_date(timeline.csv$V1)

## Step 3. Create a coverage with information about the input data
##         Thi is a raster metadata table

cat("\n")
message("Step 3 - Create a coverage with metadta")
cat("\n")
coverage_name <- paste0("MODIS-",name)
raster.tb <- sits_coverage(files = unname(files), name = coverage_name,
                           timeline = timeline, bands = c("ndvi", "evi", "nir", "mir"))
message(paste0("Created coverage ",coverage_name))
cat("\n")


## Step 4. Select the ground samples to be used for processing
#

message("Step 4. Select a ground sample data set")
print(paste0("1. Samples (2,115) for 9 classes"))
print(paste0("2. Samples (2,892) for 11 classes"))
print(paste0("3. Samples (11,743) for 13 classes"))

n <- as.integer(readline(prompt="Enter an integer value: "))

samples.tb <- sits_tibble()
name_samples <- character()

if (n == 1) {
    data(samples_MT_9classes)
    #select the bands for classification
    samples.tb <- sits_select(samples_MT_9classes, bands = c("ndvi", "evi", "nir", "mir"))
    name_samples <-  c("9classes")
} else if (n == 2) {
    samples_dbox <- paste0("https://www.dropbox.com/s/voisuoc1fwqtaaw/samples_cerrado_11classes.rda?raw=1")
    download.file(samples_dbox, destfile = "./cerrado_11classes.rda")
    load(file = "./cerrado_11classes.rda")
    samples.tb <- sits_select(samples_Cerrado_11classes, bands = c("ndvi", "evi", "nir", "mir"))
    name_samples <-  c("11classes")
} else if (n == 3){
    samples_dbox <- paste0("https://www.dropbox.com/s/addv5lxbpjm85jr/cerrado_13classes_modis_col6.rda?raw=1")
    #samples <- paste0("https://www.dropbox.com/s/p1fhzokfwdoum2m/cerrado_13_classes_col6_adj.rda?raw=1")

    download.file(samples_dbox, destfile = "./cerrado_13classes_col6_adj.rda")
    load(file = "./cerrado_13classes_col6_adj.rda")
    samples.tb <- sits_select(samples.tb, bands = c("ndvi", "evi", "nir", "mir"))
    name_samples <-  c("13classes")
} else {
    message("Invalid option")
    stop()
}
cat("\n")
message(paste0("Selected samples - ", name_samples))
cat("\n")

## Step 5. Select a classification model
#


message("Step 5. Please select a classification model")
print(paste0("1. SVM - radial kernel"))
print(paste0("2. Random forest - 5000 trees"))
print(paste0("3. Deep learning - 3 hidden layers"))

n <- as.integer(readline(prompt="Enter an integer value: "))

if (n == 1) {
    model_name <- "svm"
    model <- sits_svm(kernel = "radial", degree = 3, coef0 = 0, cost = 10,
                      tolerance = 0.001, epsilon = 0.1)
} else if (n == 2) {
    model_name <- "rfor"
    model<- sits_rfor(ntree = 5000)
} else if (n == 3) {
    model_name <- "dl-mlp"
    model <- sits_deeplearning(units = c(500, 400, 300, 200),
                               dropout_rates = c(0.45, 0.4, 0.35, 0.25),
                               epochs = 500)
} else {
    message("invalid value")
    stop()
}

cat("\n")
message(paste0("Selected method - ", model_name))
cat("\n")

## Step 6. Select an adjustment function
#
message("Please select an adjustment function")
print(paste0("1. Add  a fixed value"))
print(paste0("2. Normalize to (0,1)"))
print(paste0("3. Identity"))

n <- as.integer(readline(prompt="Enter an integer value: "))

if (n == 1) {
    afun <- sits_adjust()
    afun_name <- "adjust"
} else if (n == 2){
    afun <- function(x) {BBmisc::normalize(x, method = "range")}
    afun_name <- "normalize"
} else if (n == 3) {
    afun <- function(x) {identity(x)}
    afun_name <- "identity"
} else {
    message("Invalid function")
    stop()
}

cat("\n")
message(paste0("Selected adjustment - ", afun_name))
cat("\n")

## Step 7. Select a blocksize and number of cores
#
message("Please select a blocksize (tipical values 10000 - 1000000)")
blocksize <- as.integer(readline(prompt="Enter a blocksize value: "))

message("Please select number of cores (tipical values 1 - 32)")
multicores <- as.integer(readline(prompt="Enter number of cores: "))

result_file <- paste0("./",code,"-class-",name_samples,"-",model_name,"-",afun_name)

sits_classify_raster(file = result_file, raster.tb, samples.tb,
                     ml_method = model, adj_fun = sits_adjust(),
                     blocksize = blocksize, multicores = multicores)
