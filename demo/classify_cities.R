# install the SITS library (if not already done)
# devtools::install_github("e-sensing/sits")

#load the SITS library
library(sits)
library(rgdal)
library(raster)
library(keras)

message("Classification of time series for cities in the Brazilian Cerrado")

##
## Step 1. Select one municipality in the Brazilian Cerrado to process
##

cities <- vector()
evi <- vector()
ndvi <- vector()
blue <- vector()
red <- vector()
nir <- vector()
mir <- vector()

# these are the links to access data via GDAL, using vsicurl and symbolic links for the files at dropbox

## Data for Alvorada do Norte

cities["AlvoradaDoNorte_GO"] <- "ADN"
evi["ADN"]  <- paste0("/vsicurl/https://www.dropbox.com/s/q2iuzwr24l5nkjo/AlvoradaDoNorte_GO_EVI.tif?raw=1")
mir["ADN"]  <- paste0("/vsicurl/https://www.dropbox.com/s/kt4pmhe7jpwqt1b/AlvoradaDoNorte_GO_MIR_reflectance.tif?raw=1")
ndvi["ADN"] <- paste0("/vsicurl/https://www.dropbox.com/s/lei3ff1e09v7mvp/AlvoradaDoNorte_GO_NDVI.tif?raw=1")
nir["ADN"]  <- paste0("/vsicurl/https://www.dropbox.com/s/ib2vu1rzq24vf1f/AlvoradaDoNorte_GO_NIR_reflectance.tif?raw=1")
blue["ADN"] <- paste0("/vsicurl/https://www.dropbox.com/s/gjcztnrf4i6c20d/AlvoradaDoNorte_GO_blue_reflectance.tif?raw=1")
red["AND"]  <- paste0("/vsicurl/https://www.dropbox.com/s/4azj3u1ev6yjc97/AlvoradaDoNorte_GO_red_reflectance.tif?raw=1")


## Data for Feira Nova do Maranhao

cities["FeiraNovaMaranhao-MA"] <- "FNM"

blue["FNM"] <- paste0("/vsicurl/https://www.dropbox.com/s/c0ybp825k32uhc6/FeiraNovaMaranhao_MA_blue_reflectance.tif?raw=1")
evi["FNM"]  <- paste0("/vsicurl/https://www.dropbox.com/s/jrqnnau0gfdxkqz/FeiraNovaMaranhao_MA_EVI.tif?raw=1")
mir["FNM"]  <- paste0("/vsicurl/https://www.dropbox.com/s/dd3p45ijuho8eth/FeiraNovaMaranhao_MA_MIR_reflectance.tif?raw=1")
ndvi["FNM"] <- paste0("/vsicurl/https://www.dropbox.com/s/8j2f54z8cl65f12/FeiraNovaMaranhao_MA_NDVI.tif?raw=1")
nir["FNM"]  <- paste0("/vsicurl/https://www.dropbox.com/s/90dl6ut0m96k0qa/FeiraNovaMaranhao_MA_NIR_reflectance.tif?raw=1")
red["FNM"]  <- paste0("/vsicurl/https://www.dropbox.com/s/b7l64voya2ph38t/FeiraNovaMaranhao_MA_red_reflectance.tif?raw=1")

# Data for Iaciara
cities["Iaciara-GO"] <- "IAC"

blue["IAC"] <- paste0("/vsicurl/https://www.dropbox.com/s/uojt2yb0q98rp3s/Iaciara_GO_blue_reflectance.tif?raw=1")
evi["IAC"]  <- paste0("/vsicurl/https://www.dropbox.com/s/o54dos3tbgheprr/Iaciara_GO_EVI.tif?raw=1")
mir["IAC"]  <- paste0("/vsicurl/https://www.dropbox.com/s/dr03wg011jx0aiw/Iaciara_GO_MIR_reflectance.tif?raw=1")
ndvi["IAC"] <- paste0("/vsicurl/https://www.dropbox.com/s/d4ouexjpey0jkfi/Iaciara_GO_NDVI.tif?raw=1")
nir["IAC"]  <- paste0("/vsicurl/https://www.dropbox.com/s/9yr8xork60zjqkv/Iaciara_GO_NIR_reflectance.tif?raw=1")
red["IAC"]  <- paste0("/vsicurl/https://www.dropbox.com/s/hvw3dq1mnndxp55/Iaciara_GO_red_reflectance.tif?raw=1")

# Data for Luciara
cities["Luciara-GO"] <- "LUC"

blue["LUC"] <- paste0("/vsicurl/https://www.dropbox.com/s/1sy0fuat9p6xepr/Luciara_MT_blue_reflectance.tif?raw=1")
evi["LUC"]  <- paste0("/vsicurl/https://www.dropbox.com/s/t0ylq3thbugnvqg/Luciara_MT_EVI.tif?raw=1")
mir["LUC"]  <- paste0("/vsicurl/https://www.dropbox.com/s/d4bqrq64bbyjs6l/Luciara_MT_MIR_reflectance.tif?raw=1")
ndvi["LUC"] <- paste0("/vsicurl/https://www.dropbox.com/s/nh66el51gp3a5s2/Luciara_MT_NDVI.tif?raw=1")
nir["LUC"]  <- paste0("/vsicurl/https://www.dropbox.com/s/17prqs17wwccp5q/Luciara_MT_NIR_reflectance.tif?raw=1")
red["LUC"]  <- paste0("/vsicurl/https://www.dropbox.com/s/aav63bmfquem10g/Luciara_MT_red_reflectance.tif?raw=1")

## Data for Luiz Eduardo Magalhaes
cities["LuizEduardoMagalhaes_BA"] <- "LEM"

blue["LEM"] <- paste0("/vsicurl/https://www.dropbox.com/s/vxq2pcfg25qs9bt/LuisEduardoMagalhaes_BA_blue_reflectance.tif?raw=1")
evi["LEM"]  <- paste0("/vsicurl/","https://www.dropbox.com/s/o56a4bpkti1an09/LuisEduardoMagalhaes_BA_EVI.tif?raw=1")
mir["LEM"]  <- paste0("/vsicurl/","https://www.dropbox.com/s/1o3pihti03m5g9j/LuisEduardoMagalhaes_BA_MIR_reflectance.tif?raw=1")
ndvi["LEM"] <- paste0("/vsicurl/","https://www.dropbox.com/s/albixsodrvln0jv/LuisEduardoMagalhaes_BA_NDVI.tif?raw=1")
nir["LEM"]  <- paste0("/vsicurl/","https://www.dropbox.com/s/wsmnje5ueazou7f/LuisEduardoMagalhaes_BA_NIR_reflectance.tif?raw=1")
red["LEM"]  <- paste0("/vsicurl/https://www.dropbox.com/s/w8o9ij0olarwm6w/LuisEduardoMagalhaes_BA_red_reflectance.tif?raw=1")

## Data for Planalto da Serra

cities["PlanaltoDaSerra_GO"] <- "PDS"

blue["PDS"] <- paste0("/vsicurl/https://www.dropbox.com/s/71jlsci6qso26t4/PlanaltoDaSerra_MT_blue_reflectance.tif?raw=1")
evi["PDS"]  <- paste0("/vsicurl/https://www.dropbox.com/s/5dzv3d3n98sv82v/PlanaltoDaSerra_MT_EVI.tif?raw=1")
mir["PDS"]  <- paste0("/vsicurl/https://www.dropbox.com/s/nfpwbw0w35564ly/PlanaltoDaSerra_MT_MIR_reflectance.tif?raw=1")
ndvi["PDS"] <- paste0("/vsicurl/https://www.dropbox.com/s/qef1pjkxlk4d3ru/PlanaltoDaSerra_MT_NDVI.tif?raw=1")
nir["PDS"]  <- paste0("/vsicurl/https://www.dropbox.com/s/qwqswzwg3d5nnp8/PlanaltoDaSerra_MT_NIR_reflectance.tif?raw=1")
red["PDS"]  <- paste0("/vsicurl/https://www.dropbox.com/s/qi19g84x29so9ks/PlanaltoDaSerra_MT_red_reflectance.tif?raw=1")

# Data for Ribeiraozinho
cities["Ribeiraozinho_MT"] <- "RIB"

blue["RIB"] <- paste0("/vsicurl/https://www.dropbox.com/s/g67rll87wjdi9iz/Ribeiraozinho_MT_blue_reflectance.tif?raw=1")
evi["RIB"]  <- paste0("/vsicurl/https://www.dropbox.com/s/fs7hqvk3oa3qqq1/Ribeiraozinho_MT_EVI.tif?raw=1")
mir["RIB"]  <- paste0("/vsicurl/https://www.dropbox.com/s/0kk5u8o28h3jps8/Ribeiraozinho_MT_MIR_reflectance.tif?raw=1")
ndvi["RIB"] <- paste0("/vsicurl/https://www.dropbox.com/s/f921kbfd98uthmc/Ribeiraozinho_MT_NDVI.tif?raw=1")
nir["RIB"]  <- paste0("/vsicurl/https://www.dropbox.com/s/92wa4jzqg8qjgck/Ribeiraozinho_MT_NIR_reflectance.tif?raw=1")
red["RIB"]  <- paste0("/vsicurl/https://www.dropbox.com/s/gfxkuujo95cpq5m/Ribeiraozinho_MT_red_reflectance.tif?raw=1")

# Data for Sandolandia
cities["Sandolandia_TO"] <- "SAN"

blue["SAN"] <- paste0("/vsicurl/https://www.dropbox.com/s/bs2hbbkg3wvcgfq/Sandolandia_TO_blue_reflectance.tif?raw=1")
evi["SAN"]  <- paste0("/vsicurl/https://www.dropbox.com/s/ylfim7nytw0c3z1/Sandolandia_TO_EVI.tif?raw=1")
mir["SAN"]  <- paste0("/vsicurl/https://www.dropbox.com/s/rwzoz9l3x5j7o5h/Sandolandia_TO_MIR_reflectance.tif?raw=1")
ndvi["SAN"] <- paste0("/vsicurl/https://www.dropbox.com/s/gtevj9xjf8ugak9/Sandolandia_TO_NDVI.tif?raw=1")
nir["SAN"]  <- paste0("/vsicurl/https://www.dropbox.com/s/59f46tj7u5wvg2f/Sandolandia_TO_NIR_reflectance.tif?raw=1")
red["SAN"]  <- paste0("/vsicurl/https://www.dropbox.com/s/a9mq7d3v6kr1otc/Sandolandia_TO_red_reflectance.tif?raw=1")

# Data for Sao Domingos

cities["SaoDomingos_GO"] <- "SDO"

blue["SDO"] <- paste0("/vsicurl/https://www.dropbox.com/s/0n77pqtobotfc5s/SaoDomingos_GO_blue_reflectance.tif?raw=1")
evi["SDO"] <- paste0("/vsicurl/","https://www.dropbox.com/s/vkvmgd93d2gqx1r/SaoDomingos_GO_EVI.tif?raw=1")
mir["SDO"]  <- paste0("/vsicurl/","https://www.dropbox.com/s/he3lka7l40fb1p0/SaoDomingos_GO_MIR_reflectance.tif?raw=1")
ndvi["SDO"] <- paste0("/vsicurl/","https://www.dropbox.com/s/6g3sfipdtavt36g/SaoDomingos_GO_NDVI.tif?raw=1")
nir["SDO"]  <- paste0("/vsicurl/","https://www.dropbox.com/s/79syujlqj8l2v84/SaoDomingos_GO_NIR_reflectance.tif?raw=1")
red["SDO"] <- paste0("/vsicurl/https://www.dropbox.com/s/volko844t35ttqb/SaoDomingos_GO_red_reflectance.tif?raw=1")

## Data for Sao Felix do Araguaia

cities["SaoFelixDoAraguaia_MT"] <- "SFA"


blue["SFA"] <- paste0("/vsicurl/https://www.dropbox.com/s/b9gxkk4fkroemzm/SaoFelixDoAraguaia_MT_blue_reflectance.tif?raw=1")
evi["SFA"]  <- paste0("/vsicurl/","https://www.dropbox.com/s/mtx4c024aspt1sv/SaoFelixDoAraguaia_MT_EVI.tif?raw=1")
mir["SFA"]  <- paste0("/vsicurl/","https://www.dropbox.com/s/5du4nxwatdvb6bt/SaoFelixDoAraguaia_MT_MIR_reflectance.tif?raw=1")
ndvi["SFA"] <- paste0("/vsicurl/","https://www.dropbox.com/s/z6dgfc48ezvfqv6/SaoFelixDoAraguaia_MT_NDVI.tif?raw=1")
nir["SFA"]  <- paste0("/vsicurl/","https://www.dropbox.com/s/dmvv33tqujoo39y/SaoFelixDoAraguaia_MT_NIR_reflectance.tif?raw=1")
red["SFA"]  <- paste0("/vsicurl/https://www.dropbox.com/s/dl5fz29qmxxqzbv/SaoFelixDoAraguaia_MT_red_reflectance.tif?raw=1")

## Data for Sidrolandia

cities["Sidrolandia_MS"] <- "SID"

blue["SID"] <- paste0("/vsicurl/https://www.dropbox.com/s/r7qpngz5lnejviw/Sidrolandia_MS_blue_reflectance.tif?raw=1")
evi["SID"] <- paste0("/vsicurl/https://www.dropbox.com/s/ybeh9d5x62hj8ga/Sidrolandia_MS_EVI.tif?raw=1")
mir["SID"] <- paste0("/vsicurl/https://www.dropbox.com/s/gq04jaiau9v4do3/Sidrolandia_MS_MIR_reflectance.tif?raw=1")
ndvi["SID"] <- paste0("/vsicurl/https://www.dropbox.com/s/h48ip132xkqsbnr/Sidrolandia_MS_NDVI.tif?raw=1")
nir["SID"] <- paste0("/vsicurl/https://www.dropbox.com/s/f97mnna2i0uhwkp/Sidrolandia_MS_NIR_reflectance.tif?raw=1")
red["SID"] <- paste0("/vsicurl/https://www.dropbox.com/s/0idg6rdw1gmj8u9/Sidrolandia_MS_red_reflectance.tif?raw=1")
## Data for Wanderley

cities["Wanderley_BA"] <- "WAN"
blue["WAN"] <- paste0("/vsicurl/https://www.dropbox.com/s/2lgcit0h3zux6v3/Wanderley_BA_blue_reflectance.tif?raw=1")
evi["WAN"]  <- paste0("/vsicurl/","https://www.dropbox.com/s/1zkyxzfwoztxzoq/Wanderley_BA_EVI.tif?raw=1")
mir["WAN"]  <- paste0("/vsicurl/","https://www.dropbox.com/s/dnm33l1up36ao71/Wanderley_BA_MIR_reflectance.tif?raw=1")
ndvi["WAN"] <- paste0("/vsicurl/","https://www.dropbox.com/s/hja8vkrvcs7p2rr/Wanderley_BA_NDVI.tif?raw=1")
nir["WAN"]  <- paste0("/vsicurl/","https://www.dropbox.com/s/yqt67p74od9eqx9/Wanderley_BA_NIR_reflectance.tif?raw=1")
red["WAN"]  <- paste0("/vsicurl/https://www.dropbox.com/s/y3jhyg1sets6ywq/Wanderley_BA_red_reflectance.tif?raw=1")

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

## Step 2. Select the bands to process

ndvi_file <- ndvi[code]
evi_file <-  evi[code]
nir_file <-  nir[code]
mir_file <-  mir[code]
red_file <-  red[code]
blue_file <- blue[code]

message("Step 2 - Please select the bands to process")

all_files <- c(ndvi_file, evi_file, blue_file, red_file, nir_file, mir_file)
all_bands <- c("ndvi", "evi", "blue", "red", "nir", "mir")

yes <- vector()

for (i in 1:length(all_bands)) {
    r <- c("")
    while (!(r == "y" || r == "n"))
        r <- readline(prompt= paste0("Include ", all_bands[i], " band (y/n)?: "))
    if (r == c("y"))
        yes[length(yes) + 1] <- TRUE
    else
        yes[length(yes) + 1] <- FALSE
}
bands <- all_bands[yes]
files <- all_files[yes]

## Step 3. Get the timeline (the same works for all data sets)

message("Step 3 - Get a valid timeline")

data("timeline_2000_2017")

## Step 4. Create a coverage with information about the input data
##         This is a raster metadata table

cat("\n")
message("Step 4 - Create a coverage with metadata")
cat("\n")
coverage_name <- paste0("MODIS-",name)
raster.tb <- sits_coverage(files = unname(files), name = coverage_name,
                           timeline = timeline_2000_2017, bands = bands)
message(paste0("Created coverage ",coverage_name))
cat("\n")


## Step 5. Select the ground samples to be used for processing
#

samples.tb <- sits_tibble()

samples_dbox <- paste0("https://www.dropbox.com/s/8vyi0snhetokfb9/samples_27022018.rda?raw=1")
download.file(samples_dbox, destfile = "./samples_Cerrado_27022018.rda")
load(file = "./samples_Cerrado_27022018.rda")
samples.tb <- sits_select_bands(samples_Cerrado_27022018.tb, bands)
name_samples <-  c("Cerrado-27022018")

cat("\n")
message(paste0("Selected samples - ", name_samples))
cat("\n")

## Step 6. Select a classification model
#


message("Step 6. Please select a classification method")
print(paste0("1. SVM - radial kernel"))
print(paste0("2. Random forest - 5000 trees"))
print(paste0("3. Deep learning - 3 hidden layers"))

n <- 0
while (!(n %in% c(1:3)))
    n <- as.integer(readline(prompt = "Enter an integer value (1-3): "))

if (n == 1) {
    method_name <- "svm"
    method <- sits_svm(kernel = "radial", degree = 3, coef0 = 0, cost = 10,
                      tolerance = 0.001, epsilon = 0.1)
    aval <- 3.0

} else if (n == 2) {
    method_name <- "rfor"
    method  <- sits_rfor(ntree = 5000)
    aval <- 0.0
} else if (n == 3) {
    method_name <- "dl-mlp"
    method <- sits_deeplearning(
        units            = c(512, 512, 512),
        activation       = 'elu',
        dropout_rates    = c(0.40, 0.40, 0.30),
        optimizer = keras::optimizer_adam(),
        epochs = 500,
        batch_size = 128,
        validation_split = 0.2)
    aval <- 0.0
} else {
    message("invalid value")
    stop()
}

cat("\n")
message(paste0("Selected method - ", method_name))
cat("\n")


## Step 8. Select memory size and number of cores
#
message("Please select memory size avaliable for processing in GB (tipical values 1 - 100)")
memsize <- as.integer(readline(prompt = "Enter a memsize value: "))

message("Please select number of cores (tipical values 1 - 32)")
multicores <- as.integer(readline(prompt = "Enter number of cores: "))

result_file <- paste0("./",code,"-class-",name_samples,"-",method_name)

sits_classify_raster(file = result_file, raster.tb, samples.tb,
                     ml_method = method,  adj_val = aval, memsize = memsize, multicores = multicores)
